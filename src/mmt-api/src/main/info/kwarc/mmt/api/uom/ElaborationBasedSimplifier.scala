package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import frontend._
import info.kwarc.mmt.api.checking.{Checker, CheckingEnvironment, MMTStructureChecker, RelationHandler}
import modules._
import symbols._
import patterns._
import objects._
import utils.MyList.fromList

import collection.immutable.{HashMap, HashSet}

/** used by [[MMTStructureSimplifier]] */
case class ByStructureSimplifier(home: Term, view: Term) extends Origin

/**
 * if set, the element has been elaborated
 */
object ElaboratedElement extends ClientProperty[StructuralElement,Option[Boolean]](utils.mmt.baseURI / "clientProperties" / "controller" / "elaborated") {
  def is(t : StructuralElement) : Boolean = get(t).getOrElse(None).isDefined
  def isProperly(t : StructuralElement) : Boolean = get(t).contains(Some(true))
  def set(t: StructuralElement) {
    put(t, Some(false))
  }
  def setProperly(t : StructuralElement) = put(t,Some(true))
}


/**
 * the primary class for all flattening, materialization, enriching of theories
 *
 * code in [[Closer]] should be merged into here
 */
class ElaborationBasedSimplifier(oS: uom.ObjectSimplifier) extends Simplifier(oS) with ChangeListener {
  private lazy val memory = controller.memory
  private lazy val lup = controller.globalLookup

  override def logPrefix = "structure-simplifier"

  def apply(s: StructuralElement) {
    log("simplifying " + s.path)
    s match {
     case m: DeclaredModule => flatten(m)
     case d: DefinedModule => // TODO materialize
     case d: Declaration => d.home match {
       case OMMOD(p) =>
         val mod = controller.globalLookup.getAs(classOf[DeclaredModule], p)
         flattenDeclaration(mod, d, None)
       case _ => // TODO materialize 
     }
     case _ =>
    }
  }

  def elaborateContext(outer: Context, con: Context) : Context = {
    var ret = Context.empty
    def currentContext = outer ++ ret
    con.foreach {vd =>
      val r = vd match {
        case dv @ DerivedVarDecl(name,feat,_,args) =>
          val sfOpt = controller.extman.get(classOf[StructuralFeature], feat)
          if (sfOpt.isDefined) {
            // sfOpt.get.checkInContext(currentContext,dv)
            vd :: sfOpt.get.elaborateInContext(currentContext,dv)
          } else List(vd)
        case v => List(v)
      }
      ret = ret ++ r
    }
    ret
  }

  /** flattens all declarations in a theory */
  private def flatten(m: DeclaredModule) {
     if (ElaboratedElement.is(m))
       return
     log("flattening " + m.path)
     try {
        val rules = RuleSet.collectRules(controller, m.getInnerContext)
        m match {
          case t: DeclaredTheory =>
            t.meta foreach apply
          case _ =>
        }
        m.getDeclarations.foreach {d => flattenDeclaration(m, d, Some(rules))}
     } finally {// if something goes wrong, don't try again
      ElaboratedElement.set(m)
     }
  }

  /** adds elaboration of d to parent */
  private def flattenDeclaration(mod: DeclaredModule, dOrig: Declaration, rulesOpt: Option[RuleSet] = None) {
    if (ElaboratedElement.is(dOrig))
      return
    lazy val rules = rulesOpt.getOrElse {
      RuleSet.collectRules(controller, mod.getInnerContext)
    }
    val parent = mod match {
      case t: DeclaredTheory => t
      case _ => return //TODO
    }
    lazy val alreadyIncluded = parent.getIncludes
    val dElab: List[Declaration] = dOrig match {
      // plain includes: copy (only) includes
      case Include(h, from, Nil) =>
        val dom = lup.getAs(classOf[DeclaredTheory], from)
        flatten(dom)
        dom.getDeclarations.flatMap {
          case Include(_, p, args) =>
            if (alreadyIncluded.contains(p)) {
               if (args != Nil) {
                 //TODO check for equality of arguments, if inequal raise error
               }
               Nil
            }
            else {
               List(Include(h, p, args))
            }
          case _ =>
            Nil
        }
      // any other import (including includes of complex theories): copy and translate all declarations
      case s: Structure =>
         val dom = materialize(Context(parent.path), s.from, true, None).asInstanceOf[DeclaredTheory]
         flatten(dom)
         dom.getDeclarations.flatMap(d => {
           val dF = lup.getAs(classOf[Declaration], parent.path ? s.name / d.name)
           d match {
             case PlainInclude(i,_) =>
               dF :: lup.get(i).getDeclarations.flatMap {
                 case PlainInclude(_,_) =>
                   Nil
                 case id: Declaration =>
                   val idF = lup.getAs(classOf[Declaration], parent.path ? (s.name / ComplexStep(i) / id.name))
                   List(idF)
               }
             case d => List(dF)
           }
         })
      // derived declarations: elaborate
      case dd: DerivedDeclaration =>
         controller.extman.get(classOf[StructuralFeature], dd.feature) match {
           case None => Nil
           case Some(sf) =>
             ElaboratedElement.set(dOrig)
              val elab = sf.elaborate(parent, dd)
              dd.module.setOrigin(GeneratedBy(dd.path))
              val simp = oS.toTranslator(rules)
             /*
             val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
               throw GeneralError(s"no mmt checker found")
             }.asInstanceOf[MMTStructureChecker]
             var cont = checker.elabContext(parent)(new CheckingEnvironment(new ErrorLogger(report), RelationHandler.ignore,new MMTTask{}))
              */
             val cont = elaborateContext(Context.empty,parent.getInnerContext)
              elab.getDeclarations.map {d =>
                //println(d)
                val dS = d.translate(simp,cont)
                dS
              }
         }
      case _ =>
        Nil
    }
    dElab.reverseMap {e =>
       e.setOrigin(ElaborationOf(dOrig.path))
       log("flattening yields " + e.path)
       controller.add(e, Some(dOrig.name))
    }
    if (dElab.isEmpty) ElaboratedElement.set(dOrig) else ElaboratedElement.setProperly(dOrig)
  }

  // TODO change management does not propagate to other theories yet

  /** deletes all declarations that were added by elaborating se */
  override def onDelete(se: StructuralElement) {
     if (! ElaboratedElement.is(se))
       return
     se match {
       case d: Declaration =>
         controller.get(d.home.toMPath) match {
           case thy: DeclaredTheory =>
             thy.getDeclarations.foreach {e =>
               if (e.getOrigin == ElaborationOf(d.path))
                 thy.delete(e.name)
             }
         }
       case _ =>
     }
  }
  /** reelaborates if old element was */
  override def onUpdate(old: StructuralElement, nw: StructuralElement) {
     if (! ElaboratedElement.is(old))
       return
     onDelete(old)
     (controller.getO(nw.parent), nw) match {
       case (Some(thy: DeclaredTheory), nw: Declaration) =>
         flattenDeclaration(thy, nw)
       case _ =>
     }
  }

  override def onAdd(c: StructuralElement) {
    // this makes sure there are no cycles, i.e., elaboration triggering itself
    // not sure if this is needed or even reasonable
    c.getOrigin match {
      case ElaborationOf(_) => return
      case _ =>
    }
    
    val parent = c.parent match {
      case cp : ContentPath => controller.get(cp)
      case _ => return
    }

    parent.getOrigin match {
      case GeneratedBy(dp : GlobalName) =>
        val ddOpt = controller.getO(dp)
        ddOpt match {
          case Some(dd : DerivedDeclaration) =>
            val thOpt = controller.getO(dd.parent)
            thOpt match {
              case Some(th : DeclaredTheory) =>
                onDelete(dd)
                ElaboratedElement.erase(dd)
                flattenDeclaration(th,dd)
              case _ =>
            }
          case _ =>
        }
      case _ =>
    }

    if (!ElaboratedElement.is(parent)) return
    (parent,c) match {
      case (t : DeclaredTheory, dec : Declaration) =>
        flattenDeclaration(t,dec)
      case (s : DeclaredStructure, dec : Declaration) =>
        controller.getO(s.parent) match {
          case Some(t : DeclaredTheory) =>
            onDelete(s)
            ElaboratedElement.erase(s)
            flattenDeclaration(t,s)
          case _ => {}
        }
      case (dd : DerivedDeclaration, dec : Declaration) =>
        controller.getO(dd.parent) match {
          case Some(t : DeclaredTheory) =>
            onDelete(dd)
            ElaboratedElement.erase(dd)
            flattenDeclaration(t,dd)
          case _ => {}
        }
      case _ => {}
    }
  }

  private var materializeCounter = 0
  private def newName: MPath = {
     val i = materializeCounter
     materializeCounter += 1
     utils.mmt.mmtbase ? LocalName("")/"E"/i.toString
  }
  /** Elaborate a theory expression into a module
    *
    *  @param exp the theory expression
   *  @param expandDefs materialize all DefinedTheory's and return a DeclaredTheory
   *  @param path the path to use if a new theory has to be created
   */
  def materialize(context: Context, exp: Term, expandDefs: Boolean, pathOpt: Option[MPath]): Theory = {
    exp match {
      case OMMOD(p: MPath) => lup.getTheory(p) match {
         case d: DefinedTheory if expandDefs => materialize(context, d.df, expandDefs, None)
         case d => d
      }
      /* case OMPMOD(p, args) =>
         val t = getTheory(p)
         new InstantiatedTheory(t, args) */
      case _ => // create a new theory and return it
        val path = pathOpt.getOrElse(newName)
        val ComplexTheory(cont) = exp
        val meta = TheoryExp.metas(exp, all = false)(lup).headOption
        val thy = new DeclaredTheory(path.parent, path.name, meta)
        cont.foreach { vd =>
          val d = vd.toDeclaration(exp)
          thy.add(d)
        }
        thy.setOrigin(Materialized(exp))
        thy
    }
  }
  
  //TODO move to library if it's not doing anything else
  def getBody(context: Context, moduleExp: Term): ElementContainer[NamedElement] = moduleExp match {
     case OMMOD(p) => lup.getTheory(p) match {
       case m: DefinedModule => getBody(context, m.df)
       case m: DeclaredModule => m
     }
     //TODO OMPMOD
     case ComplexTheory(cont) => cont
  }

   /* everything below here is Mihnea's enrichment code, which may be outdated or incomplete */

  /** auxiliary method of enriching */
  private lazy val loadAll = {
    memory.ontology.getInds(ontology.IsTheory) foreach {p =>
      controller.get(p)
    }
    memory.ontology.getInds(ontology.IsView) foreach {p =>
      controller.get(p)
    }
  }
  private lazy val modules = controller.memory.content.getModules

  /**
   * adds declarations induced by views to all theories
   */
  def enrich(t : DeclaredTheory) : DeclaredTheory =  {
    loadAll
    println("Flattening: " + t.path)
    val tbar = new DeclaredTheory(t.parent, t.name, t.meta)
    t.getDeclarations foreach {d =>
      tbar.add(d)
    }
    val views = modules collect {
      case v : DeclaredView if v.to == t.toTerm => v
    } // all views to T

    views foreach { v =>
      val s = v.from
      implicit val rules = makeRules(v)
      modules collect {
        case sprime : DeclaredTheory if memory.content.visible(sprime.toTerm).toSet.contains(s) =>
          // here we have v : s -> t and sprime includes s -- (include relation is transitive, reflexive)
          // therefore we make a structure with sprime^v and add it to tbar
          /*
          val str = SimpleDeclaredStructure(tbar.toTerm, (LocalName(v.path) / sprime.path.toPath), sprime.path, false)
          sprime.getDeclarations foreach {d =>
            str.add(rewrite(d))
          }
          tbar.add(str)
          */
          //conceptually this should be a structure, but adding the declarations directly is more efficient
          sprime.getDeclarations foreach {
            case c : Constant => tbar.add(rewrite(c, s.toMPath, tbar.path, t.getInnerContext))
            case _ => //nothing for now //TODO handle structures
          }
      }
    }
    println(t.path + ": " + t.getDeclarations.length + " ->  " + tbar.getDeclarations.length)
    tbar
  }
  //Flattens by generating a new theory for every view, used for flatsearch
  def enrichFineGrained(t : DeclaredTheory) : List[DeclaredTheory] = {
    loadAll
    var thys : List[DeclaredTheory] = Nil
    val tbar = new DeclaredTheory(t.parent, t.name, t.meta)
    t.getDeclarations foreach {d =>
      tbar.add(d)
    }
    thys ::= tbar
    val views = modules collect {
      case v : DeclaredView if v.to == t.toTerm => v
    }
    views foreach { v=>
      val s = v.from
      implicit val rules = makeRules(v)
      modules collect {
        case sprime : DeclaredTheory if memory.content.visible(sprime.toTerm).toSet.contains(s) =>
          val tvw = new DeclaredTheory(t.parent, sprime.name / v.name, t.meta)
          sprime.getDeclarations foreach {
            case c : Constant => tvw.add(rewrite(c, v.path, tbar.path, t.getInnerContext))
            case _ => //nothing for now //TODO handle structures
          }
          thys ::= tvw
      }

    }

    thys
  }


  private def makeRules(v : DeclaredView) : HashMap[Path, Term] = {
    val path = v.from.toMPath
    var rules = new HashMap[Path,Term]
    val decl = v.getDeclarations

    v.getDeclarations foreach {
      case c : Constant =>
        c.df.foreach {t =>
          rules += (path ? c.name -> t)
        }
      case d : DefinedStructure =>
        try {
          controller.get(d.df.toMPath) match {
            case d : DeclaredView => rules ++= makeRules(d)
            case x => //nothing to do
          }
        } catch {
          case e : Error => println(e)//nothing to do
          case e : Exception => println(e)//nothing to do
        }
    }
    rules
  }

  private def rewrite(d : Declaration, vpath : MPath, newhome : MPath, context : Context)(implicit rules : HashMap[Path, Term]) : Declaration = d match {
    case c : Constant =>
      val newtpC = TermContainer(c.tp.map(t => controller.simplifier.apply(rewrite(t), context)))
      val newdfC = TermContainer(c.df.map(rewrite))
      val newname = LocalName(vpath.toPath) / c.home.toMPath.toPath / c.name
      val newCons = new FinalConstant(OMMOD(newhome), newname, c.alias, newtpC, newdfC, c.rl, c.notC)
      newCons.setOrigin(ByStructureSimplifier(c.home, OMID(vpath)))
      newCons
    case x => x
  }


  private def rewrite(t : Term)(implicit rules : HashMap[Path, Term]) : Term = {
    t match {
    case OMID(p) =>
      if (rules.isDefinedAt(p)) rules(p) else t
    case OMA(f, args) => OMA(rewrite(f), args.map(rewrite))
    case OMBINDC(b, con, bodies) => OMBINDC(rewrite(b), rewrite(con), bodies.map(rewrite))
    case _ => t
  }}

  private def rewrite(con : Context)(implicit rules : HashMap[Path, Term]) : Context = {
    val vars = con.variables map {
      case VarDecl(n, tp, df, not) => VarDecl(n, tp.map(rewrite), df.map(rewrite), not)
    }
    Context(vars : _*)
  }
}
