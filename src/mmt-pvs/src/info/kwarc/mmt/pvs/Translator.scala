package info.kwarc.mmt.pvs

import info.kwarc.mmt.api.frontend.{Controller, Logger}
import info.kwarc.mmt.api.opaque.{OpaqueElement, OpaqueText, StringFragment}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.lf.{ApplySpine, Lambda, Pi, Typed}
import syntax._
import info.kwarc.mmt.api._
import documents._
import modules._
import parser._
import info.kwarc.mmt.api.objects._
import utils._
import archives._
import info.kwarc.mmt.LFX.Subtyping.subtypeJudg
import info.kwarc.mmt.api.checking.{Checker, CheckingEnvironment, MMTStructureChecker, RelationHandler}

class TheoryState(val parent : DPath, val name : LocalName, val meta : MPath) {
  val path : MPath = parent ? name
  var parameters : Context= Context.empty
  protected var decls : List[Declaration] = Nil
  def add(d : Declaration) = decls ::= d
  def getDeclarations = decls.reverse
  def toTerm = OMMOD(path)
  def declares(n : LocalName) : Boolean = decls.exists(d => d.name == n)
  object includes {
    var stored : List[(MPath,Boolean)] = Nil
    def contains(p : MPath) = stored.exists(a => a._1 == p)
    def ::=(p : MPath, par : Boolean = false) = stored ::= (p,par)
    def find(cond : MPath => Boolean) : Option[MPath] = stored.map(_._1).find(cond)
    def inPars(p : MPath) : Option[Boolean] = stored.find(q => q._1 ==p).map(_._2)
  }
}

abstract class ImportState(t:PVSImportTask) {
  val isPrelude : Boolean
  var inFormals = true

  val th : TheoryState

  var tcc : Option[FinalConstant] = None
  var vars : Context = Context.empty

  var inductive : Option[GlobalName] = None

  private var unknowns = 0
  def doUnknown = OMV("""/i/""" + {unknowns+=1;unknowns-1})
  private def bindUnknowns(t : Term) = {
    val symbs = t.freeVars.collect{
      case ln if ln.toString.startsWith("""/i/""") => ln
    }
    val cont = symbs.flatMap(n => {
      val i = (0 until unknowns).find(j => n == LocalName("""/i/""" + j))/*.getOrElse(
          throw new Exception("Wrong free Variable: " + n + " in " + t)
        )*/
      if (i.isDefined)
        List(VarDecl(LocalName("""/I/""" + i.get),Some(OMS(Typed.ktype)),None,None),VarDecl(n, Some(OMV("""/I/""" + i.get)), None, None))
      else throw GeneralError("No unknown " + n)
    })
    if (unknowns > 0 && cont.nonEmpty) OMBIND(OMS(Path.parseS("http://cds.omdoc.org/mmt?mmt?unknown", NamespaceMap.empty)),
      cont,
      t)
    else t
  }
  def reset = {
    unknowns = 0
    vars = Context.empty
  }
  def bind(t : Term) : Term = {
    val allvars = t.freeVars.collect{case v if vars.exists(_.name == v) && !th.parameters.exists(_.name == v)=> vars.find(_.name == v).get}
    bindUnknowns(if (allvars.nonEmpty) Pi(allvars.distinct.map{
      case VarDecl(name,Some(tp),df,not) => VarDecl(name,Some(PVSTheory.expr(tp)),df,not)
    },t) else t)
  }

  def addinclude(p : MPath) : Unit =
    if (inFormals && !th.includes.contains(p)) {
      /*
      val or = t.controller.getO(p).asInstanceOf[Option[DeclaredTheory]]
      if (or.isDefined) {
        or.get.getDerivedDeclarations(BoundInclude.feature).foreach(d =>
          addinclude(d.getComponent(DomComponent).get.asInstanceOf[AbstractTermContainer].get match {
            case Some(OMMOD(np)) => np
          }))
      }
      */
      th.parameters = th.parameters ++ DerivedVarDecl(LocalName(p),BoundInclude.feature,BoundInclude.mpath,List(OMMOD(p)))/* DerivedVarDecl(LocalName(p),BoundIncludeRule.feature,
        Some(OMMOD(p))) */
      t.deps::=p
      th.includes ::= (p,true)
    } else if (!th.includes.contains(p)) {
      /*
      val or = t.controller.getO(p).asInstanceOf[Option[DeclaredTheory]]
      if (or.isDefined) {
        or.get.getDerivedDeclarations(BoundInclude.feature).foreach(d =>
          d.getComponent(DomComponent).get.asInstanceOf[AbstractTermContainer].get match {
          case Some(OMMOD(np)) => addinclude(np)
          case _ => println("Weird: " + d.path)
              println("    - " + d.getComponent(DomComponent).get.asInstanceOf[AbstractTermContainer].get)
              readLine()
        })
      } */
      th add BoundInclude(th.path,p)
      /*
      try {
        t.controller.globalLookup(p).asInstanceOf[DeclaredTheory]
      } catch {
        case _ : Exception =>
          // if (p.toString contains "_adt") throw _adt(p) else
          throw Dependency(p)
      }
      */
      t.deps::=p
      th.includes ::= p
    }
}

case class Dependency(p : MPath) extends Exception
//case class _adt(p : MPath) extends Exception

class PVSImportTask(val controller: Controller, bt: BuildTask, index: Document => Unit) extends Logger with MMTTask {
  def logPrefix = "pvs-omdoc"
  protected def report = controller.report

  val path = bt.narrationDPath.^!.^!
  var state : ImportState = null
  var deps : List[MPath] = Nil

  def doDocument(d: pvs_file) : BuildResult = {
    // println("Document:" +bt.narrationDPath)
    try {
      val modsM = d._modules map doModule
       //.add(m) ; MRef(bt.narrationDPath, m.path)})
      try {
        deps.distinct foreach {
          controller.get(_).asInstanceOf[DeclaredTheory]
        }
      } catch {
        case e : Exception => throw Dependency(deps.head)
      }
      log("Translated: " + state.th.name)

      /*
      val ex = controller.getO(modsM.head.path) match {
        case Some(dt : DeclaredTheory) =>
          log("Already done")
          return BuildResult.empty
        case _ => None
      }
      */

      val doc = new Document(bt.narrationDPath, true)
      val ths = modsM map (m => {
        val theory = new DeclaredTheory(m.parent,m.name,Some(m.meta))//,m.parameters)
        if (m.parameters.nonEmpty) {
          log(m.parameters.toString)
          theory.paramC.set(m.parameters)
        }
        controller add theory
        m.getDeclarations foreach { controller.add(_) }
        //println(theory)
        controller simplifier theory
        doc add MRef(bt.narrationDPath, theory.path)
        theory
      })
      controller.add(doc)

      // println(controller.presenter.asString(ths.head))
      // println(ths.head)

      log("Checking:")
      logGroup {
        val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
          throw GeneralError(s"no checker $id found")
        }.asInstanceOf[MMTStructureChecker]
        ths foreach (p => checker.timeoutCheck(p,30000)(
          new CheckingEnvironment(new ErrorLogger(report), RelationHandler.ignore,this)))
      }
      index(doc)


      BuildSuccess(deps.map(LogicalDependency),modsM.map(m => LogicalDependency(m.path)))
    } catch {
      case Dependency(p) =>
        deps = (p::deps).distinct
        log("FAIL: " + state.th.name + " depends on " + deps)
        MissingDependency(deps.map(LogicalDependency),List(LogicalDependency(state.th.path)))
      case t : Exception =>
        log("Exception: " + t.getMessage)
        t.printStackTrace()
        sys.exit
    }
  }

  def doModule(m: syntax.Module): TheoryState = m match {
    case theory(named, theory_formals, assuming, exporting, decls) =>
      val isPrel = path.toString == "http://pvs.csl.sri.com/Prelude"
      val meta = if (isPrel) PVSTheory.thpath else PVSTheory.preludepath

      val theory = new TheoryState(path, doName(named.id), meta)
      state = new ImportState(this) {
        val isPrelude = isPrel
        val th = theory
      }
      deps ::= meta
      if (isPrel && named.id == "booleans") {
        log("Skipped Booleans")
        // theory add new OpaqueText(path,List(StringFragment(""))).
        return theory
      }
      if (isPrel && named.id == "equalities") {
        log("Skipped Equalities")
        return theory
      }

      log("Doing " + theory.path)

      theory_formals foreach doFormal
      assuming foreach doAssumption
      decls foreach (doDecl(_)(false))
      state.th

    case datatype(TopDatatypeBody(named, theory_formals, importings, constructors)) =>
      // println(" -- Datatype: " + named.id)
      val isPrel = path.toString == "http://pvs.csl.sri.com/Prelude"
      val meta = if (isPrel) PVSTheory.thpath else PVSTheory.preludepath
      val theory = new TheoryState(path, doName(named.id /* + "_adt" */ ), meta)
      state = new ImportState(this) {
        val isPrelude = isPrel
        val th = theory
      }
      deps::=meta

      log("Doing " + theory.path)

      importings foreach doFormal
      theory_formals foreach doFormal
      val datatp = Constant(state.th.toTerm, doName(named.id), Nil, Some(PVSTheory.tp.term), None, None)
      state.th add datatp

      constructors.foreach(doDatatypeConstructor(_,datatp)) // TODO subtype_id ?
      state.th

    case _ =>
      println(" -- OTHER: " + m.getClass)
      sys.exit
  }

  def doDatatypeConstructor(con:constructor,datatp:FinalConstant) = {
    val conname = newName(con.named.id)
    state.reset
    val accs = con.accessors.map(a => {
      val tp = doType(a._type)
      (newName(a.named.id), tp)})
    val contp = if (accs.isEmpty) datatp.toTerm
    else if (accs.length == 1) PVSTheory.fun_type(accs.head._2, datatp.toTerm)
    else PVSTheory.fun_type(PVSTheory.tuple_type(accs.map(_._2)), datatp.toTerm)
    val const = Constant(state.th.toTerm, conname, Nil, Some(state.bind(PVSTheory.expr(contp))), None, Some("Constructor"))
    // println(const)
    val reco = Constant(state.th.toTerm, newName(con.recognizer), Nil, Some(
      state.bind(PVSTheory.expr(PVSTheory.fun_type(datatp.toTerm, PVSTheory.bool.term)))), None, Some("Recognizer"))
    state.th add reco
    accs.foreach(ac =>
      state.th add Constant(state.th.toTerm, ac._1, Nil, Some(
        state.bind(PVSTheory.expr(PVSTheory.fun_type(PVSTheory.setsub(datatp.toTerm, reco.toTerm), ac._2)))
      ), None, Some("Accessor"))
    )
  }

  def doAssumption (ad:AssumingDecl) : Unit = ad match {
    case assumption(named,assertion) => doDecl(axiom_decl(named,assertion))(true)
    case d : Decl => doDecl(d)(true)
    case _ =>
      println("TODO Assumption: "+ad.getClass)
      sys.exit
  }


  def doFormal(f:FormalParameter) : Unit = {
    state.inFormals = true
    f match {
      case formal_type_decl(named,nonempty) =>
        state.reset
        val v = VarDecl(
          doName(named.named.id),
          Some(if (nonempty.nonempty_p) PVSTheory.nonempty.tps else PVSTheory.tp.term),
          None,
          None)
        state.th.parameters = state.th.parameters ++ v
        state.inFormals = false
        /*
        if (nonempty.nonempty_p && nonempty.contains.isDefined) {
          state.th.add(Constant(state.th.toTerm,newName("INTERNAL_Assumption"),Nil,Some(
            state.bind(PVSTheory.is_nonempty(OMV(v.name),doExprAs(nonempty.contains.get,OMV(v.name))))),
            None,Some("Assumption")))
        } else if (nonempty.nonempty_p) {
          state.th.add(Constant(state.th.toTerm,newName("INTERNAL_Assumption"),Nil,Some(
            PVSTheory.nonempty(OMV(v.name))),
            None,Some("Assumption")))
        }
        */
      case formal_subtype_decl(ChainedDecl(NamedDecl(id,_,_),_,_),nonempty,sup,pred) =>
        val name = newName(id)
        state.reset
        val actsup = doType(sup._declared)
        val v = VarDecl(name,Some(state.bind(PVSTheory.powertp(actsup))),None,None)
        state.th.parameters = state.th.parameters ++ v
        state.inFormals = false
        /*
        if (nonempty.nonempty_p && nonempty.contains.isDefined) {
          state.th.add(Constant(state.th.toTerm,newName("INTERNAL_Assumption"),Nil,Some(
            state.bind(PVSTheory.is_nonempty(OMV(v.name),doExprAs(nonempty.contains.get,OMV(v.name))))),
            None,Some("Assumption")))
        } else if (nonempty.nonempty_p) {
          state.th.add(Constant(state.th.toTerm,newName("INTERNAL_Assumption"),Nil,Some(
            PVSTheory.nonempty(OMV(v.name))),
            None,Some("Assumption")))
        }
        */
        doDecl(pred)(true)

      case formal_const_decl(ChainedDecl(NamedDecl(id,_,_),_,_),tp) =>
        state.reset
        val fulltp = doType(tp._internal)
        val v = VarDecl(doName(id),Some(state.bind(PVSTheory.expr(fulltp))),None,None)
        state.th.parameters = state.th.parameters ++ v
        state.inFormals = false
      // case d : Decl => doDecl(d)(true)
      case _ =>
        println("TODO Formal: " + f.getClass + ": " + f)
        sys.exit
    }
  }

  def doDecl(d: Decl)(isAss : Boolean = false) : Unit = {
    //    println(d)
    state.inFormals = false
    d match {
      case md : macro_decl => doDecl(md.decl)(isAss)

      case var_decl(id, unnamed, tp) => // Not needed

      case tcc_decl(ChainedDecl(NamedDecl(id, _, _), _, _), Assertion(kind, formula)) =>
        state.reset
        val name = newName(id)//if (id.dropRightUntil().endsWith("TCC")) newName(id) else newName(id + "_TCC")
        val c = Constant(state.th.toTerm, name, Nil, Some(state.bind(PVSTheory.proof(kind, doExprAs(formula, PVSTheory.bool.term)))), None,
          Some(if (isAss) "Assumption_TCC" else "TCC"))
        state.th add c
        state.tcc = Some(c)

      case const_decl(ChainedDecl(NamedDecl(id, _, _), _, _), arg_formals, tp, defOpt) =>
        state.reset
        val tptm = doType(tp._declared)
        val exptp = arg_formals.flatMap(_._bindings).foldRight(tptm)((b, t) =>
          PVSTheory.pvspi(doName(b.id), doType(b._type), t)
        )
        val defi = defOpt.map(d => arg_formals.flatMap(_._bindings).foldRight(doExprAs(d, tptm), tptm)((b, t) =>
          (PVSTheory.pvslambda(doName(b.id), doType(b._type), t._2, t._1),
            PVSTheory.pvspi(doName(b.id), doType(b._type), t._2)
            ))._1)
        state.th add Constant(state.th.toTerm, newName(id), Nil, Some(state.bind(PVSTheory.expr(exptp))), defi.map(state.bind),
          if (isAss) Some("Assumption") else None)

      case formula_decl(ChainedDecl(NamedDecl(id, _, _), _, _), Assertion(kind, form)) =>
        state.reset
        val phi = doExprAs(form, PVSTheory.bool.term)
        state.th add Constant(state.th.toTerm, newName(id), Nil, Some(state.bind(PVSTheory.proof(kind, phi))), None, if (isAss) Some("Assumption") else None)

      case conversion_decl(UnnamedDecl(_, _, _), kind, expr) => // TODO

      case auto_rewrite(UnnamedDecl(_, _, _), key, kind, namelist) => // TODO

      case def_decl(named, arg_formals, tp, df, optMeasure, optOrder) =>
        state.reset
        val tptm = doType(tp._internal)
        val name = newName(named.named.id)
        val exptp = arg_formals.flatMap(_._bindings).foldRight(tptm)((b, t) =>
          PVSTheory.pvspi(doName(b.id), doType(b._type), t)
        )
        state.inductive = Some(state.th.path ? name)
        //state.vars = state.vars++VarDecl(name,Some(exptp),None,None)
        val (defi, _) = arg_formals.flatMap(_._bindings).foldRight(doExprAs(df, tptm), tptm)((b, t) =>
          (PVSTheory.pvslambda(doName(b.id), doType(b._type), t._2, t._1),
            PVSTheory.pvspi(doName(b.id), doType(b._type), t._2)
            ))
        //state.vars = Context.empty
        state.inductive = None
        state.th add Constant(state.th.toTerm, name, Nil, Some(state.bind(PVSTheory.expr(exptp))),
          Some(state.bind(PVSTheory.recursor(exptp, name, defi))), if (isAss) Some("Assumption") else None)

      case ind_decl(named, arg_formals, tp, df) =>
        //doDecl(def_decl(ChainedDecl(named,false,false),arg_formals,tp,df,None,None))(isAss) // TODO?
        state.reset
        val tptm = doType(tp._internal)
        val name = newName(named.id)
        val exptp = arg_formals.flatMap(_._bindings).foldRight(tptm)((b, t) =>
          PVSTheory.pvspi(doName(b.id), doType(b._type), t)
        )
        state.inductive = Some(state.th.path ? name)
        //state.vars = state.vars++VarDecl(name,Some(exptp),None,None)
        val defi = PVSTheory.recursor(exptp, name, arg_formals.flatMap(_._bindings).foldRight(doExprAs(df, tptm), tptm)((b, t) =>
          (PVSTheory.pvslambda(doName(b.id), doType(b._type), t._2, t._1),
            PVSTheory.pvspi(doName(b.id), doType(b._type), t._2)
            ))._1)
        state.inductive = None
        //state.vars = Context.empty
        state.th add Constant(state.th.toTerm, name, Nil, Some(state.bind(PVSTheory.expr(exptp))),
          Some(state.bind(defi)), if (isAss) Some("Assumption") else None)

      case application_judgement(ond, nameexpr, arg_formals, tp) =>
        val name = newName(ond.id.getOrElse("App_Judgment"))
        state.reset
        val (exp, restp) = doExpr(arg_formals.flatMap(_._bindings).foldLeft(nameexpr.asInstanceOf[Expr])((e, b) =>
          application("", e, varname_expr("", b.id, b._type), false)))
        val fulltp = Pi(arg_formals.flatMap(_._bindings).map(b => VarDecl(doName(b.id),
          Some(PVSTheory.expr(doType(b._type))), None, None)),
          PVSTheory.tpjudg(exp, restp, doType(tp._internal)))
        state.th add Constant(state.th.toTerm, name, Nil, Some(state.bind(fulltp)), None,
          if (isAss) Some("Assumption") else None)

      case type_def_decl(NamedDecl(id, _, _), NonEmptiness(neb, exprOpt), arg_formals, df) =>
        state.reset
        val bindings = arg_formals.flatMap(_._bindings).map(b => (doName(b.id), doType(b._type)))
        val defi = bindings.foldRight[Term](doType(df._declared))((b, t) =>
          Lambda(b._1, PVSTheory.expr(b._2), t))
        val tp = bindings.foldRight[Term](PVSTheory.tp.term)((b, t) =>
          Pi(b._1, PVSTheory.expr(b._2), t))
        val name = newName(id)
        val c = Constant(
          state.th.toTerm,
          name,
          Nil,
          Some(if (neb) state.bind(PVSTheory.nonempty.tps) else state.bind(tp)), Some(state.bind(defi)),
          if (isAss) Some("Assumption") else None)
        state.th add c
        /*
        if (neb && exprOpt.isDefined) {
          state.th.add(Constant(state.th.toTerm, newName("INTERNAL_Judgment"), Nil, Some(
            state.bind(PVSTheory.is_nonempty(c.toTerm, doExprAs(exprOpt.get, defi)))),
            None, if (isAss) Some("Assumption") else None))
        } else if (neb) {
          state.th.add(Constant(state.th.toTerm, newName("INTERNAL_Judgment"), Nil, Some(
            PVSTheory.nonempty(c.toTerm)),
            None, if (isAss) Some("Assumption") else None))
        }
        */

      case axiom_decl(ChainedDecl(NamedDecl(id, _, _), _, _), Assertion(kind, form)) =>
        state.reset
        val phi = doExprAs(form, PVSTheory.bool.term)
        state.th add Constant(state.th.toTerm, newName(id), Nil, Some(state.bind(PVSTheory.proof(kind, phi))), None,
          if (isAss) Some("Assumption") else None)

      case subtype_judgement(OptNamedDecl(id, _, _, _), sub, sup) =>
        state.reset
        val subtp = doType(sub._internal)
        val suptp = doType(sup._internal)
        val c = Constant(state.th.toTerm, newName(id.getOrElse("INTERNAL_Judgment")), Nil, Some(
          state.bind(PVSTheory.subtpjudg(subtp, suptp))
        ), None, None)
        state.th add c
        state.th add Constant(state.th.toTerm, newName(id.getOrElse("INTERNAL_Assumption")), Nil,
          Some(state.bind(subtypeJudg(PVSTheory.expr(subtp), PVSTheory.expr(suptp)))),
          Some(state.bind(PVSTheory.subtpissubtype(subtp, suptp, c.path))), Some("Assumption"))

      case type_from_decl(ChainedDecl(NamedDecl(id, _, _), _, _), nonempty, tp) =>
        state.reset
        val name = newName(id)
        val realtp = doType(tp._declared)
        val c = Constant(state.th.toTerm, name, Nil, Some(PVSTheory.tp.term), None, None)
        state.th add c
        val d = Constant(state.th.toTerm, newName("INTERNAL_Assumption"), Nil,
          Some(state.bind(PVSTheory.subtpjudg(OMS(c.path), realtp))), None, Some("Assumption"))
        state.th add d
        state.th add Constant(state.th.toTerm, newName("INTERNAL_Assumption"), Nil,
          Some(state.bind(subtypeJudg(PVSTheory.expr(OMS(c.path)), PVSTheory.expr(realtp)))),
          Some(state.bind(PVSTheory.subtpissubtype(OMS(c.path), realtp, d.path))), Some("Assumption"))
        state.th add Constant(state.th.toTerm, newName(id + "_pred"), Nil,
          Some(state.bind(PVSTheory.expr(PVSTheory.fun_type(realtp, PVSTheory.bool.term)))), None, Some("Assumption"))
      // TODO add type equality name = setsubtype(name_pred,actsup)

      case type_decl(ChainedDecl(NamedDecl(id, _, _), _, _), nonempty) =>
        // if (state.th.path != (PVSTheory.rootdpath / "Prelude") ? "numbers" && id!="number") {
        val c = Constant(state.th.toTerm, newName(id), Nil, Some(PVSTheory.tp.term), None, None)
        state.th add c
        if (nonempty) {
          state.th add Constant(state.th.toTerm, newName("INTERNAL_Assumption"), Nil, Some(
            PVSTheory.nonempty(c.toTerm)),
            None, if (isAss) Some("Assumption") else None)
        }

      case name_judgement(OptNamedDecl(id, _, _, _), nameexpr, tp) =>
        val name = newName(id.getOrElse("Name_Judgment"))
        state.reset
        val (exp, restp) = doExpr(nameexpr)
        val fulltp = PVSTheory.tpjudg(exp, restp, doType(tp._internal))
        state.th add Constant(state.th.toTerm, name, Nil, Some(state.bind(fulltp)), None, if (isAss) Some("Assumption") else None)

      case enumtype_decl(NamedDecl(id, _, _), enum_elts) =>
        state.th add Constant(state.th.toTerm, newName(id), Nil, Some(PVSTheory.tp.term),
          Some(PVSTheory.enumtype(enum_elts.map(_._id))), None)

      case importing(_, namedec) =>
        state.addinclude(doMPath(namedec, true))

      case inline_datatype(InlineDatatypeBody(NamedDecl(id, _, _), arg_formals, constructors)) =>
        // TODO check if right
        val datatp = Constant(state.th.toTerm, doName(id), Nil, Some(PVSTheory.tp.term), None, None)
        state.th add datatp
        constructors foreach (doDatatypeConstructor(_, datatp))

      case theory_decl(ChainedDecl(NamedDecl(id, _, _), _, _), domain) =>
        val p = doMPath(domain, false)
        state.addinclude(p)
        //TODO : this should be named etc.

      case e@expr_judgement(optNamed, bindings, expr, tp) =>
        val name = newName(optNamed.id.getOrElse("Name_Judgment"))
        state.reset
        val (exp, restp) = doExpr(expr)
        val fulltp = PVSTheory.tpjudg(exp, restp, doType(tp._internal))
        //println(exp)
        if (bindings.nonEmpty) {
          println("bindings in Decl expr_judgement: " + bindings)
          println(e)
          sys.exit
        }
        state.th add Constant(state.th.toTerm, name, Nil, Some(state.bind(fulltp)), None, if (isAss) Some("Assumption") else None)
      case _ =>
        println("TODO Decl: " + d.getClass + ": " + d)
        sys.exit
    }
  }

  def doObject(o:Object) : Term = o match {
    case tp: Type => doType(tp)
    case e: Expr => doExpr(e)._1
    case _ =>
      println("TODO Object: " + o.getClass + ": " + o)
      sys.exit
  }

  def doType(t: Type): Term = {
    val tM: Term = t match {
      case type_name(_,name,res) =>
        doPath(name,res)
      case function_type(_,from,to) =>
        from match {
          case binding(id,named,tp) =>
            PVSTheory.pvspi(doName(id),doType(tp),doType(to))
          case t: Type => PVSTheory.fun_type(doType(t),doType(to))
        }
      case tuple_type(_,doms) =>
        val last = doms.reverse.head match {
          case tp : Type => doType(tp)
          case tp : DeclaredType => doType(tp._internal)
          case binding(id,named,tp) => doType(tp)
          case _ => throw new Exception("Last element of tuple is not independently typed")
        }
        val rest = doms.dropRight(1)
        rest.foldRight(last)((d,tm) => d match {
          case tp : Type => PVSTheory.tuple_type(List(doType(tp),tm))
          case tp : DeclaredType => PVSTheory.tuple_type(List(doType(tp._internal),tm))
          case binding(id,named,tp) => PVSTheory.pvssigma(doName(id),doType(tp),tm)
        })
      case expr_as_type(_,expr,optType) =>
        val (e,tp) = doExpr(expr)
        val tp2 = optType.map(doType)/*
        println(controller.presenter.asString(e))
        println(controller.presenter.asString(tp))
        if (optType.isDefined) {
          println(" = " + controller.presenter.asString(doType(optType.get)))
        }
        readLine()
        */

        PVSTheory.expr_as_type(e,tp2.getOrElse(tp))(state)
      case record_type(_,fields) =>
        PVSTheory.recordtp(fields.map(f => (doName(f.named.id),doType(f._type))):_*)
      case setsubtype(_,tp,exp) =>
        PVSTheory.setsub(doType(tp),doExpr(exp)._1)
      case type_application(_,tpname,args) =>
        ApplySpine(doType(tpname),args.map(doExpr(_)._1):_*)
      case type_extension(_,tp,by) =>
        PVSTheory.typeext(doType(tp),doType(by))
      case _ =>
        println("TODO Type: " + t.getClass + ": " + t)
        sys.exit
    }
    doSourceRef(t, tM)
    tM
  }

  def doExprAs(e : Expr,t : Term) : Term = {
    /*
    def equal(t1:Term,t2:Term) : Boolean = {
      if (t1==t2) true else (t1,t2) match {
        case (PVSTheory.pvspi(_,a,Lambda(_,_,b)),PVSTheory.pvspi(_,a2,Lambda(_,_,b2))) => equal(a,a2) && equal(b,b2)
        case (PVSTheory.tuple_type(l),PVSTheory.tuple_type(r)) => l.length==r.length && l.indices.forall(i => equal(l(i),r(i)))
        case (PVSTheory.pred(a),PVSTheory.pred(b)) => equal(a,b)
        case (PVSTheory.bool(),PVSTheory.bool()) => true
        case _ => false
      }
    }
    */
    val (tm,tp) = doExpr(e)
    /*
    if (!equal(tp,t)) {
      PVSTheory.typecast(tp,t,tm,if (state.tcc.isDefined) state.tcc.get else {
        null
      })
    } else */ tm
  }

  def doExpr(e: Expr): (Term,Term) = {
    val eM: (Term,Term) = e match {
      case name_expr(_,name,optp,res) =>
        (doPath(name,Some(res)),optp.map(doType).getOrElse(state.doUnknown))

      case forall_expr(_,bindings,body) => // TODO make this sequence compatible
        val vars = state.vars
        state.vars = Nil
        val bd = doExpr(body)._1
        val con = Context(bindings.map(b =>
        {
          val name = doName(b.id)
          val typ = doType(b._type)
          VarDecl(name,Some(typ),None,None)
        }):_*)
        bindings foreach (b=> state.vars = state.vars.variables.filter(_.name.toString != b.id.toString).toList)
        state.vars = (state.vars ::: vars).distinct
        (PVSTheory.forall(con,bd),PVSTheory.bool.term)

      case exists_expr(_,bindings,body) =>
        val vars = state.vars
        state.vars = Nil
        val bd = doExpr(body)._1
        val con = Context(bindings.map(b => {
          val name = doName(b.id)
          val typ = doType(b._type)
          VarDecl(name, Some(typ), None, None)
        }):_*)
        bindings foreach (b=> state.vars = state.vars.variables.filter(_.name.toString != b.id.toString).toList)
        state.vars = (state.vars ::: vars).distinct
        (PVSTheory.exists(con,bd),PVSTheory.bool.term)

      case application(_,f,arg,_) =>
        val (tmf,tpf) = doExpr(f)
        val (tmarg,tparg) = doExpr(arg)
        PVSTheory.pvsapply(tmf,tmarg,tpf)(state)
      case lambda_expr(_,bindings,body) =>
        val vars = state.vars
        state.vars = Nil
        val (bd,tptarget) = doExpr(body)
        val ret = bindings.foldRight((bd,tptarget))((b,pair) => {
          val tp = doType(b._type)
          val name = doName(b.id)
          (PVSTheory.pvslambda(name, tp, pair._2, pair._1),
            PVSTheory.pvspi(name, tp, pair._2))
        })
        bindings foreach (b=> state.vars = state.vars.variables.filter(_.name.toString != b.id.toString).toList)
        state.vars = (state.vars ::: vars).distinct
        ret

      case tuple_expr(_,args) =>
        val tms = args.map(doExpr)
        PVSTheory.tuple_expr(tms)

      case varname_expr(_,id,tp) =>
        val name = doName(id)
        val typ = doType(tp)
        state.vars ::= VarDecl(name,Some(typ),None,None)
        (OMV(name),typ)

      case cases_expr(_,expr,sels) =>
        // variables here?
        val (t,tp) = doExpr(expr)
        val cases = sels.map(s => {
          val (cons,casetp) = doExpr(s._cons)
          PVSTheory.selection(
            cons,
            s.bindings.map(b => VarDecl(doName(b.id),Some(doType(b._type)),None,None)),
            doExpr(s._expr)._1,
            casetp
          )
        })
        (PVSTheory.pvsmatch(t,tp,cases,state.doUnknown),state.doUnknown)

      case field_appl_expr(_,id,expr) =>
        val (tm,tmtp) = doExpr(expr)
        (PVSTheory.fieldapp(tm,id),tmtp match {
          case PVSTheory.recordtp(ls) => ls.find(p => p._1==id).getOrElse(("",state.doUnknown))._2
          case _ => state.doUnknown
        })

      case record_expr(_,asslist) =>
        val list = asslist.map(a => {
          val (df,tp) = doExpr(a._expr)
          (a.assignment_args match {
            case List(field_assign(_,id)) => doName(id)
            case _ => throw new Exception("field_assign expected!")
          },tp,df)
        })
        (PVSTheory.recordexpr(list:_*),PVSTheory.recordtp(list.map(t => (t._1,t._2)):_*))

      case number_expr(_,j) => (OMLIT(j,NatLiterals),NatLiterals.pvstp)

      case rational_expr(_,s) => (OMLIT(s.replace(" ","/"),RationalLiterals),RationalLiterals.pvstp)

      case proj_appl_expr(_,expr,i) =>
        val (tm,tp) = doExpr(expr)
        PVSTheory.projection(tm,tp,i)(state)

      case update_expr(_,expr,assignlist) =>
        // variables here?
        val (tm,tp) = doExpr(expr)
        def doUpdateAssignment(ex : Term,ass : update_assignment) : Term = {
          val (asstm,asstp) = doExpr(ass._expr)
          PVSTheory.update(ex,ass.assignment_args match {
            case field_assign(_,id)::args =>
              val realargs = args.map(x => doExpr(x.asInstanceOf[Expr])._1)
              PVSTheory.recupdate(id,asstm,realargs)
            case List(proj_assign(_,i)) =>
              PVSTheory.tupleupdate(i,asstm)
            case ls : List[Expr] if ls.forall(_.isInstanceOf[Expr])/*(e:Expr)*/ =>
              val expr = PVSTheory.tuple_expr(ls.map(doExpr))
              PVSTheory.funupdate(expr._1,asstm)
            case x =>
              println("TODO update_expr assignment arg of type " + x.head.getClass)
              println("Update " + ex)
              println(" - " + asstm)
              println(" - " + asstp)
              println(" - " + x)
              println(" in " + tm + " : " + tp)
              sys.exit
          })
        }
        (assignlist.foldLeft(tm)((t,ass) => doUpdateAssignment(t,ass)),tp)

      case string_expr(_,str) => (OMLIT(str,StringLiterals),StringLiterals.synType)

      case _ =>
        println("TODO Expr: " + e.getClass + ": " + e)
        sys.exit
    }
    doSourceRef(e, eM._1)
    eM
  }

  def doName(s:String) : LocalName = LocalName(s)

  def newName(s:String,start:Int = 1,ln : Option[LocalName] = None) : LocalName = {
    val n = ln.getOrElse(doName(s))
    if (!state.th.declares(n) && !state.th.parameters.exists(v => v.name==n)) n
    else {
      val newn = doName(s + "_" + start)
      newName(s, start + 1,Some(newn))
    }
  }

  def doSourceRef(o: Object, oM: Term) = {
    if (o.place!="") {
      val List(bR, bC, eR, eC) = stringToList(o.place, " ").map(_.toInt)
      val reg = SourceRegion(SourcePosition(-1, bR, bC), SourcePosition(-1, eR, eC))
      SourceRef.update(oM, SourceRef(FileURI(bt.inFile), reg))
    }
  }

  def doPath(n:name,res:Option[resolution]) : Term = {
    var (id,thid,library_id,mappings,opttarget,allactuals) = (n,res) match {
      case (name(id1,thid1,library_id1,mappings1,opttarget1,actuals1,dactuals1),Some(resolution(theory_name(_,thid2,library_id2,mappings2,opttarget2,actuals2,dactuals2),ind))) =>
        (id1+(if(ind>0) "_"+ind else ""),
          if (thid2 == "") thid1 else thid2,
          if (library_id2 == "") library_id1 else library_id2,
          mappings2,opttarget2,actuals2:::dactuals2)
      case (name(id1,thid1,library_id1,mappings1,opttarget1,actuals,dactuals),None) =>
        (id1,thid1,library_id1,mappings1,opttarget1,actuals:::dactuals)
    }

    val mpath = doMPath(theory_name("",thid,library_id,mappings,opttarget,allactuals,Nil))

    if(mappings.nonEmpty) {
      println("Found mappings in doPath")
      sys.exit
    }
    if(opttarget.isDefined) {
      println("Found target in doPath")
      sys.exit
    }

    if (mpath == state.th.path && (state.th.parameters.exists(v => v.name==LocalName(id)) || state.inductive.contains(mpath ? id))) {
      // if (state.vars.exists(v => v.name==LocalName(id))) println("Yields: " + OMV(id))
      OMV(id)
    } else if (mpath.toString == state.th.path.toString) {
      OMS(state.th.path ? id)
    } else if (mpath == PVSTheory.thpath) {
      val sym = OMS(mpath ? id)
      if (allactuals.nonEmpty) ApplySpine(sym, allactuals map (a => doObject(a)): _*) else sym
    } else {
      if (state.isPrelude) state.addinclude(mpath)
      // should be unnecessary ouside of Prelude
      val sym =
      if (state.isPrelude || !((PVSTheory.rootdpath / "Prelude") <= mpath)) {
        if (state.th.includes.inPars(mpath) contains true) OMV(LocalName(mpath) / id) else
          OMS(state.th.path ? (LocalName(mpath) / id))
      }
      else OMS(PVSTheory.preludepath ? (LocalName(mpath) / id))
      // apply theory parameters
      if (allactuals.nonEmpty) ApplySpine(sym, allactuals map (a => doObject(a)): _*) else sym
      // println("Yields: " + ret)
    }
  }

  def doMPath(thname : theory_name, isimport : Boolean = false) : MPath = {
    var (thid,library_id,mappings,opttarget,allactuals) =
      (thname.id,thname.library_id,thname.mappings,thname.target,thname.actuals ::: thname.dactuals)

    val doc =
      if (library_id=="") {
        if (thid == state.th.name.toString) path
        else if (isimport)  state.th.path.^^
        else {
          val optth = state.th.includes.find(p => p.^^ == state.th.path.^^ && p.name.toString == thid)
          if (optth.isDefined) optth.get.^^
          else DPath((URI.http colon "pvs.csl.sri.com") / "Prelude")
        }
      }
      else {
        val ret = DPath(state.th.path.parent.uri.resolve(library_id))
        //println("PATH: " + ret)
        ret
        //sys.exit
      }
    // DPath((URI.http colon "pvs.csl.sri.com") / (if (library_id=="") "Prelude" else  library_id))

    // redirect booleans and equalities
    if (doc.toString == "http://pvs.csl.sri.com/Prelude" && (thid == "booleans" || thid == "equalities")) {
      // println("Yields: " + OMS(PVSTheory.thpath ? id))
      return PVSTheory.thpath
    }
    // if ((doc ? thid)!=state.th.path) println("PATH TO: " + (doc ? thid))
    doc ? thid
  }

}