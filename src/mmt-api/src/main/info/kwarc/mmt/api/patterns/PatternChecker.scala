package info.kwarc.mmt.api.patterns

import info.kwarc.mmt.api._
import libraries._
import modules._
import frontend._
import symbols._
import objects._
import notations._

import objects.Conversions._

import utils._
import utils.MyList._

import scala.io.Source

/*

/** tries to turn constants into instances
 */
class PatternChecker(controller: Controller) {
  private var valid : Boolean = true // theory is valid? 
  
  case class getPatternsError(msg : String) extends java.lang.Throwable(msg)
/** retrieves patterns from theory
* 
*/
  def getPatterns(home : Term)(n : Int) : List[Pattern] = {         
     home match {
       case OMMOD(p) => 
         val thy = controller.globalLookup(p)
         thy match {
           case d : DeclaredTheory => d.meta match { // other case d: DefinedTheory, it does not have meta
             case Some(m) => 
               val cmeta = controller.globalLookup(m)
               cmeta match {
                 case mthy : DeclaredTheory => 
                   mthy.getPatterns.filter {p =>
                       p.body.variables.toList.length == n 
                   }
                 case _ => Nil //TODO
               }
             case None => Nil
            }
           case _ => throw getPatternsError("not a declared theory")
          }
        case _ => throw getPatternsError(home.toString + " not an OMMOD") 
     }
  }
  def patternCheck(constants : List[Constant], pattern : Pattern) : Option[Instance] = {        
    if (constants.length == pattern.body.length) {
      val mat = new Matcher(controller,pattern.params)
      var sub = Substitution() // obsolete?
      // substitution
      val z = constants.zip(pattern.body).map {
        case (con,decl) =>
          val dtype = decl.tp.map(t => t ^ sub)
          val ddef = decl.df.map(d => d ^ sub)
//          sub ++ Sub(con.name,decl.name)      
//          println("matching >> " + con.name +  " <<>> " + pattern.name)
          val res1 = mat(con.tp,dtype,Context())// match type
          val res2 = mat(con.df,ddef,Context())// match definition
          (res1,res2) match {
            case (Some(a),Some(b)) => sub = sub ++ a ++ b 
              						Some(a ++ b)
            case _ => None
          }
      }.flatten
      if (z.isEmpty) return None // if no matches were found, z is empty list, so this is not the pattern we want
      sub = z.foldLeft(sub)((a,b) => a ++ b)
      mat.metaContext.toSubstitution
      val c = constants(0)
      Some(new Instance(c.home,c.name,c.home.toMPath ? pattern.name,sub.map(_.target)))
      
    } else None //Fail: Wrong number of declarations in pattern or number of constants               
  }  
  
  // check const decl vs list of patterns
  def getInstance(constList : List[Constant], pattList : List[Pattern]) : List[Instance]= {        
        val ins = constList.map{
          c => pattList.map( p => {
            this.patternCheck(List(c),p) match {
              case Some(sub) => Some(sub)
              case None => None
            } 
          }) 
          }.flatten.flatten
        val valid = constList.length == ins.length
        ins
  }
 /*	takes 1 constant and a list of declaration patterns
  * validates in 
  */
  def getInstance(const : Constant, pattList : List[Pattern]) : List[Instance] = {
    val ins = pattList.map { p =>
      patternCheck(List(const),p)
    }.flatten
    ins.length match {
      case 1 => println("constant " + const.home.toString() + const.name + " validated") 
      case 0 => valid = false
      		println("constant " + const.home.toString() + const.name + " could not be validated: no patterns matched")
      case _ => valid = false
      		println("constant " + const.home.toString() + const.name + " could not be validated: more than one pattern matched")
    }
    ins
  }
}


class Matcher(controller : Controller, var metaContext : Context) {  
  def apply(dterm : Term, pterm : Term, con : Context = Context()) : Option[Substitution] = {    
    //if (lengthChecker(dterm,pterm)) {          
//    println("matching " + dterm.toString() + " with " + pterm.toString())    
        (dterm,pterm) match {
        	// OM reference
        	case (OMID(a), OMID(b)) =>
							if (a == b) //Some(Substitution(Sub("OMID match",OMID(a)))) else None
							Some(Substitution())
							else None
        	// variables
            case (OMV(v),OMV(w)) => if ((v == w) && (con.isDeclared(v) && con.isDeclared(w)))
              Some(Substitution(Sub(OMV(w).name,OMV(v)))) 
              else None  
            // function application  
            case (OMA(f1,args1),OMA(f2,args2)) => 
               val fun = apply(f1,f2,con) match {
                 case Some(a) => a
                 case None => return None // if functions did not match, quit
               }
               val argl = args1.zip(args2).map { 
                  case (x,y) => apply(x,y,con) 
               }              
               // if some arguments did not match, the lengths will be different
               if (argl.length != argl.flatten.length) return None  
               val args = argl.flatten.foldRight(Substitution())((a,b) => a ++ b)
               Some(fun ++ args)
            // OM symbol
            case (OMS(a),OMS(b)) => apply(OMID(a),OMID(b), con)
            // conditional binder
            case (OMBINDC(b1, ctx1, scopes1), OMBINDC(b2,ctx2,scopes2)) => 
              val res1 = apply(b1,b2,con) 
              val res2 = scopes1.zip(scopes2).map {
                 case (s1,s2) => apply(s1,s2,con ++ ctx1)
              }
              if (res1.isDefined && res2.forall(_.isDefined))
                 Some(res2.foldLeft(res1.get) {case (x,y) => x ++ y.get})
              else None
            // var to anything  
            case (OMV(v), x) => if (metaContext.isDeclared(v)) Some(Substitution(Sub("OMV to Term", OMV(v)))) else None 
            // check if constant and variable types are the same                        
            case (OMS(s),OMV(v)) => {
              val const = controller.globalLookup.getConstant(s)
              val vardecs = metaContext.variables
              val vardec = vardecs.find(x => x.name == v)
              vardec match {                
                case Some(w) => (w.tp,const.tp) match {
                  	case (None,None) => Some(Substitution(Sub("OMS to OMV",OMV(v.toString))))//empty  
                  	case (Some(a),Some(b)) => if (a == b) Some(Substitution(Sub("OMS to OMV" + OMV(v).name,a))) else None
                  	case (_,_) => None
                }
                case _ => None
              }              
            }
            // term to var
            case (t, OMV(v)) => {                
              if (metaContext.isDeclared(v)) Some(Substitution(Sub(OMV(v).name,t)))
              else None
            }
            case (_,_) => None      
        }
    //}
  }
    
  def apply(dterm : Option[Term], pterm : Option[Term], con : Context) : Option[Substitution] = {
    (dterm,pterm) match {
      case (Some(d),Some(p)) => apply(d,p,con)//?
      case (None,None) => Some(Substitution())// occurs when there is no definient 
      case (_,_) => None
    }
  }
  
  def lengthChecker(term1 : Term, term2 : Term) : Boolean = {
    true
     //val len1N = normalize(length(term1))
     //val len2N = normalize(length(term2))
    /*
     (term1,len2N) match {
       case (OMI(m),OMI(n)) if (m == n) => true
       case (_,_) => 
         freeVars(len1N)
         freeVars(len1N)
         lengthSolver()
     }
     */   
  }
}


// a test run with THF (THF0) patterns
object PatternTest  {  
  //TODO 
     // read omdoc file content --> register archive and call content command    
     // should get a list of constants
     // check constants one by one - thf can only have one declaration anyway     
     // check a parsed constant immediatelly against all patterns OR get list of constants and then check      
     
  val tptpbase = DPath(URI("http://latin.omdoc.org/logics/tptp"))
  val pbbase = DPath(URI("http://oaff.omdoc.org/tptp/problems"))// problem base
  val baseType = new Pattern(OMID(tptpbase ? "THF0"), LocalName("baseType"),Context(), OMV("t") % OMS(tptpbase ? "Types" ? "$tType"), NotationContainer())
  val typedCon = new Pattern(OMID(tptpbase ? "THF0"), LocalName("typedCon"), OMV("A") % OMS(tptpbase ? "Types" ? "$tType") , OMV("c") % OMA(OMS(tptpbase ? "Types" ? "$tm"), List(OMV("A"))), NotationContainer() )
  val axiom = new Pattern(OMID(tptpbase ? "THF0"), LocalName("axiom"), OMV("F") % OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMS(tptpbase ? "THF0" ? "$o"))) , OMV("c") % OMA(OMS(tptpbase ? "Types" ? "$istrue"), List(OMV("F"))), NotationContainer() )
  val typedConDef = new Pattern(OMID(tptpbase ? "THF0"), LocalName("typedConDef"), OMV("A") % OMS(tptpbase ? "Types" ? "$tType") ++ OMV("D") % OMA(OMS(tptpbase ? "Types" ? "$tm"), List(OMV("A"))), VarDecl(LocalName("c"),Some(OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMV("A")))),Some(OMV("D")),None), NotationContainer())
  val theorem = new Pattern(OMID(tptpbase ? "THF0"), LocalName("theorem"), OMV("F") % OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMS(tptpbase ? "THF0" ? "$o"))) ++ OMV("D") % OMA(OMS(tptpbase ? "Types" ? "$tm"),List(OMS(tptpbase ? "THF0" ? "$o"))), VarDecl(LocalName("c"),Some(OMA(OMS(tptpbase ? "Types" ? "$istrue"), List(OMV("F")))),Some(OMV("D")),None), NotationContainer())
  val controller = new Controller
  controller.handleLine("file pattern-test.msl")// run what's written in this file first - add logs, archives etc.
  controller.add(baseType)
  controller.add(typedCon)
  controller.add(axiom)
  controller.add(typedConDef)
  controller.add(theorem)
    
  case class Error(msg : String) extends java.lang.Throwable(msg)  
  
  def main(args : Array[String]) {  
    val pc = new PatternChecker(controller)        														
//    val testget = controller.globalLookup.getStructure(pbbase / "SomeProblem.omdoc" ? "SomeProblem")
    														// file name ? theory name ? constant name 
    
    // get list of constant declarations
//    val conMu = try { 
//      controller.globalLookup.getConstant(pbbase  ? "AGT027^1" ? "mu")
//    } catch {
////      case GetError(m) => GetError//TODO : deal with the error  
//      case e => e.printStackTrace()
//    }
    val constTheory = controller.globalLookup.getTheory(pbbase  ? "AGT027^1") match {
      case c : DeclaredTheory => c
      case _ => throw Error("no constants in " + pbbase + "?" + "AGT027^1")
    }
    val constList : List[Constant] = constTheory.getConstants
    //     get list of patterns from controller
    val pp = try {
      controller.globalLookup.getPattern(tptpbase ? "THF0" ? "baseType")
    } catch {
//      case GetError(m) => throw GetError(m)
      case e : Throwable => e.printStackTrace()
    }
    val pattTheory = controller.globalLookup.getTheory(tptpbase ? "THF0") match {
      case t : DeclaredTheory => t
      case _ => throw Error("no patterns in " + tptpbase + "?" + "THF0")      
    }        
    val pattList : List[Pattern] = pattTheory.getPatterns
        
    
    //     <------------------------ pattern checking happens here  ------------------------------->
    // for each constant declaration check with each pattern declaration
    val matches = constList.foreach {
        a => println(a.name.toString) 
          pattList.foreach {
          p => {
           pc.patternCheck(List(a),p) match {
             case Some(ins) => {
            	 println(a.name + " matches " + p.name)
            	 println("substitution: " + ins.matches.toString())
             }
             case _ =>
           } 
          } 
        }
      }
    
    
//      val testCon = controller.globalLookup.getConstant(pbbase  ? "SomeProblem" ? "meq_ind")
//    
//      pc.patternCheck(List(testCon), typedConDef)
//      pc.patternCheck(List(testCon), theorem)
//      pc.patternCheck(List(testCon), typedCon) 
//      pc.patternCheck(List(testCon), axiom) 
    
  }
  
    
}


*/





















