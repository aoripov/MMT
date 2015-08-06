package info.kwarc.mmt.leo.AgentSystem.MMTSystem

import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.{Active, modules, RuleSet}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.leo.AgentSystem.Blackboard

/**
 * Created by Mark on 7/21/2015.
 *
 * This represents the class of the LF blackboard which handles proofs in the LF prover
 */
class MMTBlackboard(val rules:RuleSet,val goal: Goal)(implicit controller: Controller,oLP:String) extends Blackboard {

  val c = controller
  override def logPrefix = oLP + "#MMTBlackboard"

  /*val invertibleBackward = rules.get(classOf[BackwardInvertible]).toList
  val invertibleForward  = rules.get(classOf[ForwardInvertible]).toList
  val searchBackward     = rules.get(classOf[BackwardSearch]).toList.sortBy(_.priority).reverse
  val searchForward      = rules.get(classOf[ForwardSearch]).toList*/

  val invertibleBackward = List(PiIntroduction)
  val invertibleForward  = Nil
  val searchBackward     = List(BackwardPiElimination)
  val searchForward      = List(ForwardPiElimination)
  val searchTerms        = List(TermGeneration)
  val transitivityRules  = List(TransitivityGeneration)

  log("Rules: " + rules)
  log("Invertible Backwards rules:" + invertibleBackward)

  val goalSection = new GoalSection(this,goal)
  addSection(goalSection)
  log("Added Goal of type: " + goal.getClass + goal)

  val shapeDepth = 2
  val factSection = new FactSection(this, shapeDepth)
  addSection(factSection)
  implicit val facts:Facts = factSection.data


  val termSection = new TermSection(this)
  addSection(termSection)
  implicit val terms:Terms = termSection.data

  val transitivitySection = new TransitivitySection(this)
  addSection(transitivitySection)


  implicit val presentObj: Obj => String = o => controller.presenter.asString(o)
  override lazy val report = controller.report

  /** convenience function to create a matcher in the current situation */
  def makeMatcher(context: Context, queryVars: Context) = new Matcher(controller, rules, context, queryVars)


  /** Boolean representing the status of the prof goal */
  override def finished: Boolean = goal.isSolved
}

object Indent {
  def indent(depth: Int) = (0 to depth).map(_ => "  ").mkString("")
}

