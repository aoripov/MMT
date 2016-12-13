package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import frontend._
import info.kwarc.mmt.api.objects._

import scala.collection.mutable.HashSet


object QueryEvaluator {
  val free = OMID(utils.mmt.mmtbase ? "mmt" ? "free")
  type QuerySubstitution = List[(LocalName, BaseType)]
}

/** evaluates a query expression to a query result */
class QueryEvaluator(controller: Controller) {
  /**
    * Evaluates a Query in memory, expects all I()s to be resolved already
    *
    * @param q
    * @return
    */
  def apply(q: Query): QueryResult = {
    log(q.toString)
    QueryChecker.infer(q)(Context.empty) match {
      case TupleQuery(_) => ElemResult(evalElem(q)(Nil))
      case SetTupleQuery(_) => SetResult(evalSet(q)(Nil).map(ElemResult))
    }
  }

  private def log(msg: => String) {
    controller.report("query", msg)
  }

  private class ResultSet extends HashSet[List[BaseType]] {
    def +=(b: BaseType) {
      this += List(b)
    }
  }

  private def empty = new ResultSet

  private def singleton(b: BaseType) = {
    val res = empty;
    res += b;
    res
  }

  private lazy val rs = controller.depstore
  private lazy val lup = controller.globalLookup

  /**
    * Evaluates an element query.
    *
    * @param q       Query to evaluate
    * @param context Context to evaluate Query in
    * @return
    */
  private def evalElem(q: Query)(implicit subst: QueryEvaluator.QuerySubstitution): List[BaseType] = evalSet(q).head // just take the head

  /**
    * Evaluates a single element query
    *
    * @param q
    * @return
    */
  private def evalSingleElem(q: Query)(implicit subst: QueryEvaluator.QuerySubstitution): BaseType = evalElem(q).head


  /**
    * Evaluates
    *
    * @param e
    * @param subst
    * @return
    */
  private def evalSinglePath(e: Query)(implicit subst: QueryEvaluator.QuerySubstitution): Path = evalSingleElem(e).asInstanceOf[Path]

  /** pre: Query.infer(e) = Elem(ObjectType) */
  private def evalElemObj(e: Query)(implicit subst: QueryEvaluator.QuerySubstitution): Obj = evalSingleElem(e).asInstanceOf[Obj]

  /**
    * Evaluates an isoltaed querySet.
    *
    * @param q
    * @param hint
    * @param subst
    * @return
    */
  private def evalISet(q: Query, hint: String)(implicit subst: QueryEvaluator.QuerySubstitution): HashSet[List[BaseType]] = ???

  private def evalSet(q: Query)(implicit subst: QueryEvaluator.QuerySubstitution): HashSet[List[BaseType]] = q match {
    /** evaluate a query with a hint */
    case I(qq, Some(h)) => evalISet(qq, h)

    /** evaluate a query without a hint */
    case I(qq, None) => evalSet(qq) // TODO: Throw a warning or think about what to do

    /** bound variable => lookup in the substitution */
    case Bound(vn) => singleton(subst.find(_._1 == vn).get._2)

    /** get the components of a path */
    case ontology.Component(of, comp) =>
      val res = empty
      evalSet(of) foreach {
        case List(p: ContentPath) =>
          lup.get(p).getComponent(comp) match {
            case Some(tc: AbstractTermContainer) => tc.get foreach {
              res += _
            }
            case Some(_) => throw GetError("component exists but does not indicate an object: " + comp)
            case _ => throw GetError("component does not exist: " + comp)
          }
        case _ => throw ImplementationError("ill-typed query")
      }
      res

    /** retrieve a subobject of a single result */
    case SubObject(of, pos) =>
      val (con, obj) = evalElemObj(of).subobject(pos)
      val closure = obj match {
        case t: Term => OMBIND(QueryEvaluator.free, con, t)
        case o => o //TODO assuming bound variables occur only in terms
      }
      singleton(closure)

    /** query the rs for all the objects */
    case Related(to, by) =>
      val res = empty
      evalSet(to) foreach { p =>
        rs.query(p.head.asInstanceOf[Path], by)(res += _) // p has type List(Path) by precondition
      }
      res

    /** Literal => return the item as is */
    case Literal(b) => singleton(b)

    /** Literals => return the items as is */
    case Literals(bs@_*) =>
      val res = empty
      bs foreach {
        res += _
      }
      res

    /** evaluate the outer query and add to the context */
    case Let(vn, v, in) =>
      val vE = evalSingleElem(v)
      evalSet(in)((vn, vE) :: subst)

    /** evaluate a singleton query */
    case Singleton(e) =>
      val res = empty
      res += evalElem(e)
      res

    /** get a set of paths to objects */
    case Paths(c) =>
      val res = empty
      rs.getInds(c) foreach {
        res += _
      }
      res

    /** all objects that unify with a certain object */
    case Unifies(_) =>
      // TODO: Implement this
      throw ImplementationError("Unifies() query not implemented")

    /** close of a set of paths */
    case Closure(of) =>
      evalSinglePath(of) match {
        case p: MPath =>
          val res = empty
          rs.theoryClosure(p) foreach {
            res += _
          }
          res
        case p => throw GetError("must be a module path " + p)
      }

    /** union of the result sets */
    case Union(s, t) =>
      evalSet(s) union evalSet(t)

    /** a big union of sets */
    case BigUnion(dom, vn, s) =>
      val res = empty
      evalSet(dom) foreach {
        e => evalSet(s)((vn, e.head) :: subst) foreach { x => res += x }
      }
      res

    /** intersection of sets */
    case Intersection(s, t) =>
      evalSet(s) intersect evalSet(t)

    /** set difference */
    case Difference(o, w) =>
      evalSet(o) diff evalSet(w)

    /** comprehension filters by a predicate */
    case Comprehension(dom, vn, pred) =>
      evalSet(dom) filter { e => evalProp(pred)((vn, e.head) :: subst) }

    /** Tuple: run each query one by query */
    case Tuple(l) =>
      val res = empty
      res += l map { q => evalSingleElem(q) }
      res

    /** Projection takes the ith element */
    case Projection(p, i) =>
      val t = evalElem(q)
      val res = empty
      res += t(i - 1)
      res

    /** applies a Query Function */
    case QueryFunctionApply(fun, args, param) =>
      val argsE = evalSet(args)
      val res = empty
      argsE foreach {
        case List(b) =>
          res += fun.evaluate(b, param)
        case _ => throw ImplementationError("ill-typed query")
      }
      res

    case _ => throw ImplementationError("unknown query")
  }

  private def evalProp(p: Prop)(implicit subst: QueryEvaluator.QuerySubstitution): Boolean = p match {
    /* check e is a c */
    case IsA(e, c) =>
      rs.getInds(c) contains evalSinglePath(e)

    /** check if a path is a prefix of another */
    case PrefixOf(short, long)
    => evalSinglePath(short) <= evalSinglePath(long)

    /** check if e is in t */
    case IsIn(e, t) =>
      evalSet(t) contains evalElem(e)

    /** check if a result set is empty */
    case IsEmpty(r) =>
      evalSet(r).isEmpty

    /** check if two elements are equal */
    case Equal(l, r) =>
      evalElem(l) == evalElem(r)

    /** check if one or the other is true */
    case Or(l, r) =>
      evalProp(l) || evalProp(p)

    /** check if a single value exists */
    case Exists(dom, vn, sc) =>
      evalSet(dom) exists { e => evalProp(sc)((vn, e.head) :: subst) }

    /** check that something is not the case */
    case Not(q) =>
      !evalProp(q)

    /** check that both are true */
    case And(l, r) =>
      evalProp(l) && evalProp(p)

    /** check that it is true for all values */
    case Forall(dom, vn, sc) =>
      evalSet(dom) forall { e => evalProp(sc)((vn, e.head) :: subst) }

    /** check that a judgement holds for a given item */
    case Holds(about, varname, j) =>
      // TODO: Implement this
      throw ImplementationError("Holds() predecate not implemented")
  }
}