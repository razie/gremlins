/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wfs

import razie.AA
import razie.base.{ ActionContext => AC }
import razie.wf.{ wf, WfActivity, WfaCollector, WFunc, WfExec, HasDsl }
import razie.wf.act.WfPar
import razie.wf.act._

/** 
 * scala workflows - these are not nice because can't be serialized/distributed easily
 * 
 * NOTE the wf and wfs are two different gremlin worlds and should not be mixed. Import one xor the other
 * 
 * Example: {{{  val workflow = seq {    par {      seq {      println ("he he - definition time")        _ + "runtime-a"      }      seq {        _ + "runtime-b"      }    }    sort[String] (_ < _)    later { case x : List[String] => x mkString "," }  }}}}
 * 
 * The body is executed once, like the constructor of a class. only specially marked statements 
 * become nodes in the workflow...those prefixed by a keyword in this object.
 * 
 * The workflow then built can be run at will, once! You can build it again and again wihta def instead of val.
 * 
 * As a general rule, each node gets an input value and returns an output value. The "par" will create a list.
 * 
 * @author razvanc
 */
object wfs {
  // shortcut for nocolelct
  def noCollect[T](f: => T): T = WfaCollector.noCollect (f)

  /** build a lazy seq node in a scala workflow. Note that the body won't be executed until the workflow is started */
  def seqf(f: (Any) => Any): WfActivity = seq (f)
  def seq(f: (Any) => Any): WfActivity = new WfDynSeq(new WfeScalaV1(f))

  /** build a lazy seq node in a scala workflow. Note that the body won't be executed until the workflow is started */
  def seq(f: => Unit): WfActivity = new WfDynSeq(new WfeScala(f))

  // this allows seq{par{}}
  def seq(f: => WfActivity) = new WfDynSeq(new WfeScala(f))

  /** build a lazy par node in a scala workflow. Note that the body won't be executed until the workflow is started.
   * 
   *  the body of the par is executed when the par node is started. at the end, all defined nodes will be started in sequence */
  def par(f: => Unit) = new WfDynPar(new WfeScala(f))

  def foldLeft[T](zero: T)(plus: (T, T) => T) = new WfFoldLeft[T](zero)(plus)
  def sort[T](lt: (T, T) => Boolean) = new WfSort[T](lt)

  //----------------- base activitities

  // TODO this doesn't work if implicit...see ScaBug1
  def w(f: => Unit) = new WfScala(() => f)
  def w(f: Any => Any) = new WfScalaV1((x) => f(x))
  //  def w(f: => Any) = new WfScalaV0(() => f)
  //  def wa(f: Any => Any) = new WfScalaV1((x) => f(x))
  //  /*implicit*/ def wau(f: Any => Unit) = new WfScalaV1u((x) => f(x))

  //  def later(f: => Unit) = new WfScala(() => f)
  //  def later(f: (Any) => Any) = new WfScalaV1((x) => f(x))
  def later[B](f: PartialFunction[Any, B]) =
    new WfScalaV1((x) => if (f.isDefinedAt(x)) f(x) else x)

  def apply(f: => Unit) = w(f)
  def apply(f: Any => Any) = w(f)

  /** assign name in context to the last value produced by the previous activity */
  def assign(name: String) = wf.assign(name, wf.$0)

  def $(name: String) = wf.$(name)

  class LetBang(name: String) {
    def !(f: => WfActivity) = wfs.let (name) (f)
  }

  def let = new LetBang(razie.g.GRef.uid)

  def let(name: String)(f: => WfActivity) = {
    // this is collected...but not in a par
    if (WfaCollector.who.map(_.isInstanceOf[WfPar]).getOrElse(false))
      throw new IllegalStateException("You can't use a let in a par{} block")
    val v = new LetBangVar(name)
    wf.seq (noCollect { Seq(f, wf.wrap(new LetBangVarAssign(v))) })
    v
  }
}

/** collects the current value of the workflow */
class LetBangVarAssign(l : LetBangVar) extends WfExec {
  override def apply(in: AC, prevValue: Any) = {
    l set Option(prevValue)
  }
}

/** collects the current value of the workflow */
class LetBangVar(name: String) {
  var v: Option[Any] = None
  def set (o:Option[Any]) { v = o }
  def apply() = v.get
  def get = v.get
  def opt = v
  override def toString = v.toString
}

//class AsyncMonad[A] {
//  def flatMap[B] (f:A => AsyncMonad[B]) : AsyncMonad[B] = {
//  }
//}
//
//class AsyncMonad[A] (l:List[A]){
//  def map[B] (f:A => B) : AsyncMonad[B]  = {
//    new AsyncMonad (l map f)
//  }
//  def flatMap[B] (f:A => AsyncMonad[B]) : AsyncMonad[B] = {
//    new AsyncMonad (l flatMap f)
//  }
//}
//
//object TAM {
//  def async (l:List[_])   = new AsyncMonad (l)
//  for (a <- async (List(1,2,3))) yield a
//}

/** 
 * scala workflow tests - these are not nice because can't be serialized/distributed easily
 * 
 * Note the wf and wfs are two different gremlin worlds and should not be mixed. Import one xor the other
 * 
 * @author razvanc
 */
object wft {
  //----------------- base activitities

  // TODO this doesn't work if implicit...see ScaBug1
  /*implicit*/ def w(f: => Unit) = new WfScala(() => f)
  def w(f: => Any) = new WfScalaV0(() => f)
  def wa(f: Any => Any) = new WfScalaV1((x) => f(x))
  /*implicit*/ def wau(f: Any => Unit) = new WfScalaV1u((x) => f(x))

  def apply(f: => Unit) = w(f)
  def apply(f: => Any) = w(f)

  //----------------- if

  type FB = WFunc[Boolean]
  type Cond1 = Any => Boolean

  implicit def wc0(cond: Boolean): WFunc[Boolean] = new WFunc[Boolean] { override def apply(in: AC, v: Any) = cond }
  def wc1(cond: Any => Boolean): WFunc[Boolean] = new WFunc[Boolean] { override def apply(in: AC, v: Any) = cond(v) }

  def wuif(cond: FB)(f: => Unit) = WfIf (cond, new WfScala(() => f))
  def wsif(cond: FB)(f: => Any) = WfIf (cond, new WfScalaV0(() => f))
  def waif(cond: Cond1)(f: Any => Any) = WfIf (wc1(cond), new WfScalaV1((x) => f(x)))
  def waif(cond: FB)(f: Any => Any) = WfIf (cond, new WfScalaV1((x) => f(x)))
  def wauif(cond: FB)(f: Any => Unit) = WfIf (cond, new WfScalaV1((x) => f(x)))

  //----------------- match

  // def wmatch1 (expr : =>Any) (f: PartialFunction[Any, Unit]) = WfMatch1 (()=>expr, WfCaseB (()=>expr, (x:Any)=>f.apply(x)))
  def wmatch1(expr: => Any)(f: WfCases1) = new WfMatch1(() => expr, f)
  def wguard1(expr: => Any)(f: WfCases1) = new WfGuard1(() => expr, f)
  def wcase1(f: => PartialFunction[Any, WfActivity]) = new WfCase1(f)
  def wcaseany1(f: WfActivity) = new WfCaseAny1(f)

  def wcase2[T](t: T)(f: => Unit) = new WfCase2[T](t)(w(f))
  def wcase2[T](cond: T => Boolean)(f: => Unit) = new WfCase2p[T](cond)(w(f))
  def wcase2a[T <: Any](f: T => Unit) = new WfCase2a[T](wau(x => f(x.asInstanceOf[T])))
  def wcase2a[T <: Any](cond: T => Boolean)(f: T => Unit) = new WfCase2ap[T](cond)(wau(x => f(x.asInstanceOf[T])))

  def wcase[T](t: T)(f: => Unit) = wcase2(t)(f)
  def wcase[T](cond: T => Boolean)(f: => Unit) = wcase2(cond)(f)
  def wcasea[T <: Any](f: T => Unit) = wcase2a(f)
}
