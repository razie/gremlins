/** ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie

import java.lang.IllegalStateException
import razie.base.{ ActionContext => AC }
import razie.gremlins.act.WfCase2ap
import razie.gremlins.act.WfGroupBy
import razie.gremlins.act.WfSliceMap
import razie.gremlins.act.WfCase1
import razie.gremlins.act.WfCase2
import razie.gremlins.act.WfCase2a
import razie.gremlins.act.WfCase2p
import razie.gremlins.act.WfCaseAny1
import razie.gremlins.act.WfCases1
import razie.gremlins.act.WfDynIf
import razie.gremlins.act.WfDynIfa
import razie.gremlins.act.WfDynPar
import razie.gremlins.act.WfDynSeq
import razie.gremlins.act.WfFlatten
import razie.gremlins.act.WfFoldLeft
import razie.gremlins.act.WfGuard1
import razie.gremlins.act.WfIf
import razie.gremlins.act.WfLabelInsert
import razie.gremlins.act.WfMap
import razie.gremlins.act.WfMatch1
import razie.gremlins.act.WfPar
import razie.gremlins.act.WfScala
import razie.gremlins.act.WfScalaV0
import razie.gremlins.act.WfScalaV1
import razie.gremlins.act.WfScalaV1u
import razie.gremlins.act.WfSort
import razie.gremlins.act.WfeScala
import razie.gremlins.act.WfeScalaV1
import razie.gremlins.WFunc
import razie.gremlins.$Expr
import razie.gremlins.BExpr
import razie.gremlins.WfActivity
import razie.gremlins.WfExec
import razie.gremlins.WfaCollector
import razie.gremlins.lib.CSP
import razie.gremlins.lib.WfChannel

/** scala workflows - these are not nice because can't be serialized/distributed easily
 *
 *  NOTE the wf and wfs are two different gremlin worlds and should not be mixed. Import one xor the other
 *
 *  Lazy Example:
 *  {{{
 *  def workflow = seq {
 *   par {
 *     seq {
 *       println ("he he - definition time")
 *       async { _ + "runtime-a" }
 *       }
 *     async { _ + "runtime-b" }
 *     }
 *     sort[String] (_ < _)
 *    later { case x : List[String] => x mkString "," }
 *    }
 *  }}}
 *
 *  The body of the different nodes are executed as the nodes are run!
 *
 *  Strict Example:
 *  {{{
 *  val strict_workflow = wfs strict workflow // that's why workflow above is a def not a val
 *  }}}
 *
 *  The ENTIRE body is executed once, like the constructor of a class. only specially marked statements
 *  become nodes in the workflow...those prefixed by a keyword in this object.
 *
 *  The workflow then built can be run at will, once! You can build it again and again wihta def instead of val.
 *
 *  As a general rule, each node gets an input value and returns an output value. The "par" will create a list.
 *
 *  @author razvanc
 */
class wfs {
  import razie.wf

  // shortcut for nocolelct -  don't collect these activities. Collection is a hack for the workflow DSL
  def noCollect[T](f: => T): T = WfaCollector.noCollect (f)

  /** do not collect the contents, just the result */
  def collectOne(f: => WfActivity): WfActivity = {
    val body = noCollect {
      f
    }
    WfaCollector.current.map { _ collect body }
    body
  }

  /** do not collect the contents, just the result */
  def resultOf(f: => WfActivity): Unit = collectOne (f)

  /** interpret this workflow definition right now, not later */
  def strict(f: => WfActivity): WfActivity = WfaCollector.flagged ("strict") (f)

  /** build a lazy seq node in a scala workflow. Note that the body won't be executed until the workflow is started */
  def seqf(f: (Any) => Any): WfActivity = later (f)
  /** create leaf activity containing scala code. It cannot contain more activities */
  //  def sync(f: (Any) => Any): WfActivity = w(f)
  //  def sync(f: => Unit): WfActivity = w(f)
  /** create leaf activity containing scala code. It cannot contain more activities */
  def async(f: (Any) => Any): WfActivity = w(f)
  //  def async(f: => Unit): WfActivity = w(f)

  /** call a sub-process with a different startup value than $0
   *
   *  @param e an expression evaluated in the context of THIS workflow
   */
  def call(e: $Expr)(f: => WfActivity): WfActivity = new WfsCall(e, f) // TODO not tested
  def call(f: => WfActivity): WfActivity = new WfsCall($0, f) // TODO not tested
  
  def scall(a: => WfActivity) = noCollect {a}.run ("kk")

  /** scala code with input and return values */
  class WfsCall(val e: $Expr, val a: WfActivity) extends WfScalaV1((x) => x) {
    override def apply(in: AC, v: Any): Any = a.run (e.apply(in, v))
  }

  /** build a lazy seq node in a scala workflow. Note that the body won't be executed until the workflow is started */
  def seq(f: => Unit): WfActivity = new WfDynSeq(new WfeScala(f))

  //  // this allows seq{par{}}
  //  def seq(f: => WfDynPar) = new WfDynSeq(new WfeScala(f))

  /** build a lazy par node in a scala workflow. Note that the body won't be executed until the workflow is started.
   *
   *  the body of the par is executed when the par node is started. at the end, all defined nodes will be started in parallel
   */
  def par(f: => Unit) = new WfDynPar(new WfeScala(f))

  //----------------- base activitities

  /** create leaf activity containing scala code. It cannot contain more activities */
  def w(f: => Unit): WfActivity = new WfScala(() => {
    WfaCollector.cantCollect ("async") { f }
  })

  /** create leaf activity containing scala code. It cannot contain more activities */
  def w(f: Any => Any): WfActivity = new WfScalaV1((x) => {
    WfaCollector.cantCollect ("sync") { f(x) }
  })
  //  def w(f: => Any) = new WfScalaV0(() => f)
  //  def wa(f: Any => Any) = new WfScalaV1((x) => f(x))
  //  /*implicit*/ def wau(f: Any => Unit) = new WfScalaV1u((x) => f(x))

  def later(f: Any => Any): WfActivity = w(f)
  //  def later(f: => Unit): WfActivity = w(f)
  def matchLater[B](f: PartialFunction[Any, B]) =
    new WfScalaV1((x) => if (f.isDefinedAt(x)) f(x) else x)

  //  def apply(f: => Unit) = w(f)
  def apply(f: Any => Any) = w(f)

  /** assign name in context to the last value produced by the previous activity */
  def assign(name: String) = wf.assign(name, wf.$0)

  def $(name: String) = wf.$(name)
  def $0 = wf.$0

  //--------------------- simulating the let! from F# syntax

  class LetBang(name: String) {
    def !(f: => WfActivity) = wfs.let (name) (f)
  }

  def let = new LetBang("var-" + razie.g.GRef.uid)

  def ilet(name: String)(f: => WfActivity) = {
    // this is collected...but not in a par
    if (WfaCollector.who.map(_.isInstanceOf[WfPar]).getOrElse(false))
      throw new IllegalStateException("You can't use a let in a par{} block")
    val v = new LetBangVar(name)
    (wf seq noCollect {
      Seq(f, wf.wrap(new LetBangVarAssign(v)))
    },
      v)
  }

  def let(name: String)(f: => WfActivity) = ilet (name) (f)._2

  //----------------- if

  type FB = WFunc[Boolean]
  type Cond1 = Any => Boolean

  implicit def wc0(cond: Boolean): WFunc[Boolean] = new WFunc[Boolean] { override def apply(in: AC, v: Any) = cond }
  def wc1(cond: Any => Boolean): WFunc[Boolean] = new WFunc[Boolean] { override def apply(in: AC, v: Any) = cond(v) }
  def wcAnd(cond: Any => Boolean)(wf: WFunc[Boolean]): WFunc[Boolean] = new WFunc[Boolean] { override def apply(in: AC, v: Any) = cond(v) && wf.apply(in, v) }
  def wcnot(wf: WFunc[Boolean]): WFunc[Boolean] = new WFunc[Boolean] { override def apply(in: AC, v: Any) = !wf.apply(in, v) }

  def wif(cond: FB)(f: => Unit) = new WfDynIf(cond, f)
  def wif(cond: Cond1)(f: => Unit) = new WfDynIf(wc1(cond), f)

  //----------------------- list stuff

  def map[A, B](f: A => B) = new WfMap[A, B](f)
  def imap[A, B](a: Int, b: Int)(f: A => B) = new WfSliceMap[A, B](new WfeScalaV1(x => (a, b)))(f)

  def foldLeft[T](zero: T)(plus: (T, T) => T) = new WfFoldLeft[T](zero)(plus)
  def groupBy[T, K](g: T => K) = new WfGroupBy[T, K](g)
  def sort[T](lt: (T, T) => Boolean) = new WfSort[T](lt)

  implicit val linkFactory = (x, y) => new razie.gremlins.WfLink(x, y)

  class LString(name: String) {
    //    def label (f: => WfActivity) = wf.label(name, f)
    def label(f: => WfActivity) = new WfLabelInsert(name) --> noCollect { f }
  }
  implicit def toLString(label: String) = new LString(label)

  //  def wforeach[T](f: => WfActivity) = wf seq noCollect {
  def wforeach[T](f: => WfActivity) = "wforeach" label noCollect {
    val (vorigA, vorig) = wfs.ilet ("var-" + razie.g.GRef.uid) (wf.nop)
    val (va, v) = wfs.ilet ("var-" + razie.g.GRef.uid) (new WfScalaV0(() => 0))
    val head = wf.label ("head", wf.nop)
    val loop = wf.label ("loop", wf.stop(1))
    val done = wf.label ("done", wf.stop(1))
    val body = wf seq Seq (
      // swap list with current element
      "swapElement" label w { x => vorig.get.asInstanceOf[List[T]] (v.get.asInstanceOf[Int]) },
      f,
      "next" label w { x => v.get.asInstanceOf[Int] + 1 }, // increment counter
      wf.wrap(new LetBangVarAssign(v)),
      "restore" label w { x => vorig.get } // restore the list
      )
    body --> loop
    val i = wfs strict {
      new WfDynIfa(
        wc1(x => { v.get.asInstanceOf[Int] < vorig.get.asInstanceOf[List[T]].size }),
        body) welse done
    }

    // after the if is built, redirect end to head
    loop --> head

    //    Seq(va, va1, head, i)
    vorigA --| va --| (head --> i)
  }

  val max: Int = 100

  /** build the equivalent construct of an actor: a loop reading from a queue, until the body's result evaluates the condition to false
   *
   *  NOTE that the body needs to respect the reset() because it will loop
   *
   *  NOTE if you want to have body behave like an actor (not read from default but wait on a channel), then use a channel yourself, see the PingPong parallel sample, in samples
   *
   *  @param cond is a boolean expression evaluated on each message, while true the actor runs, when false it stops
   *  @param body the body of the actor
   */
  def actWhile(e: Any => Boolean)(body: => WfActivity): WfActivity = "actUntil" label noCollect {
    repeatUntil (!e(_)) {
      //      (channel ? body)
      body
    } + wf.stash (wf.log ("actUntil loop is completed"))
  }

  def doWhile(e: Any => Boolean)(body: => WfActivity): WfActivity =
    doWhile (wc1(x => e(x))) (body)

  def doWhile(e: WFunc[Boolean])(body: => WfActivity): WfActivity = wf.doWhile(e)(body)

  def repeatUntil(e: Any => Boolean)(body: => WfActivity): WfActivity =
    repeatUntil (wc1(x => e(x))) (body)

  def repeatUntil(e: WFunc[Boolean])(body: => WfActivity): WfActivity = wf.repeatUntil(e)(body)

  def wsmap[A, B](branches: Int)(f: A => B) = "wsmap" label noCollect {
    val p = seq {
      par {
        for (i <- 0 until branches)
          new WfSliceMap[A, B](new WfeScalaV1(x => {
            val s = x.asInstanceOf[List[A]].size
            val step = s / branches
            val max = s / branches
            (i * step, min ((i + 1) * step, s))
          }))(f)
      }
      // sort the lists
      w { x =>
        {
          val v = x.asInstanceOf[List[(Int, List[B])]]
          val r = v.sortWith ((a, b) => a._1 < b._1)
          r map (_._2)
        }
      }
      new WfFlatten[B]
    }
    p
  }

  private def min(a: Int, b: Int): Int = if (a < b) a else b

  //  simple map branch
  def wimap[T](f: => WfActivity) = "wsimap" label noCollect {
    val (vorigA, vorig) = wfs.ilet ("var-" + razie.g.GRef.uid) (wf.nop)
    val (va, v) = wfs.ilet ("var-" + razie.g.GRef.uid) (new WfScalaV0(() => 0))
    val (vac, vc) = wfs.ilet ("var-" + razie.g.GRef.uid) (new WfScalaV0(() => List()))
    val head = wf.label ("head", wf.nop)
    val loop = wf.label ("loop", wf.stop(1))
    val done = wf.label ("done", wf.stop(1))
    val body = wf seq Seq (
      // swap list with current element
      "swapElement" label w { x => vorig.get.asInstanceOf[List[T]] (v.get.asInstanceOf[Int]) },
      f,
      "collect" label w { x => x :: vc.get.asInstanceOf[List[T]] },
      wf.wrap(new LetBangVarAssign(vc)),
      "next" label w { x => v.get.asInstanceOf[Int] + 1 }, // increment counter
      wf.wrap(new LetBangVarAssign(v)),
      "restore" label w { x => vorig.get } // restore the list
      )
    body --> loop
    val i = new WfDynIfa(
      wc1(x => { v.get.asInstanceOf[Int] < vorig.get.asInstanceOf[List[T]].size }),
      body) welse done

    val result = "result" label w { x => vc.get }

    // after the if is built, redirect end to head
    loop --> head

    //    Seq(va, va1, head, i)
    vorigA --| va --| vac --| (head --> i) // --| result
  }

  def wmap[T](branches: Int)(f: => WfActivity) = "wmap" label noCollect {
    val (va1, v1) = wfs.ilet ("var-" + razie.g.GRef.uid) (wf.nop)
    val (va, v) = wfs.ilet ("var-" + razie.g.GRef.uid) (new WfScalaV0(() => 0))
    val (vac, vc) = wfs.ilet ("var-" + razie.g.GRef.uid) (new WfScalaV0(() => List()))
    val head = wf.label ("head", wf.nop)
    val loop = wf.label ("loop", wf.stop(1))
    val done = wf.label ("done", wf.stop(1))
    val body = wf seq Seq (
      // swap list with current element
      "1" label w { x => v1.get.asInstanceOf[List[T]] (v.get.asInstanceOf[Int]) },
      f,
      "2" label w { x => v.get.asInstanceOf[Int] + 1 }, // increment counter
      wf.wrap(new LetBangVarAssign(v)),
      "3" label w { x => v1.get } // restore the list
      )
    body --> loop
    val i = new WfDynIfa(
      wc1(x => { v.get.asInstanceOf[Int] <= wf.$0.asInstanceOf[List[T]].size }),
      body) welse done

    // after the if is built, redirect end to head
    loop --> head

    //    Seq(va, va1, head, i)
    va1 --| va --| vac --| (head --> i)
  }

  //-----------------------  full workflow loops
  //  def wmap (f: Any => Any): WfActivity = {
  ////     this is collected...but not in a par
  //    if (WfaCollector.who.map(_.isInstanceOf[WfPar]).getOrElse(false))
  //      throw new IllegalStateException("You can't use a wmap in a par{} block")
  //    wf seq noCollect {
  //      val v = new LetBangVar(name)
  //      val m = WfMap(WfeScalaV1(f))
  //      Seq(wf.wrap(new LetBangVarAssign(v), m, welsegoto(m))
  //    }
  //    v
  //  }

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

/** collects the current value of the workflow */
class LetBangVarAssign(l: LetBangVar) extends WfExec {
  override def apply(in: AC, prevValue: Any) = {
    l set Option(prevValue)
    // also set it in the context...?
    in.set (l.name, l)
    prevValue
  }
}

/** collects the current value of the workflow */
class LetBangVar(val name: String) {
  var v: Option[Any] = None
  def set(o: Option[Any]) { v = o }
  def apply() = v.get
  def get = v.get
  def opt = v
  override def toString = v.toString
}

object wfs extends wfs

/** scala workflow tests - these are not nice because can't be serialized/distributed easily
 *
 *  Note the wf and wfs are two different gremlin worlds and should not be mixed. Import one xor the other
 *
 *  @author razvanc
 */
object wft extends wfs {
  def apply(f: => Unit) = w(f)
  //----------------- base activitities

  // TODO this doesn't work if implicit...see ScaBug1
  //  /*implicit*/ def w(f: => Unit) = new WfScala(() => f)
  //  def w(f: => Any) = new WfScalaV0(() => f)
  def wa(f: Any => Any) = new WfScalaV1((x) => f(x))
  /*implicit*/ def wau(f: Any => Unit) = new WfScalaV1u((x) => f(x))

  //----------------- if

  def wuif(cond: FB)(f: => Unit) = noCollect { WfIf (cond, new WfScala(() => f)) }
  def wuif(cond: Cond1)(f: => Unit) = noCollect { WfIf (wc1(cond), new WfScala(() => f)) }

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
