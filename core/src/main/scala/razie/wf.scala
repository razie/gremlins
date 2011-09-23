/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie

import razie.base.{ ActionContext => AC }
import razie.gremlins._
import razie.gremlins.lib._
import razie.gremlins.act._

/** 
 * these are the final nice wrappers for all the basic activities
 * 
 * NOTE the wf and wfs are two different gremlin worlds and should not be mixed. Import one xor the other
 * 
 * TODO study 5 will settle the underlying workflow engine - there were some details not working in study4,
 * especially arround scopes (WfProxy contents)...to fix that I had to invent the WfScope but that messed
 * up the simplicity of the traversing engine.
 * 
 * I chose to solve these by changing WfProxy's behaviour to not just proxy exec but instead redirect 
 * the graph through its actions...this way there's no change in the engine - it remains simple graph 
 * traversal - hence the WfScope on top of WfProxy (which i actually didn't change but have replaced 
 * with WfScope)
 * 
 * @author razvanc
 */
object wf extends WfBaseLib[WfActivity] {

  override def wrap(e: WfExec): WfActivity = new WfWrapper(e)

  final val INDENT = "                                                                                       "

  def indent(block: => String): String = block.split('\n').map("  " + _) mkString "\n"

  def toDsl(x: AnyRef) = x match {
    case a: HasDsl => a.toDsl
    case _ => throw new IllegalArgumentException("x not HasDsl cls=" + x.getClass.getName)
  }

  import razie.gremlins.res.WTimer
  def timeout(time: Long)(f: => WfActivity): WfActivity = {
    val x = f
    val a = WfResReq (WTimer.gref, WTimer.WAITREL, AA(), CExpr(time))
    par {
      seq {
        a
        new WfResReply()
        cancel (x)
      }
      seq {
        x
        WfResReq (WTimer.gref, WTimer.CANCEL, AA(), CExpr(a.tok))
        new WfResReply()
      }
    }
  }

  def sleepAsync(time: Long): WfActivity =
    seq {
      WfResReq (WTimer.gref, WTimer.WAITREL, AA(), CExpr(time))
      new WfResReply()
    }

  def cancel(target: WfActivity): WfActivity = new eng.WfSkip(x => target)
  //  def cancel (g:Gref) = WfSkip (_.resolve(g))
  //  def cancel (w:WPath) = WfSkip (_.resolve(w))

  //----------------- base activitities

  // return the parsed workflow or throw exception with error
  def apply(s: String): WfActivity = {
    val res = WCF.parseitman(s)
    // TODO use factory and hookup with WCF registration of libraries

    if (res.successful) res.get
    else throw new IllegalArgumentException(res.toString)
  }

  //----------------- if

  type FB = WFunc[Boolean]
  type Cond1 = Any => Boolean

  implicit def wc0(cond: => Boolean): WFunc[Boolean] = new WFunc[Boolean] { override def apply(in: AC, v: Any) = cond }
  def wc1(cond: Cond1): WFunc[Boolean] = new WFunc[Boolean] { override def apply(in: AC, v: Any) = cond(v) }
  implicit def wc3(script: String): WFunc[Boolean] = WCFExpr parseBExpr script
  //  implicit def wc2 (e : BExpr) : WFunc[Boolean] = (x) => true // TODO evaluate the s

  //  def wif  (cond : => Boolean) (f: => WfActivity)     = WfIf (cond, f)
  def wif(cond: Cond1)(f: => WfActivity) = WfIf (wc1(cond), f)
  def wif(cond: FB)(f: => WfActivity) = WfIf (cond, f)
  //  def wif  (cond : BExpr) (f: => WfActivity)     = WfIf (cond, f)

  //----------------- match

  //  def wmatch2 (expr : =>Any) (f: WfCases2) = WfMatch2 ((x,y)=>expr, f.l)
  def wmatch2(expr: AExpr)(f: WfCases2) = WfMatch2 (expr, f.l)
  //  def wguard1 (expr : =>Any) (f: WfCases1) = WfGuard1 (()=>expr, f)
  // this should work because it is only called when the value actually matches...
  def wcaseany2(f: => WfActivity) = new WfCaseAny2(f)
  def wcase2[T](t: T)(f: => WfActivity) = new WfCase2[T](t)(f)

  //  def wmatch (expr : =>Any) (f: WfCases2) = wmatch2 (expr)(f)
  def wmatch(expr: AExpr)(f: WfCases2) = wmatch2 (expr)(f)
  def wcaseany(f: WfActivity) = wcaseany2(f)
  def wcase[T](t: T)(f: => WfActivity) = new WfCase2[T](t)(f)

  // --------------- seq, par

  def seq(a: Seq[WfActivity]): WfActivity = // optimization - if just one unconnected sub-graph, don't wrap in SEQ
    if (a.size == 1 && a.head.glinks.isEmpty) a.head
    else new WfSeq(a: _*)

  def par(a: Seq[WfActivity]): WfPar = new WfPar(a: _*)

  // this is the funny version
  def seq(f: => WfActivity): WfActivity = {
    val lb = new collection.mutable.ListBuffer[WfActivity]
    WfaCollector.collect (lb append _) (this) (f)
    // construct with the colected
    seq (lb)
  }

  // this is the funny version
  def par(f: => WfActivity): WfPar = {
    val lb = new collection.mutable.ListBuffer[WfActivity]
    WfaCollector.collect (lb append _) (this) (f)
    // construct with the colected
    par (lb)
  }

  def label(n: String, a: WfActivity) = new WfLabel(n, a)

  //  def split (a : WfActivity*) : WfActivity = new WfSelectMany (a:_*)

  /** bound this subgraph in a scope, ONLY if needed */
  def scope(a: WfActivity) = // optimization - if just one unconnected sub-graph, don't wrap in scope
    if (a.glinks.isEmpty) a
    else {
      // optimized - if a is a scope with a single unconnected END, don't wrap again
      val ends = razie.g.Graphs.entire[WfActivity, WfLink](a).dag filterNodes { z => z.glinks.isEmpty }
      if (ends.size == 1) a
//      if (ends.size == 1 && ends.head.isInstanceOf[WfScopeEnd]) a
      else new WfScope(a)
    }

  //  implicit val linkFactory = (x,y) => WfLink(x,y)

  /** bound this subgraph in a scope, if needed */
  def scope(a: WfActivity, l: WfLink*) = // optimization - if just one unconnected sub-graph, don't wrap in scope
    // TODO optimize - if a is a scope with a single unconnected END, don't wrap again
    if (a.glinks.isEmpty) {
      l map (a +-> _.z)
      a
    } else
      new WfScope(a, l: _*)

}
