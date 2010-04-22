/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.AA
import razie.base.{ActionContext => AC}

/** 
 * study 5 will settle the underlying workflow engine - there were some details not working in study4,
 * especially arround scopes (WfProxy contents)...to fix that I had to invent the WfScope but that messed
 * up the simplicity of the traversing engine.
 * 
 * I chose to solve these by changing WfProxy's behaviour to not just proxy exec but instead redirect 
 * the graph through its actions...this way there's no change in the engine - it remains simple graph 
 * traversal - hence the WfScope on top of WfProxy (which i actually didn't change but have replaced 
 * with WfScope)
 * 
 * I added the joins or "xxxEnd" nodes, the WfScope
 * 
 * @author razvanc
 */
object wf extends WfLib[WfAct] {
   
  override def wrap (e:WfExec) : WfAct = razie.wf.WfWrapper (e)
   
  //----------------- base activitities
   
  // TODO this doesn't work if implicit...see ScaBug1
  implicit def w   (f : => Unit) = WfScala (()=>f)
  implicit def w   (f : => Any) = WfScalaV0 (()=>f)
  implicit def wa  (f : Any => Any) = WfScalaV1 ((x)=>f(x))
  implicit def wau (f : Any => Unit) = WfScalaV1u ((x)=>f(x))
  
  def apply (f : => Unit) = w(f)
  def apply (f : => Any) = w(f)
  
  //----------------- if
  
  type Cond0 = Unit => Boolean
  type Cond1 = Any => Boolean

  implicit def wc0 (cond : => Boolean) : Cond1 = (x) => cond
  
  def wif  (cond : Cond1, t:WfAct)        = WfIf (cond, t)
  def wuif (cond : Cond1) (f: => Unit)    = WfIf (cond, WfScala(()=>f))
  def wif  (cond : Cond1) (f: => Any)     = WfIf (cond, WfScalaV0(()=>f))
  def waif (cond : Cond1) (f: Any => Any) = WfIf (cond, WfScalaV1((x)=>f(x)))
  def wauif (cond : Cond1) (f: Any => Unit)= WfIf (cond, WfScalaV1((x)=>f(x)))
  
  //----------------- match
  
  // def wmatch1 (expr : =>Any) (f: PartialFunction[Any, Unit]) = WfMatch1 (()=>expr, WfCaseB (()=>expr, (x:Any)=>f.apply(x)))
  def wmatch1 (expr : =>Any) (f: WfCases1) = WfMatch1 (()=>expr, f)
  def wguard1 (expr : =>Any) (f: WfCases1) = WfGuard1 (()=>expr, f)
  def wcase1 (f: => PartialFunction[Any, WfAct]) = new WfCase1(f)
  def wcaseany1 (f: WfAct) = new WfCaseAny1(f)
  
  def wmatch2 (expr : =>Any) (f: WfCases2) = WfMatch2 (()=>expr, f.l)
//  def wguard1 (expr : =>Any) (f: WfCases1) = WfGuard1 (()=>expr, f)
  def wcase2[T] (t:T) (f: => Unit) = new WfCase2[T](t)(w(f))
  def wcase2[T] (cond: T => Boolean) (f: => Unit) = new WfCase2p[T](cond)(w(f))
  // this should work because it is only called when the value actually matches...
  def wcase2a[T <: Any] (f: T => Unit) = new WfCase2a[T](wau(x => f(x.asInstanceOf[T])))
  def wcase2a[T <: Any] (cond:T => Boolean)(f: T => Unit) = new WfCase2ap[T](cond)(wau(x => f(x.asInstanceOf[T])))
  def wcaseany2 (f: WfAct) = new WfCaseAny2(f)
  
  def wmatch (expr : =>Any) (f: WfCases2) = wmatch2 (expr)(f)
  def wcase[T] (t:T) (f: => Unit) = wcase2(t)(f)
  def wcase[T] (cond: T => Boolean) (f: => Unit) = wcase2(cond)(f)
  def wcasea[T <: Any] (f: T => Unit) = wcase2a(f)
  def wcaseany (f: WfAct) = wcaseany2(f)

  def seq (a : WfAct*) = // optimization - if just one unconnected sub-graph, don't wrap in SEQ
     if (a.size == 1 && a.first.glinks.isEmpty) a.first
     else new WfSeq (a:_*)
    
  /** bound this subgraph in a scope, if needed */
  def bound (a : WfAct) = // optimization - if just one unconnected sub-graph, don't wrap in scope
     if (a.glinks.isEmpty) a
     else new WfScope (a)
     
  implicit val linkFactory = (x,y) => WL(x,y)
  
  /** bound this subgraph in a scope, if needed */
  def bound (a : WfAct, l:WL*) = // optimization - if just one unconnected sub-graph, don't wrap in scope
     if (a.glinks.isEmpty) {
        l map (a +-> _)
        a
     } else 
        new WfScope (a, l:_*)
     
}
