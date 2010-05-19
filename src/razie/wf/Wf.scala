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
object wf extends WfBaseLib[WfAct] {
   
  override def wrap (e:WfExec) : WfAct = razie.wf.WfWrapper (e)
  
  def toDsl (x:AnyRef) = x match {
     case a : HasDsl => a.toDsl 
     case _ => throw new IllegalArgumentException ("x not HasDsl cls="+x.getClass.getName)
  }
   
  //----------------- base activitities
   
  // return the parsed workflow or throw exception with error
  def apply (s:String) : WfAct = {
     val res = WCF.parseitman(s) // TODO use factory and hookup with WCF registration of libraries
    
     if (res.successful) res.get
     else throw new IllegalArgumentException (res.toString)
  }
  
  //----------------- if
 
  type FB = WFunc[Boolean]
  type Cond1 = Any => Boolean
  
  implicit def wc0 (cond : => Boolean) : WFunc[Boolean] = new WFunc[Boolean] { override def apply (in:AC, v:Any) = cond }
  def wc1 (cond : Cond1) : WFunc[Boolean] = new WFunc[Boolean] { override def apply (in:AC, v:Any) = cond(v) }
  implicit def wc3 (script : String) : WFunc[Boolean] = WCFExpr parseBExpr script
//  implicit def wc2 (e : BExpr) : WFunc[Boolean] = (x) => true // TODO evaluate the s
  
//  def wif  (cond : => Boolean) (f: => WfAct)     = WfIf (cond, f)
  def wif  (cond : Cond1) (f: => WfAct)     = WfIf (wc1(cond), f)
  def wif  (cond : FB) (f: => WfAct)     = WfIf (cond, f)
//  def wif  (cond : BExpr) (f: => WfAct)     = WfIf (cond, f)
  
  //----------------- match
  
//  def wmatch2 (expr : =>Any) (f: WfCases2) = WfMatch2 ((x,y)=>expr, f.l)
  def wmatch2 (expr : XExpr) (f: WfCases2) = WfMatch2 (expr, f.l)
//  def wguard1 (expr : =>Any) (f: WfCases1) = WfGuard1 (()=>expr, f)
  // this should work because it is only called when the value actually matches...
  def wcaseany2 (f: => WfAct) = new WfCaseAny2(f)
  def wcase2[T] (t:T) (f: => WfAct) = new WfCase2[T](t)(f)
  
//  def wmatch (expr : =>Any) (f: WfCases2) = wmatch2 (expr)(f)
  def wmatch (expr : XExpr) (f: WfCases2) = wmatch2 (expr)(f)
  def wcaseany (f: WfAct) = wcaseany2(f)
  def wcase[T] (t:T) (f: => WfAct) = new WfCase2[T](t)(f)

  // --------------- seq, par
  
  def seq (a : WfAct*) : WfAct = // optimization - if just one unconnected sub-graph, don't wrap in SEQ
     if (a.size == 1 && a.first.glinks.isEmpty) a.first
     else new WfSeq (a:_*)
    
  def par (a : WfAct*) : WfPar = new WfPar (a:_*)
  
  def label (n:String, a : WfAct) = new WfLabel (n, a)
    
//  def split (a : WfAct*) : WfAct = new WfSelectMany (a:_*)
  
  /** bound this subgraph in a scope, if needed */
  def scope (a : WfAct) = // optimization - if just one unconnected sub-graph, don't wrap in scope
     if (a.glinks.isEmpty) a
     else new WfScope (a)
     
//  implicit val linkFactory = (x,y) => WL(x,y)
  
  /** bound this subgraph in a scope, if needed */
  def scope (a : WfAct, l:WL*) = // optimization - if just one unconnected sub-graph, don't wrap in scope
     if (a.glinks.isEmpty) {
        l map (a +-> _.z)
        a
     } else 
        new WfScope (a, l:_*)

}

/** 
 * this deals with scala workflows - these are not nice because can't be serialized/distributed easily
 * 
 * @author razvanc
 */
object wfs {
   
  //----------------- base activitities
   
  // TODO this doesn't work if implicit...see ScaBug1
  implicit def w   (f : => Unit) = WfScala (()=>f)
  def w   (f : => Any) = WfScalaV0 (()=>f)
  def wa  (f : Any => Any) = WfScalaV1 ((x)=>f(x))
  implicit def wau (f : Any => Unit) = WfScalaV1u ((x)=>f(x))
  
  def apply (f : => Unit) = w(f)
  def apply (f : => Any) = w(f)

  //----------------- if
  
  type FB = WFunc[Boolean]
  type Cond1 = Any => Boolean

  implicit def wc0 (cond : Boolean) : WFunc[Boolean] = new WFunc[Boolean] { override def apply (in:AC, v:Any) = cond }
  def wc1 (cond : Any => Boolean) : WFunc[Boolean] = new WFunc[Boolean] { override def apply (in:AC, v:Any) = cond(v) }
  
  def wuif (cond : FB) (f: => Unit)    = WfIf (cond, WfScala(()=>f))
  def wsif  (cond : FB) (f: => Any)     = WfIf (cond, WfScalaV0(()=>f))
  def waif (cond : Cond1) (f: Any => Any) = WfIf (wc1(cond), WfScalaV1((x)=>f(x)))
  def waif (cond : FB) (f: Any => Any) = WfIf (cond, WfScalaV1((x)=>f(x)))
  def wauif (cond : FB) (f: Any => Unit)= WfIf (cond, WfScalaV1((x)=>f(x)))
  
  //----------------- match
  
  // def wmatch1 (expr : =>Any) (f: PartialFunction[Any, Unit]) = WfMatch1 (()=>expr, WfCaseB (()=>expr, (x:Any)=>f.apply(x)))
  def wmatch1 (expr : =>Any) (f: WfCases1) = WfMatch1 (()=>expr, f)
  def wguard1 (expr : =>Any) (f: WfCases1) = WfGuard1 (()=>expr, f)
  def wcase1 (f: => PartialFunction[Any, WfAct]) = new WfCase1(f)
  def wcaseany1 (f: WfAct) = new WfCaseAny1(f)
  
  def wcase2[T] (t:T) (f: => Unit) = new WfCase2[T](t)(w(f))
  def wcase2[T] (cond: T => Boolean) (f: => Unit) = new WfCase2p[T](cond)(w(f))
  def wcase2a[T <: Any] (f: T => Unit) = new WfCase2a[T](wau(x => f(x.asInstanceOf[T])))
  def wcase2a[T <: Any] (cond:T => Boolean)(f: T => Unit) = new WfCase2ap[T](cond)(wau(x => f(x.asInstanceOf[T])))
  
  def wcase[T] (t:T) (f: => Unit) = wcase2(t)(f)
  def wcase[T] (cond: T => Boolean) (f: => Unit) = wcase2(cond)(f)
  def wcasea[T <: Any] (f: T => Unit) = wcase2a(f)
}
