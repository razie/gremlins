package razie.wf.study5

import razie.AA
import razie.base.{ActionContext => AC}

//-------------------- engine/graph

/** 
 * the workflow is modelled as a graph of activities connected by links/dependencies.
 * 
 * This is the basic node in the graph: an activity waiting to be traversed/executed. 
 * 
 * Mixing in the state also allows its removal, should I decide to store it outside, later...cool, huh?
 */
abstract case class WA () extends razie.g.GNode[WA, WL] with WfaState with razie.g.WRGraph[WA, WL] {
  override var gnodes : Seq[WA] = Nil // next activities - note that this is not containment, is it?
  override var glinks : Seq[WL] = Nil // links 
   
  /** the engine is traversing the graph...similar to executing it.
   * 
   * executing these means maybe doing something (in=>out) AND figuring out who's next
   * 
   * @param in the current context, containing variables objects whatnot
   * @param v the current value (in good scala tradition, each statemnt returns a value - this is basically the preceeding value)- 
   * @return (my value, next activities - the links I think should go next). 
   * 
   * NOTE that the links returned must be one of the static links - you can't make up new ones...otherwise I cannot recover state from storage?
   */
  def traverse (in:AC, v:Any) : (Any,Seq[WL])
    
  override def toString : String = this.getClass().getSimpleName + "()"
}
 
/** may want to store some color here...or have the link do something */
case class WL (val a:WA, val z:WA) extends razie.g.GLink[WA] { 
}
  
/** starting to add syntax niceties, implementation-transparent */
trait WfAct extends WA {
  def + (e:WfAct) = WfSeq (this,e)
  def | (e:WfAct) = WfPar (this,e)
  
  // simplified execution
  def run (initialValue : Any) : Any = 
    Engines().exec(this, razie.base.scripting.ScriptFactory.mkContext(), initialValue)

  implicit val linkFactory : LFactory = (x,y) => WL(x,y)
   
//  /** reroute */
//  def --> (z:WfAct) = super.--> (linkFactory, z)
//  /** add a new dependency */
//  def +-> (z:WfAct) = super.+-> (linkFactory, z) 
//  /** par depy a -> (b,c) */
//  def --> (z:Seq[WfAct]) = super.--> (linkFactory, z)
//  /** par depy a -> (b,c) */
//  def +-> (z:Seq[WfAct]) = super.+-> (linkFactory, z)
     
}
