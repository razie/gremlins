package razie.wf.study3

import razie.AA
import razie.base.{ActionContext => AC}
import razie.wf.{WfActBase, WfaState, ProcStatus, ProcState}

//-------------------- engine/graph

/** 
 * the workflow is modelled as a graph of activities connected by links/dependencies.
 * 
 * Mixing in the state alos allows its removal, should I decide to store it outside, later...cool, huh?
 */
abstract case class WA () extends WfActBase with razie.g.GNode[WA, WL] with WfaState {
  override def gnodes = activities
  override def glinks = links
    
  def activities : Seq[WA] // next activities - note that this is not containment, is it?
  def links : Seq[WL] // links 
   
  /** executing these means maybe doing something (in=>out) AND figuring out who's next */
  def execute (in:AC, v:Any) : (Any,Seq[WL])
    
  override def toString : String = this.getClass().getSimpleName + "()"
}
 
/** may want to store some color here...or have the link do something */
case class WL (val a:WA, val z:WA) extends razie.g.GLink[WA] { }
  

/** starting to add syntax niceties, implementation-transparent */
trait WfAct extends WA {
  def + (e:WfAct) = WfSeq (this,e)
  def | (e:WfAct) = WfPar (this,e)
}

