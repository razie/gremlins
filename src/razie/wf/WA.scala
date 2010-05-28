/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.AA
import razie.base.{ActionContext => AC}
import razie.g._

/** 
 * This is the basic node in the graph: an activity waiting to be traversed/executed. 
 * 
 * The workflow is modelled as a graph of activities connected by links/dependencies.
 * 
 * Mixing in the state also allows its removal, should I decide to store it outside, later...cool, huh?
 */
abstract class WfAct extends razie.g.GNode[WfAct, WL] with WfaState with razie.g.WRGraph[WfAct, WL] {
  override var gnodes : Seq[WfAct] = Nil // next activities - note that this is not containment, is it?
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
  def traverse (from: Option[WL], in:AC, v:Any) : (Any,Seq[WL]) = traverse (in, v)
    
  override def toString : String = this.getClass().getSimpleName + "()"
  
  // syntax niceties 
  def + (e:WfAct) : WfAct = WfSeq (this,e)
  def | (e:WfAct*) : WfAct = WfPar (this,e.toSeq:_*)
  
  def print () : WfAct = { println (this mkString); this}
  
  // simplified execution
  def run (initialValue : Any) : Any = 
    Engines().exec(this, razie.base.scripting.ScriptFactory.mkContext(), initialValue)

  implicit val linkFactory : LFactory = (x,y) => WL(x,y) // for the nice inherited --> operators 

  // -------------- specific to WF? could move down?
  
  /** bound: point all leafs to z, an end node, while avoiding z --> z */
  def --| (z:WfAct)(implicit linkFactory: LFactory)  = {
    ( razie.g.Graphs.filterNodes[WfAct,WL](this) {a => a.glinks.isEmpty && a != z} ) foreach (i => i +-> z)
    this
  }
  
  def <-- (z:WfAct) : WfAct =  z --> this
  def <-+ (z:WfAct) : WfAct =  z +-> this
}
 
/** may want to store some color here...or have the link do something */
case class WL (a:WfAct, z:WfAct) extends razie.g.GLink[WfAct] with WflState 

/** special link with a selector value. it assists the parent in deciding where to go next */
case class WLM (aa:WfAct, zz:WfAct, val selector:WFunc[Boolean]) extends WL (aa,zz)
 
/** special link with a selector value. it assists the parent in deciding where to go next */
//case class WLV (aaa:WfAct, zzz:WfAct, val aselector:Any) extends WLM (aaa,zzz, WFCMP(aselector))
case class WLV (aaa:WfAct, zzz:WfAct, val selector:Any) extends WL (aaa,zzz)

