/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.AA
import razie.base.{ActionContext => AC}
import razie.g._

//-------------------- engine/graph

/** encapsulates the state of a workflow execution/traversal path */
//trait WfThreadState {
//  def comingFrom : WL
//}

/** 
 * the workflow is modelled as a graph of activities connected by links/dependencies.
 * 
 * This is the basic node in the graph: an activity waiting to be traversed/executed. 
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
  def + (e:WfAct) = WfSeq (this,e)
  def | (e:WfAct) = WfPar (this,e)
  
  def print () : WfAct = { println (this mkString); this}
  
  // simplified execution
  def run (initialValue : Any) : Any = 
    Engines().exec(this, razie.base.scripting.ScriptFactory.mkContext(), initialValue)

  implicit val linkFactory : LFactory = (x,y) => WL(x,y)

  // -------------- specific to WF? could move down?
  
  /** point all leafs to z - an end node */
  def --| (z:WfAct)(implicit linkFactory: LFactory)  = {
    ( razie.g.Graphs.filterNodes[WfAct,WL](this) {z => z.glinks.isEmpty} ) foreach (i => i +-> z)
    this
  }
 
}
 
/** may want to store some color here...or have the link do something */
case class WL (a:WfAct, z:WfAct) extends razie.g.GLink[WfAct] with WflState 

/** special link with a selector value. it assists the parent in deciding where to go next */
case class WLV (aa:WfAct, zz:WfAct, val selector:Any) extends WL (aa,zz)
  
/** starting to add syntax niceties, implementation-transparent */
//trait WfAct extends WA {
//  this : WfAct => 
   
//  def + (e:WfAct) = WfSeq (this,e)
//  def | (e:WfAct) = WfPar (this,e)
//  
//  // simplified execution
//  def run (initialValue : Any) : Any = 
//    Engines().exec(this, razie.base.scripting.ScriptFactory.mkContext(), initialValue)
//
//  implicit val linkFactory : LFactory = (x,y) => WL(x,y)
//
//  // -------------- specific to WF? could move down?
//  
//  /** point all leafs to z - an end node */
//  def --| ( z:WfAct)(implicit linkFactory: LFactory)  = {
//    // find all leafs and connect them to me
//    ( razie.g.GStuff.filterNodes[WA,WL](this) {z => z.glinks.isEmpty} ) foreach (i => i +-> z)
//    this
//  }
// 
//  type N = WfAct
//  type NN = WA
//  
//  //----------------------- fix the return type for base functions
//  // if we remove this, you get some freaky type casts I don't fully understand
//    /** reroute */
//  override def --> (z:NN)(implicit linkFactory: LFactory) : N = 
//    super.-->(z).asInstanceOf[WfAct]
//  /** add a new dependency */
//  override def +-> ( z:NN)(implicit linkFactory: LFactory) : N = 
//    super.-->(z).asInstanceOf[WfAct]
//  /** par depy a -> (b,c) */
//  override def --> (z:Seq[NN])(implicit linkFactory: LFactory) : N =
//    super.-->(z).asInstanceOf[WfAct]
//  /** par depy a -> (b,c) */
//  override def +-> ( z:Seq[NN])(implicit linkFactory: LFactory) : N =
//    super.-->(z).asInstanceOf[WfAct]

//}

