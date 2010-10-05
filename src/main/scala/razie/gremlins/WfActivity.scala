/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins

import razie.AA
import razie.base.{ ActionContext => AC }
import razie.g._
import razie.{wf, gremlins}
import razie.Gremlins

/** 
 * This is the basic node in the graph: an activity waiting to be traversed/executed. 
 * 
 * The workflow is modelled as a graph of activities connected by links/dependencies.
 * 
 * Mixing in the state also allows its removal, should I decide to store it outside, later...cool, huh?
 */
abstract class WfActivity extends razie.g.GNode[WfActivity, WfLink] with act.WfaState with razie.g.WRGraph[WfActivity, WfLink] {
  //  override var gnodes : Seq[WfActivity] = Nil // next activities - note that this is not containment, is it?
  override var glinks: Seq[WfLink] = Nil // links 

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
  def traverse(in: AC, v: Any): (Any, Seq[WfLink])
  def traverse(from: Option[WfLink], in: AC, v: Any): (Any, Seq[WfLink]) = traverse (in, v)

  override def toString: String = this.getClass().getSimpleName + "(" + key + ")"

  // syntax niceties 
  def +(e: WfActivity): WfActivity = new gremlins.act.WfSeq(this, e)
  def |(e: WfActivity): WfActivity = new gremlins.act.WfPar(this, e)
  //  def | (e:Seq[WfActivity]) : WfActivity = WfPar ((this :: e.toList):_*)

  def print(): WfActivity = { println (this mkString); this }
  def debug(): WfActivity = { wf toDsl this; print }

  // simplified execution
  def run(initialValue: Any): Any = Gremlins().exec(this, initialValue)
  def start(initialValue: Any): Any = Gremlins().start(this, initialValue)

  implicit val linkFactory: LFactory = (x, y) => new WfLink(x, y) // for the nice inherited --> operators 

  // -------------- specific to WF? could move down?

  /** bound: point all leafs to z, an end node, while avoiding z --> z */
  def --|(z: WfActivity)(implicit linkFactory: LFactory) = {
    (razie.g.Graphs.filterNodes[WfActivity, WfLink](this) { a => a.glinks.isEmpty && a != z }) foreach (i => i +-> z)
    this
  }

  def <--(z: WfActivity): WfActivity = z --> this
  def <-+(z: WfActivity): WfActivity = z +-> this

  /** this will bind it to a parent in a DSL construct. Use this very carefully. 
   * Find usages by seeing who pushes self into WfaCollector */
  WfaCollector.current.map { _ collect this }
}

case class WfaCollector(who:Any, collect: WfActivity => Unit)

/** if you need to collect children, push yourself here and pop after xscope defined */
object WfaCollector {
  val curr = new java.lang.ThreadLocal[List[WfaCollector]] {
    override def initialValue() = List[WfaCollector]()
  }
  val flags = new java.lang.ThreadLocal[collection.mutable.Map[String,Boolean]] {
    override def initialValue() = collection.mutable.HashMap[String,Boolean]()
  }

  private[this] def push(who:Any, a: WfActivity => Unit) { curr set WfaCollector(who, a) :: currents }
  private[this] def pop() { curr.set(currents.drop(1)) }
  def currents = curr.get.asInstanceOf[List[WfaCollector]]
  def current = currents.headOption
  def who = current map (_.who)
  def debug(msg: String) = razie.Debug(">>>>>>>>>" + msg + curr.get)

  def collect[T](col: WfActivity => Unit)(who:Any)(f: => T): T = {
    push (who, col )
    val ret = try {
      f
    } finally {
      pop()
    }
    ret
  }

  def noCollect[T](f: => T): T = collect ({ x => {} }) (this) (f)
  def cantCollect[T](block:String) (f: => T): T = collect ({ x => {
    throw new IllegalStateException ("CAN'T define new activities in this block: " + block + " !!!")
    } }) (this) (f)
  def flagged[T] (name:String) (f: => T): T = collect ({ x => {} }) (name) (f)
  def isFlag (name:String) : Option[Boolean] = currents.find(_.who match { 
    case s:String if (s==name) => true
    case _ => false
    }).map (_ => true)
}

/** may want to store some color here...or have the link do something */
class WfLink(val a: WfActivity, val z: WfActivity) extends razie.g.GLink[WfActivity] with act.WflState

/** special link with a selector value. it assists the parent in deciding where to go next */
class WfLinkM(aa: WfActivity, zz: WfActivity, val selector: WFunc[Boolean]) extends WfLink(aa, zz)

/** special link with a selector value. it assists the parent in deciding where to go next */
//case class WfLinkV (aaa:WfActivity, zzz:WfActivity, val aselector:Any) extends WfLinkM (aaa,zzz, WFCMP(aselector))
class WfLinkV(aaa: WfActivity, zzz: WfActivity, val selector: Any) extends WfLink(aaa, zzz)

