/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.eng

import razie.{ AA, Debug, Log }
import razie.base.{ ActionContext => AC }
import razie.g._
import scala.actors._
import com.razie.pubstage.life._
import razie.wf._
import razie.wf.act._

// and-join state: track incoming links and their values
trait AJState extends WfaState {

  var prev: scala.collection.mutable.HashMap[WfLink, Any] = null

  def addLink(l: WfLink, v: Any) = {
    if (prev == null)
      prev = razie.Mapi[WfLink, Any]()
    prev.put(l, v)
  }
}

/** join parallel branches - accumulate the values from the branches that end */
case class AndJoin() extends WfActivity with AJState with SpecOps {
  var incoming: List[WfLink] = Nil // upon starting, the engine will traverse and populate this
  def addIncoming(l: WfLink) = incoming = List(l) ::: incoming

  /** waiting for all incoming branches and concatenating their values */
  override def traverse(in: AC, v: Any): (Any, Seq[WfLink]) =
    throw new IllegalStateException("method should never be called")

  override def traverse(from: Option[WfLink], in: AC, v: Any): (Any, Seq[WfLink]) = this.synchronized {
    addLink(from.get, v) // TODO null should bomb indicating andjoin with no branches in...
    Debug("AndJoin " + " waitingFor=" + incoming.size+ "alreadyHere=" + prev.size )
    if (prev.size == incoming.size) (prev.values.map(x => x).toList, glinks) // I'm done
    else (v, Nil) // current thread dies
  }

  def isComplete = prev.size == incoming.size

  override def toString = "AndJoin " + incoming.size + "(" + key + ")"
  //  override def toDsl = throw new UnsupportedOperationException()
}
