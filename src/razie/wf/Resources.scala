/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.AA
import razie.base.{ActionContext => AC}
import razie.g._

/** a resource. Resources have two things: wait/sync and acquire 
 * 
 * <p>
 * A WfAct can advertise that it needs a certain resource. The engine can then do some planning. 
 * The engine will acquire/release the resource FOR the action...the action doesn't have to
 * 
 * <ul>
 * <li> the WfAct advertises that it needs the resource
 * <li> the engine will acquire it. the acquire is not blocking...the resource will notify the engine when it was acquired
 * <li> the action uses the resource: sends requests. These are non-blocking
 * <li> IF the resource says anything except RRWAIT, then action gets result and continues
 * <li> IF the resource says RRWAIT, then the current processing thread is suspended waiting for the resource
 * 
 * TODO need lifecycle management - I might end up re-implementing JCAA ??
 */
trait WRes extends GReferenceable {
  /** request acquire of resource - NOT a blocking call */
  def acquire (who:WResUser, token:String)
  /** notify release of resource - NOT a blocking call */
  def release (who:WResUser, token:String)
  
  /** notify quit of whatever request outstanding - NOT a blocking call */
  def quit (who:WResUser, token:String)

  /** notify quit of whatever request outstanding - NOT a blocking call */
  def req (who:WResUser, token:String, what:String, attrs:AC, value:Any) : ReqReply

  case class ReqReply (who:WResUser, token:String)
  case class RROK     (wwho:WResUser, ttoken:String, result:Any) extends ReqReply (wwho, ttoken)
  case class RRERR    (wwho:WResUser, ttoken:String, err:Any) extends ReqReply (wwho, ttoken)
  case class RRWAIT   (wwho:WResUser, ttoken:String) extends ReqReply (wwho, ttoken)
}

/** a resource user - notification API basically */
trait WResUser extends GReferenceable {
  /** an acquire finished */
  def notifyAcquired (reply:WRes#ReqReply)
  
  /** a wait finished */
  def notifyReply (reply:WRes#ReqReply)

  /** the resource screwed up - all outstanding requests were dismissed */
  def notifyScrewup (who:WRes)
}

//-----------------

trait WfResState extends WfaState {
  var reply : Option[WRes#ReqReply] = None
}

/** an activity on a resource - the only one that can wait */
case class WfResAct (res:GRef, what:String, attrs:AC, value:XExpr) extends WfAct with WfResState { 
  override def traverse (in:AC, v:Any) : (Any,Seq[WL]) = 
     throw new IllegalStateException ("method should never be called")
  
//  def req (who:WResUser, token:String, in:AC, v:Any) =
//    reply = AllResources resolve res map (_.req(who, token, what, attrs, value(in,v))) 
}

object AllResources extends GResolver [WRes] {
  val resources = new scala.collection.mutable.HashMap[GRef, WRes]()

  def add (res : WRes) : WRes = { resources += (res.key -> res); res }
  def remove (key:GRef) { resources remove key }
  
  override def resolve (key : GRef) : Option[WRes] = resources get key
}


