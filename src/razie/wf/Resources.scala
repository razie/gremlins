/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.AA
import razie.base.{ActionContext => AC}
import razie.g._

/** a resource. Resources implement asynchronicity
 * 
 * <p>
 * A WfActivity can advertise that it needs a certain resource. The engine can then do some planning. 
 * The engine will acquire/release the resource FOR the action...the action doesn't have to
 * 
 * <ul>
 * <li> the WfActivity advertises that it needs the resource
 * <li> the engine will acquire it. the acquire is not blocking...the resource will notify the engine when it was acquired
 * <li> the action uses the resource: sends requests. These are non-blocking
 * <li> IF the resource says anything except RRWAIT, then action gets result and continues
 * <li> IF the resource says RRWAIT, then the current processing thread is suspended waiting for the resource
 * 
 * TODO need lifecycle management - I might end up re-implementing JCAA ??
 * TODO need transactionability - when to persist/rollback etc. should use context for that
 */
trait WRes extends GReferenceable {
  /** notify quit of whatever request outstanding - NOT a blocking call */
  def quit (who:WResUser, token:String)

  /** send a request - NOT a blocking call */
  def req (r:WResReq) : WResReqReply
}

case class WResReq      (who:WResUser, token:String, what:String, attrs:AC, value:Any)
  
     class WResReqReply (val who:WResUser, val token:String)
case class WResRROK     (wwho:WResUser, ttoken:String, result:Any) extends WResReqReply (wwho, ttoken)
case class WResRRERR    (wwho:WResUser, ttoken:String, err:Any) extends WResReqReply (wwho, ttoken)
case class WResRRWAIT   (wwho:WResUser, ttoken:String) extends WResReqReply (wwho, ttoken)

/** a resource user - notification API basically */
trait WResUser extends GReferenceable {
  /** a wait finished */
  def notifyReply (reply:WResReqReply)

  /** the resource screwed up - all outstanding requests were dismissed -
   * client should cleanup all requests pending on this resource... the resource itself can't
   */
  def notifyScrewup (who:WRes)
}

//-----------------

trait WfResState extends WfaState {
  var reply : Option[WResReqReply] = None
}

/** an activity on a resource - the only one that can wait */
case class WfResReq (res:GRef, what:String, attrs:AC, value:AExpr) extends WfActivity with HasDsl with WfResState { 
  override def traverse (in:AC, v:Any) : (Any,Seq[WfLink]) = (v,glinks.headOption.toList)
  
  def req (r: WResReq) = 
    AllResources resolve res map (_.req(r))
//    AllResources resolve res map (_.req(who, token, what, attrs, value(in,v))) 
  override def toString = "ResReq:" + res.meta+"."+what+" "+value
  override def toDsl = "ResReq(" + res.toString+", "+what+", "+value.toDsl+")"
}

/** follows a WfResReq and waits for a reply from the resource.
 * 
 * The default implementation here will simply remember the response value from resource and return it when 
 * finishes running. 
 * 
 * Strongly suggest you do not overload this. If you need to, override the reply method to do whatever you want
 */
class WfResReply extends WfActivity with HasDsl with WfResState { 
  override def traverse (in:AC, v:Any) : (Any,Seq[WfLink]) = (rreply.map(_.result) getOrElse v,glinks.headOption.toList)
  
  var rreply:Option[WResRROK] = None

  // TODO handle non-OK answers as well
  /** process the reply from the resource. Normally keeps state somewhere as it will be followed by traverse() as the workflow continues */
  def reply (r:WResReqReply) = rreply = Some(r.asInstanceOf[WResRROK])
     
  override def toString = this.toDsl
  override def toDsl  = "ResReply"
}

/** a special resReply that ignores the value from the resource and just propagates the input value
 */
class WfResReplyIgnore extends WfResReply { 
  override def traverse (in:AC, v:Any) : (Any,Seq[WfLink]) = (v,glinks.headOption.toList)
  
  override def toDsl  = "ResReplyIgnore"
}

object AllResources extends GResolver [WRes] {
  val resources = new scala.collection.mutable.HashMap[GRef, WRes]()

  def add (res : WRes) : WRes = synchronized { resources += (res.key -> res); res }
  def remove (key:GRef) = synchronized { resources remove key }
  def clear = resources.clear
  
  override def resolve (key : GRef) : Option[WRes] = synchronized {resources get key}
  def resolveOrCreate (key : GRef) (f: =>WRes) : Option[WRes] = synchronized {
    if (! (resources contains key))
       add (f)
    resources get key
    }
}
