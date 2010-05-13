/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.lib

import razie.AA
import razie.base.{ActionContext => AC}
import razie.wf._
import razie.g._

class ListQueue[T] {
  val q = new scala.collection.mutable.ListBuffer[T]()
  def push  (t:T) : T = { q append t; t }
  def pop         : T = q remove (q.size-1)
  def popOption   : Option[T] = this removeOption (q.size-1)
  def pull        : T = q remove 0
  def pullOption  : Option[T] = this removeOption 0
  def isEmpty = q.isEmpty
  
  def removeOption (i:Int) : Option[T] = if (i < q.size) Some (q remove(i)) else None
}

/** prod/cons or message queue implementation */
class WQueue (val name:String) extends WRes {
  override val key = GRef.id ("WQueue", name)
  
  val values = new ListQueue[Any]()
  val consumers = new ListQueue[RRWAIT]()
   
  /** request acquire of resource - NOT a blocking call */
  override def acquire (who:WResUser, token:String) = RROK (who, token, null)
  /** notify release of resource - NOT a blocking call */
  override def release (who:WResUser, token:String) = RROK (who, token, null)
  
  /** notify quit of whatever request outstanding - NOT a blocking call */
  override def quit (who:WResUser, token:String) = throw new UnsupportedOperationException ()

  /** notify quit of whatever request outstanding - NOT a blocking call */
  override def req (who:WResUser, token:String, what:String, attrs:AC, value:Any) : ReqReply = what match {
    case ProdCons.PUT => {
       values push value
       consumers.pullOption map (w => w.who notifyReply RROK (w.who, w.token, values.pull))
       RROK (who, token, value)
    }
    case ProdCons.GET => 
      if (values.isEmpty)
        consumers push RRWAIT (who, token)
      else 
        consumers.pullOption map (w => {
          w.who notifyReply RROK (w.who, w.token, values.pull)
          consumers push RRWAIT (who, token)
        }) getOrElse
          RROK (who, token, values.pop)
  }
}

object ProdCons {
  val PUT = "put"
  val GET = "get"
     
///** a resource user - notification API basically */
//trait WResUser extends GReferenceable {
//  /** an acquire finished */
//  def notifyAcquited (reply:WRes#ReqReply)
//  
//  /** a wait finished */
//  def notifyReply (reply:WRes#ReqReply)
//
//  /** the resource screwed up - all outstanding requests were dismissed */
//  def notifyScrewup (who:WRes)
//}

}
