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
  def size = q.size
  
  def removeOption (i:Int) : Option[T] = if (i < q.size) Some (q remove(i)) else None
}

/** prod/cons or message queue implementation */
class WQueue (val name:String) extends WRes {
  override val key = GRef.id ("WQueue", name)
  
  val values = new ListQueue[Any]()
  val consumers = new ListQueue[WResRRWAIT]()
   
  /** request acquire of resource - NOT a blocking call */
  override def acquire (who:WResUser, token:String) = WResRROK (who, token, null)
  /** notify release of resource - NOT a blocking call */
  override def release (who:WResUser, token:String) = WResRROK (who, token, null)
  
  /** notify quit of whatever request outstanding - NOT a blocking call */
  override def quit (who:WResUser, token:String) = throw new UnsupportedOperationException ()

  def push (a:Any) = { razie.Debug ("WQueue.push " + a); values push a}
  def pull  = { val a = values.pull; razie.Debug ("WQueue.pull " + a); a}
  def pop  = { val a = values.pop; razie.Debug ("WQueue.pop " + a); a}
  
  /** notify quit of whatever request outstanding - NOT a blocking call */
  override def req (r:WResReq) : WResReqReply = synchronized { 
     debug ("pre "+r)
     val ret = r.what match {
    case ProdCons.CLEAR => {
      consumers.q.clear 
      values.q.clear 
      WResRROK (r.who, r.token, r.value)
    }
    case ProdCons.PUT => {
       this push r.value
       consumers.pullOption map (w => w.who notifyReply WResRROK (w.who, w.token, this.pull))
       WResRROK (r.who, r.token, r.value)
    }
    case ProdCons.GET => 
      if (values.isEmpty)
        consumers push WResRRWAIT (r.who, r.token)
      else 
        consumers.pullOption map (w => {
          w.who notifyReply WResRROK (w.who, w.token, this.pull)
          consumers push WResRRWAIT (r.who, r.token)
        }) getOrElse
          WResRROK (r.who, r.token, this.pop)
    case s@_ => throw new IllegalArgumentException ("WHAT?? " + s)
  }
     debug ("post "+r)
     ret
     }
  
  def clear = synchronized { consumers.q .clear(); values.q .clear }
  def debug (msg:String) { razie.Debug ("WQueue: " + this.toString + " - "+msg+" consumers:"+consumers.size + " values="+values.size) }
}

object ProdCons {
  val PUT = "put"
  val GET = "get"
  val CLEAR = "clear"
}
