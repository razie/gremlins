/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins.res

import razie.AA
import razie.base.{ ActionContext => AC }
import razie.gremlins._
import razie.g._
import scala.collection.mutable.ListBuffer

/** simple queue implementation */
class ListQueue[T] {
  val q = new ListBuffer[T]()
  def push(t: T): T = { q append t; t }
  def pop: T = q remove (q.size - 1)
  def popOption: Option[T] = this removeOption (q.size - 1)
  def pull: T = q remove 0
  def pullOption: Option[T] = this removeOption 0
  def isEmpty = q.isEmpty
  def size = q.size

  def removeOption(i: Int): Option[T] = if (i < q.size) Some(q remove (i)) else None
  def remove(criteria: T => Boolean) {
    var t = q indexWhere criteria
    while (t > -1) {
      q remove t
      t = q indexWhere criteria
    }
  }
}

/** 
 * prod/cons or message queue implementation
 * 
 * If the maxSize is 0, then this is essentially a sync channel: the producers wait for a consumer
 * to be available
 * 
 * TODO 3-3 feature - maxSize together with flag: throw exception if queue full, NOWAIT
 */
class WQueue(val name: String, val maxSize: Int) extends WRes {
  override val key = GRef.id("WQueue", name)

  val values = new ListQueue[Any]()
  val consumers = new ListQueue[WResRRWAIT]()
  // producers is used when async, on top of values
  val producers = new ListQueue[WResRROK]()

  private[this] def push(a: Any) = { razie.Debug("WQueue.push " + a); values push a }
  private[this] def pull = { val a = values.pull; razie.Debug("WQueue.pull " + a); a }
  private[this] def pop = { val a = values.pop; razie.Debug("WQueue.pop " + a); a }

  /** notify quit of whatever request outstanding - NOT a blocking call */
  override def quit(who: WResUser, token: String) = throw new UnsupportedOperationException()

  /** notify quit of whatever request outstanding - NOT a blocking call */
  override def req(r: WRes.Req): WRes.ReqReply = synchronized {
    debug("pre  " + r)
    val ret = r.what match {
      case ProdCons.CLEAR => {
        this.clear
        WResRROK(r.who, r.token, r.value)
      }
      case ProdCons.PUT => {
        if (consumers.isEmpty && values.size >= maxSize) {
          // nobody listening and is sync - will wait
          producers push WResRROK(r.who, r.token, r.value)
          WResRRWAIT(r.who, r.token)
        } else {
          // queue value in buffer and continue
          this push r.value
          consumers.pullOption map (w => w.who notifyReply WResRROK(w.who, w.token, this.pull))
          WResRROK(r.who, r.token, r.value)
        }
      }
      case ProdCons.GET =>
        if (values.isEmpty && producers.isEmpty)
          consumers push WResRRWAIT(r.who, r.token)
        else {
          if (!producers.isEmpty) {
            val w = producers.pull
            w.who notifyReply w
            values push w.result
          }
          //        consumers.pullOption map (w => {
          //          w.who notifyReply WResRROK (w.who, w.token, this.pull)
          //          consumers push WResRRWAIT (r.who, r.token)
          //        }) getOrElse
          WResRROK(r.who, r.token, this.pop)
        }
      case s@_ => throw new IllegalArgumentException("WHAT?? " + s)
    }
    debug("post " + r)
    ret
  }

  def clear = synchronized {
    debug("Clearing queue")

    if (consumers.size > 0 || values.size > 0 || producers.size > 0)
      razie.Warn("WQueue not empty before clearing...")

    consumers.q.clear // TODO notify them
    producers.q.clear // TODO notify them
    values.q.clear
  }

  private def debug(msg: String) {
    razie.Debug("WQueue: " + this.toString + " - " + msg + " consumers:" + consumers.size +
      " values=" + values.size + " producers=" + producers.size)
  }
}

object ProdCons {
  val PUT = "put"
  val GET = "get"
  val CLEAR = "clear"
}
