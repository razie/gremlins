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

object WTimer extends WTimer {
  val WAIT = "wait" // value is the time Long in millis
  val WAITREL = "waitrel" // value is the time Long in millis
  val CLEAR = "clear"
  val CANCEL = "cancel" // value is the token to cancel

  AllResources add this

  lazy val gref = GRef.id("WTimer", "theone")
}

/** 
 * simple scheduler - will reply at the given time
 * 
 * TODO 1-1 feature persistency
 * TODO 3-1 nice expression for time
 */
class WTimer extends WRes with razie.Logging {
  override def key = WTimer.gref

  val waitlist = new ListQueue[WResRROK]()

  // my own thread that does the bidding... well, sleeping...
  val myThread = new Thread {
    setDaemon(true)
    override def run(): Unit = {
      while (true) try {
        if (nextWakeTime > System.currentTimeMillis) {
          debug ("sleeping for " + (nextWakeTime - System.currentTimeMillis))
          Thread.sleep (nextWakeTime - System.currentTimeMillis)
        } else {
          Thread.sleep (500)
        }
        tick()
      } catch {
        case _ => ; // nothing
      }
    }
  }

  myThread.start()

  /** notify quit of whatever request outstanding - NOT a blocking call */
  override def quit(who: WResUser, token: String) = synchronized {
    try {
      waitlist remove { w =>
        if (token == w.token) {
          w.who notifyReply w
          true
        } else false
      }
    } catch {
      case e@_ => log("while notifying requestor of reply: ", e)
    }
    figureOutNextWakeTime
  }

  /** notify quit of whatever request outstanding - NOT a blocking call */
  override def req(r: WRes.Req): WRes.ReqReply = synchronized {
    debug("pre  " + r)
    val ret = r.what match {
      case WTimer.CLEAR => {
        this.clear
        WResRROK(r.who, r.token, r.value)
      }
      case WTimer.WAITREL => waiting(r, System.currentTimeMillis + r.value.asInstanceOf[Long])
      case WTimer.WAIT => waiting (r, r.value.asInstanceOf[Long])
      case WTimer.CANCEL => {
        quit (null, r.value.asInstanceOf[String])
        WResRROK(r.who, r.token, r.value)
      }
      case s@_ => throw new IllegalArgumentException("WHAT?? " + s)
    }

    debug("post " + r)
    ret
  }

  def waiting(r: WRes.Req, mi: Long) = {
    debug ("????????? " + mi)
    if (System.currentTimeMillis > mi)
      WResRROK(r.who, r.token, mi)
    else {
      waitlist push WResRROK(r.who, r.token, mi)
      figureOutNextWakeTime
      myThread.interrupt()
      WResRRWAIT(r.who, r.token)
    }
  }

  var nextWakeTime = 0L

  def figureOutNextWakeTime {
    nextWakeTime = if (waitlist.q.isEmpty) 0L else
      waitlist.q.min(Ordering fromLessThan ((a: WResRROK, b: WResRROK) => a.result.asInstanceOf[Long] < b.result.asInstanceOf[Long])).result.asInstanceOf[Long]
  }

  def tick(): Unit = synchronized {
    val ct = System.currentTimeMillis

    try {
      waitlist remove { w =>
        if (ct > w.result.asInstanceOf[Long]) {
          w.who notifyReply w
          true
        } else false
      }
    } catch {
      case e@_ => log("while notifying requestor of reply: ", e)
    }

    figureOutNextWakeTime
  }

  def clear = synchronized {
    debug("Clearing timers")

    if (waitlist.size > 0)
      razie.Warn("WTimer not empty before clearing...")

    waitlist.q.clear // TODO notify them
  }

  private def debug(msg: String) {
    razie.Debug("WTimer: " + this.toString + " - " + msg + " waitlist:" + waitlist.size + " now = " + System.currentTimeMillis)
  }

}

