/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.res

import razie.AA
import razie.base.{ ActionContext => AC }
import razie.wf._
import razie.g._

object WTimer extends WTimer {
  val WAIT  = "put" // value is the time Long in millis
  val CLEAR = "clear"
  val CANCEL = "CANCEL" // value is the token to cancel

  lazy val gref = {
    AllResources add this
    GRef.id("WTimer", "theone")
  }
}

/** 
 * simple scheduler - will reply at the given time
 * 
 * TODO 1-1 feature persistency
 * TODO 3-1 nice expression for time
 */
class WTimer() extends WRes {
  override def key = WTimer.gref

  val waitlist = new ListQueue[WResRROK]()

  /** notify quit of whatever request outstanding - NOT a blocking call */
  override def quit(who: WResUser, token: String) = synchronized {
    waitlist dropWhile (_.token == token)
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
      case WTimer.WAIT => {
        if (System.currentTimeMillis > r.value.asInstanceOf[Long])
          WResRROK(r.who, r.token, r.value)
        else {
          waitlist push WResRROK(r.who, r.token, r.value)
          figureOutNextWakeTime
          WResRRWAIT(r.who, r.token)
        }
      }
      case WTimer.CANCEL => { 
        quit (null, r.value.asInstanceOf[String]) 
        WResRROK(r.who, r.token, r.value)
      }
      case s@_ => throw new IllegalArgumentException("WHAT?? " + s)
    }
    debug("post " + r)
    ret
  }

  var nextWakeTime = 0
  def figureOutNextWakeTime {
    val t = waitlist.q.min(Ordering fromLessThan (
      (a:WResRROK, b:WResRROK) => a.result.asInstanceOf[Long] < b.result.asInstanceOf[Long])).result.asInstanceOf[Long]
  }

  def tick () : Unit = synchronized {
    val ct = System.currentTimeMillis

    waitlist dropWhile { w =>
      // TODO nasty: reuse result for storage...
      if (ct > w.result.asInstanceOf[Long]) {
        w.who notifyReply w
        true
      } else false
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
    razie.Debug("WTimer: " + this.toString + " - " + msg + " waitlist:" +
      waitlist.size) }

  // my own thread that does the bidding... well, sleeping...
  val myThread = new Thread {
    setDaemon(true)
    override def run () : Unit = {
      try {
        if (nextWakeTime > System.currentTimeMillis)
          Thread.sleep (nextWakeTime - System.currentTimeMillis)
        else
          Thread.sleep (500)
        tick()
      } catch {
        case _ => ; // nothing
      }
    }
  }
}

