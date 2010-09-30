/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wfs

import org.scalatest.junit._
import razie.wf.eng.{ Engine, Threads, Actors }

class PerfScalaWorkflowTest extends JUnit3Suite {
  // import the wfs instead of the wf to get the scala workflows
  import wfs._
  import razie.wf.wf

  val noflows = 10000
  val delay = 100

  class Counter(val ceiling: Int) {
    var c = 0
    def inc = synchronized { c = c + 1 }
    def done = synchronized { c >= ceiling }
  }

  val counter = new Counter(noflows)

  def wpp1 = seq {
    wf.sleepAsync (delay)
    w { counter.inc }
  }

  def testwss2 = expect (true) {
    val time = razie.Timer {
      
      for (i <- 1 to noflows)
        wpp1.start(1)

      while (!counter.done)
        Thread.sleep (200)

    }._1
    println ("!!!!!!!!!!!!!!!!!!!!!!!!!! done in " + time)
    time < noflows * delay / 5
  }

  override def setUp() = {
//    com.razie.pub.base.log.Log.DEBUGGING = false
    razie.Log.impl = new REALLYSILENTLOG

    com.razie.pub.base.log.Log.SILENT = true
    razie.wf.Gremlins.liveInside (new Engine with Threads)
  }
  override def tearDown() = { Thread.sleep(noflows/7); razie.wf.Gremlins.die }
}

class REALLYSILENTLOG extends razie.Log {
  private def th = Thread.currentThread.getName + " "
  override def trace(f: => Any) = {}
  override def log(msg: String, t: Throwable = null) = {}
  override def alarm(msg: String, t: Throwable = null) = println ("ALARM: " + th + msg, t)
  override def audit(msg: String, t: Throwable = null) = {}
  override def error(msg: String, t: Throwable = null) = { println("ERROR: " + th + msg, t); throw t }
}

