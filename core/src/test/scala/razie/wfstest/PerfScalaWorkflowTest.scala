/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wfstest

import org.scalatest.junit._
import razie.gremlins.eng.{ Engine, Threads, Executors, Actors }

class PerfScalaWorkflowTest extends JUnit3Suite {

  val noflows = 200
  val delay = 100

  class Counter(val ceiling: Int) {
    var c = 0
    def inc = synchronized { c = c + 1 }
    def done = synchronized { c >= ceiling }
  }

  val counter = new Counter(noflows)

  // import the wfs instead of the wf to get the scala workflows
  import razie.wfs._
  import razie.wf       // can mix activities in a scala wd
  
  def wpp1 = seq {
    wf.sleepAsync (delay)
    w { counter.inc }
  }

  def doit() = {
    val time = razie.Timer {
      
      for (i <- 1 to noflows)
        wpp1.start(1)

      while (!counter.done)
        Thread.sleep (200)

    }._1
    println ("!!!!!!!!!!!!!!!!!!!!!!!!!! done in " + time)
//    time < noflows * delay / 5
    time
  }

  def testit = expect (true) {
    var results : List[String] = Nil
    
    //warmup
    using (new Engine with Threads) {
      doit
    }
    using (new Engine with Threads) {
      results = ("With Threads in " + doit()) :: results
    }
    using (new Engine with Executors) {
      results = ("With Executors in " + doit()) :: results
    }
    using (new Engine with Actors) {
      results = ("With Actors in " + doit()) :: results
    }
    results.reverse foreach (println _)
    true
  }
  
  def using(e: Engine)(work: => Any) = {
    razie.Gremlins.liveInside (e)
    val ret = work
//    Thread.sleep(1000 + noflows/3); 
    razie.Gremlins.die (2000)
    ret
  }

  override def setUp() = {
//    com.razie.pub.base.log.Log.DEBUGGING = false
//    razie.Log.impl = new REALLYSILENTLOG

    razie.Log.silent(true)
  }
}

class REALLYSILENTLOG extends razie.Log {
  private def th = Thread.currentThread.getName + " "
  override def trace(f: => Any) = {}
  override def log(msg: String, t: Throwable = null) = {}
  override def alarm(msg: String, t: Throwable = null) = println ("ALARM: " + th + msg, t)
  override def audit(msg: String, t: Throwable = null) = {}
  override def error(msg: String, t: Throwable = null) = { println("ERROR: " + th + msg, t); throw t }
  override def silent (f:Boolean) {}
}

