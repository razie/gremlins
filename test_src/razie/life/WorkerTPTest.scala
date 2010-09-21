/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.life

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._
import com.razie.pub.base.ExecutionContext

//import razie.wf._
import razie.wf._

/** PI examples */
class WorkerTPTest extends JUnit3Suite {
  import com.razie.pubstage.life._

  val ai = razie.AI ("testing")

  val mtp = new WorkerTP (5)

  def testmyp11 = expect (true) {
    var counter = new Object { var c = 0; def inc { c += 1 } }

    val t = razie.Timer {
      (Array.fill(2) {
        new Worker(ai, ExecutionContext.DFLT_CTX) {
          override def process() {
            Thread.sleep(1000)
            counter.synchronized { counter.inc }
          }
        }
      }) foreach (mtp.put(_))

      var continue = true
      while (continue) {
        counter.synchronized { continue = counter.c < 2 }
        Thread.sleep (50)
      }
      
    }._1 

    println (">>>>>>>>>t: "+t)
    t < 1500 && t > 1000
  }

}
