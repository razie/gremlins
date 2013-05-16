/** ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wfstest

import org.scalatest.junit._
import razie.gremlins.eng.{ Engine, Threads }
import razie.Gremlins
import razie.base.scripting.ScalaScriptContext
import razie.base.scripting.ScalaScript

class OtherWfTest extends JUnit3Suite {
  // import the wfs instead of the wf to get the scala workflows
  import razie.wfs
  import razie.wfs._

  import razie.wfs._
  import razie.gremlins.eng.{ Engine, Threads, Executors, Actors }

  val threads = new collection.mutable.HashSet[String]()

  def wf1 =
    par {
      for (i <- 0 until 3)
        seq {
          threads.synchronized { threads += Thread.currentThread.getName }
        }
    }

  def testwf1 = expect (3) { wf1.print run 1; threads.size }
  def testwf1s = expect (1) { wfs.strict(wf1).print run 1; threads.size }

  def scr = """
import razie.wfs._
import razie.gremlins.eng.{ Engine, Threads, Executors, Actors }
    
def wf = 
    seq {
    val threads = new collection.mutable.HashSet[String]()
par {
    for (i <- 0 until 3) 
    seq {
        threads.synchronized { threads += Thread.currentThread.getName }
    }
}
    //valueOf {threads.size}
    $0 = threads.size
}

wf run 1 
"""

  def testscr = expect (3) {
    val ctx = ScalaScriptContext()
    ScalaScript(scr).eval(ctx) getOrElse "?"
  }

  override def setUp() = { Gremlins.liveInside (new Engine with Threads) }
  override def tearDown() = { Gremlins.die() }
}
