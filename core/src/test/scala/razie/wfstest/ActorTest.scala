/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wfstest

import org.scalatest.junit._
import razie.gremlins.eng.{ Engine, Threads, Executors, Actors }

class ActorTestMain extends App {
  val t = new LoopTest
  t.setUp()
  //  t.testwif1
  t.tearDown()
}

class ActorTest extends JUnit3Suite {
  // import the wfs instead of the wf to get the scala workflows
  import razie.wfs
  import razie.wfs._

  razie.g.Graphs.maxDebugDepth = 50

  def fapp(app: String)(in: Any): Any = {
    val x = in.toString + "-" + app;
    println ("------------- woohoo " + x);
    x
  }

  // parallel map
  def wsmap1 =
    seq {
      wsmap[Int, Int] (3) { x: Int => x + 1 }
    }
  def testthreads = expect (List(2, 3, 4)) {
    using (new Engine with Threads) { wfs strict wsmap1 printAndRun List(1, 2, 3) }
  }

  def testexecutors = expect (List(2, 3, 4)) {
    using (new Engine with Executors) { wfs strict wsmap1 printAndRun List(1, 2, 3) }
  }

  def testactors = expect (List(2, 3, 4)) {
    val e = new Engine with Actors
    using (e) {
      val result = wfs strict wsmap1 printAndRun List(1, 2, 3)
      val m = e.asInstanceOf[Actors].maxActors
      println (m)
      assert (m > 2)
      result
    }
  }

  // multilevel - workflow built and then ran: 
  // the fapp is now invoked as a separate activity and others may cut in - note the sequence of messages is likely different
  def wsp4 =
    seq {
      println ("------------------woohoo start")
      par {
        seq {
          println ("------------------woohoo build a")
          w { fapp("a") _ }
        }
        seq {
          println ("------------------woohoo build b")
          w { fapp("b") _ }
        }
      }
    }
  //  def testwsp4 = expect (List("1-a", "1-b")) { razie.M anyOrder prun (wsp4, 1) }

  def using(e: Engine)(work: => Any) = {
    razie.Gremlins.liveInside (e)
    val ret = work
    razie.Gremlins.die()
    ret
  }

  override def setUp() = {}
  override def tearDown() = {}

}
