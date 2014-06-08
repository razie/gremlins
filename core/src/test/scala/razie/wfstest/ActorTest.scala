/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wfstest

import org.scalatest.junit._
import razie.gremlins.eng.{ Engine, Threads, Executors, Actors }
import razie.wfs
import razie.wfs._
import razie.Gremlins.using

class ActorTest extends JUnit3Suite {

  razie.g.Graphs.maxDebugDepth = 50

  def fapp(app: String)(in: Any): Any = {
    val x = in.toString + "-" + app;
    println("------------- woohoo " + x);
    x
  }

  // parallel map
  def wsmap1 =
    seq {
      wsmap[Int, Int](3) { x: Int => x + 1 }
    }
  def testthreads = expect(List(2, 3, 4)) {
    using(new Engine with Threads) { wfs strict wsmap1 printAndRun List(1, 2, 3) }
  }

  def testexecutors = expect(List(2, 3, 4)) {
    using(new Engine with Executors) { wfs strict wsmap1 printAndRun List(1, 2, 3) }
  }

  // make sure more than 1 actor was spawned to run this parmap
  def testactors = expect(List(2, 3, 4)) {
    val e = new Engine with Actors
    using(e) {
      val result = wfs strict wsmap1 printAndRun List(1, 2, 3)
      val m = e.asInstanceOf[Actors].maxActors
      println(m)
      assert(m > 2)
      result
    }
  }

  // multilevel - workflow built and then ran: 
  // the fapp is now invoked as a separate activity and others may cut in - note the sequence of messages is likely different
  def wsp4 =
    seq {
      par {
        seq {
          w { fapp("a") _ }
        }
        seq {
          w { fapp("b") _ }
        }
      }
    }
  def testwsp4 = expect(List("1-a", "1-b")) { 
    razie.M anyOrder {using(new Engine with Actors){wsp4 printAndRun 1}} 
    }

  override def setUp() = {}
  override def tearDown() = {}
 
}
