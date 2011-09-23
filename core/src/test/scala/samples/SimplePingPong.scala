/** ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package samples

import org.scalatest.junit.JUnit3Suite

import razie.gremlins.eng.Engine
import razie.gremlins.eng.Threads
import razie.Gremlins

class SimplePingPongTest extends JUnit3Suite {
  // import the wfs instead of the wf to get the scala workflows
  import razie.wfs._

  val PING = "ping"
  val PONG = "pong"

  /** this looks like an actor */
  def player(name: String, maxBalls: Int): Any => Any = {
    var count = 0 // trick: turn this into a closure and keep the counter in there :)

    _ match {
      case msg: String => {
        razie.Debug ("~~~~~~~~~~~~~~~", name, count)
        count = count + 1
        if (msg == "done" || count == maxBalls) "done"
        else name
      }
      case m @ _ => "too early: " + m
    }
  }

  // these have to be vals - remember they are closures with a counter
  val (ping, pong) = (player(PING, 10), player(PONG, 10))

  // this is the only workfow construct here, turning the closures into sub-workflows
  val wflow: razie.gremlins.WfActivity = seq {
    doWhile (_ != "done") {
      seq {
        matchLater {
          // turn pong into a workflow w(pong) and label it, then print it and run it
          case PING => PONG label w(pong).print run PING
          case PONG => PING label w(ping).print run PONG
        }
      }
      //      seq { matchLater { case PONG => pong } }
    }
  }

  def test1 = expect ("done") { wflow printAndRun PING }

  override def setUp() = { Gremlins.liveInside (new Engine with Threads) }
  override def tearDown() = { Gremlins.die() }
}
