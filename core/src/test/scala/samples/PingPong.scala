/** ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package samples

import org.scalatest.junit._
import razie.gremlins.eng.{ Engine, Threads }
import razie.Gremlins
import scala.util.Random
import razie.gremlins.lib.CSP
import razie.gremlins.lib.WfChannel

class PingPongTest extends JUnit3Suite {
  // import the wfs instead of the wf to get the scala workflows
  import razie.wfs
  import razie.wfs._

  def say(name: String, msg: String) {

  }

  /** this is essentially an actor */
  class Player(val name: String) {
    var partner: Option[WfChannel] = None
    val in = CSP.Channel()
    val rand = new Random()
    val done = false

    def play() = seq {
      val shouldFail = rand.nextBoolean()

      matchLater {
        case "serve" => par {
          seq { say (name, "") }
          seq { partner map (_ ! "ball") }
        }
        case "ball" =>
          par {
            seq {

            }
          }
//        case "done" => done = true
      }
    }
  }

  //  def testwsp1 = expect (List("1-a", "1-b")) { razie.M anyOrder prun (wsp1, 1) }

  override def setUp() = { Gremlins.liveInside (new Engine with Threads) }
  override def tearDown() = { Gremlins.die() }
}
