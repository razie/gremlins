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
import razie.gremlins.lib.CSP
import razie.wfs
import razie.wf

/** wfs samples: a ping-pong match, implemented in a few variations */
class PingPongTest extends JUnit3Suite {
  // import the wfs instead of the wf to get the scala workflows
  import razie.wfs._

  // this is important if you start messing it up and try to debug it...
  razie.g.Graphs.maxDebugDepth = 50

  val PING = "ping"
  val PONG = "pong"

  /** the essence of a player: returns a ball or "done"
   *
   *  trick: the variable turns this into a closure, which is an object with state... look ma, like an Actor!
   */
  def player(name: String, maxBalls: Int): Any => Any = {
    var count = 0 // turns this into a closure 

    _ match {
      case msg: String => {
        razie.Debug ("~~~~~~~~~~~~~~~", name, count)
        count = count + 1
        if (msg == "done" || count == maxBalls) "done"
        else name
      }
      case m @ _ => "What is: " + m + " ?"
    }
  }

  // these have to be vals - remember they are closures with a counter inside
  val (ping, pong) = (player(PING, 10), player(PONG, 10))

  /** sequential version: a simple do-wile loop with a branch inside */
  val wseqLoopGame: razie.gremlins.WfActivity = seq {
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
    later { _ == "done" }
  }

  /** sequential version: a simple repeat-until loop with a branch inside */
  val rseqLoopGame: razie.gremlins.WfActivity = seq {
    repeatUntil (_ == "done") {
      seq {
        matchLater {
          // turn pong into a workflow w(pong) and label it, then print it and run it
          case PING => PONG label w(pong).print run PING
          case PONG => PING label w(ping).print run PONG
        }
      }
      //      seq { matchLater { case PONG => pong } }
    }
    later { _ == "done" }
  }

    def testSeqLoopGamew = expect (true) { wseqLoopGame printAndRun PING }
    def testSeqLoopGamer = expect (true) { rseqLoopGame printAndRun PING }

  // for the parallel version we need two communication channels and each listens to his own - LIKE two actors
  val pingChannel = CSP.Channel("pingChannel", 1)
  val pongChannel = CSP.Channel("pongChannel", 1)

  /** parallel version - we still need a loop to end the workflow */
  val parLoopGame = seq {
    par {
      actWhile (_ != "done") {
        pongChannel ! (pingChannel ? w(ping))
      }
      actWhile (_ != "done") {
        pingChannel ! (pongChannel ? w(pong))
      }
      pingChannel ! $0 // start the fun and games
    }
    later { _.asInstanceOf[List[_]] contains "done" }
  }

  def testParLoopGame = expect (true) { parLoopGame printAndRun PING }

  /** as even more scala code, less activities. The two seq{} are ran in parallel, in different threads.
   *  You could use your own communication queue rather than the pipe with activities, but this one
   *  work.
   */
  val sparLoopGame = seq {
    par {
      seq {
        var ball: Any = ""
        do {
          ball = scall { pongChannel ! (pingChannel ? w(ping)) }
        } while (ball != "done")
        later { x => ball }
      }

      seq {
        var ball: Any = ""
        do {
          ball = scall { pingChannel ! (pongChannel ? w(pong)) }
        } while (ball != "done")
        later { x => ball }
      }

      pingChannel ! $0 // start the fun and games
    }
    later { _.asInstanceOf[List[_]] contains "done" }
  }

  def testParLoopGames = expect (true) { sparLoopGame printAndRun PING }

  override def setUp() = { Gremlins.liveInside (new Engine with Threads) }
  override def tearDown() = { Gremlins.die() }
}
