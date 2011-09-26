/**
 * ____    __    ____  ____  ____,,___     ____  __  __  ____
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
import scala.collection.mutable.ListBuffer

/** wfs samples: a ping-pong match, implemented in a few variations */
class PingPongTest extends JUnit3Suite {
  // import the wfs instead of the wf to get the scala workflows
  import razie.wfs._

  // this is important if you start messing it up and try to debug it...
  razie.g.Graphs.maxDebugDepth = 50

  val PING = "ping"
  val PONG = "pong"

  /**
   * the essence of a player: returns a ball or "done"
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

  // prime the wfs framework
  def testPrime = expect ("primed") { wf.log("primed") run 1 }

  /**
   * version 1: sequential scala version: a simple do-wile loop with a branch inside
   *
   * this is the fastest implementation but it really has noting to do with threads, actors :)
   *
   * The disadvantage of this approach is that it is a black box at runtime: you don't know where it is stuck...
   */
  val ver1 = { input: Any =>
    val (ping, pong) = (player(PING, 10), player(PONG, 10))
    var $0 = input
    while ($0 != "done") {
      $0 = $0 match {
        case PING => ping ($0)
        case PONG => pong ($0)
      }
    }
    $0 == "done"
  }
  def testver1 = expect (true) { ver1(PING) }

  /**
   * version 2: simple sequential workflow : a simple do-wile loop with a branch inside
   *
   * Note that here, the contents of matchLater are still a blackbox - basically you can choose the level of granularity
   */
  val ver2: razie.gremlins.WfActivity = seq {
    val (ping, pong) = (player(PING, 10), player(PONG, 10))
    doWhile (_ != "done") {
      seq {
        matchLater {
          case PING => pong(PING)
          case PONG => ping(PONG)
        }
      }
      //      seq { matchLater { case PONG => pong } }
    }
    later { _ == "done" }
  }
  def testver2 = expect (true) { ver2 printAndRun PING }

  /**
   * version 3: same as 2 but with visibility into the match/case
   *
   * Note the difference between the auto-collecting wfs activities and the regular wf builders, which need +
   * Also, because the wf. builders are not smart about collecting, you need to wrap them with resultOf
   */
  val ver3: razie.gremlins.WfActivity = seq {
    val (ping, pong) = (player(PING, 10), player(PONG, 10))
    doWhile (_ != "done") {
      seq {
        resultOf {
          wf.wmatch ($0) (
            wf.wcase (PING) { PONG label w(pong) } +
              wf.wcase (PONG) { PING label w(ping) })
        }
      }
      //      seq { matchLater { case PONG => pong } }
    }
    later { _ == "done" }
  }
  def testver3 = expect (true) { ver3 printAndRun PING }

  /**
   * Version 4: sequential version: a simple repeat-until loop with a branch inside
   *
   * Note the reversed condition inside repeatUntil
   */
  val ver4: razie.gremlins.WfActivity = seq {
    val (ping, pong) = (player(PING, 10), player(PONG, 10))
    repeatUntil (_ == "done") {
      seq {
        resultOf {
          wf.wmatch ($0) (
            wf.wcase (PING) { PONG label w(pong) } +
              wf.wcase (PONG) { PING label w(ping) })
        }
      }
      //      seq { matchLater { case PONG => pong } }
    }
    later { _ == "done" }
  }
  def testver4 = expect (true) { ver4 printAndRun PING }

  //============================== parallel versions

  /**
   * version 1a: scala version with actors
   *
   * The disadvantage of this approach is that it is a black box at runtime: you don't know where it is stuck...
   */
  val ver1a = { input: String =>
    import scala.actors.Actor
    import scala.actors.Actor._

    val (ping, pong) = (player(PING, 10), player(PONG, 10))

    val supervisor = new Actor { // supervisor to wait for the end of the game
      override def act() = {
        receive {
          case _ => {
            println(1)
            receive { case "done" => {} }
            println(2)
            receive { case "done" => {} }
            println(3)
            reply ("done")
          }
        }
      }
    }
    supervisor.start

    def aplayer(other: ListBuffer[Actor], player: Any => Any) = actor {
      loop {
        react {
          case msg @ _ => {
            val ball = player(msg)
            other map (_ ! ball) // pass the ball
            if (ball == "done") {
              supervisor ! "done";
              exit ("done")
            }
          }
        }
      }
    }

    var aping = new ListBuffer[Actor]()
    var apong = new ListBuffer[Actor]()

    aping += aplayer (apong, ping)
    apong += aplayer (aping, pong)

    aping.map (_ ! PING) // start the fun and games 
    "done" == (supervisor !? "whatever") // wait for the end of the game
  }

  def testver1a = expect (true) { ver1a(PING) }

  // for the parallel version we need two communication channels and each listens to his own - LIKE two actors
  val pingChannel = CSP.Channel("pingChannel", 1)
  val pongChannel = CSP.Channel("pongChannel", 1)

  /** Version 1p: parallel workflow version - we still need a loop to end the workflow */
  val ver1p = seq {
    val (ping, pong) = (player(PING, 10), player(PONG, 10))
    par {
      loopWhile (_ != "done") {
        pongChannel ! (pingChannel ? w(ping))
      }
      loopWhile (_ != "done") {
        pingChannel ! (pongChannel ? w(pong))
      }
      pingChannel ! $0 // start the fun and games
    }
    later { _.asInstanceOf[List[_]] contains "done" }
  }

  def testver1p = expect (true) { ver1p printAndRun PING }

  /**
   * Version 2p: parallel, with even more scala code, less activities. The two seq{} are ran in parallel, in different threads.
   *  You could use your own communication queue rather than the pipe with activities, but this one
   *  work.
   */
  val ver2p = seq {
    val (ping, pong) = (player(PING, 10), player(PONG, 10))
    pingChannel.clear
    pongChannel.clear
    par {
      seq {
        var ball: Any = ""
        while (ball != "done") {
          ball = scall { pongChannel ! (pingChannel ? w(ping)) }
        }
        later { x => ball }
      }

      seq {
        var ball: Any = ""
        while (ball != "done") {
          ball = scall { pingChannel ! (pongChannel ? w(pong)) }
        }
        later { x => ball }
      }

      pingChannel ! $0 // start the fun and games
    }
    later { _.asInstanceOf[List[_]] contains "done" }
  }

  def testver2p = expect (true) { ver2p printAndRun PING }

  override def setUp() = { Gremlins.liveInside (new Engine with Threads) }
  override def tearDown() = { Gremlins.die() }
}
