package samples

import scala.collection.mutable.ListBuffer

/** wfs samples: a ping-pong match, implemented in a few variations */
object TempPingPongTest extends App {

  val PING = "ping"
  val PONG = "pong"

  /**
   * the essence of a player: returns a ball or "done"
   *
   *  trick: the variable turns this into a closure, which is an object with state... 
   */
  def player(name: String, maxBalls: Int): Any => Any = {
    var count = 0 // turns this into a closure 

    _ match {
      case msg: String => {
        println("~~~~~~~~~~~~~~~", name, count)
        count = count + 1
        if (msg == "done" || count == maxBalls) "done"
        else name
      }
      case m @ _ => "What is: " + m + " ?"
    }
  }

  // these have to be vals - remember they are closures with a counter inside
  val (ping, pong) = (player(PING, 10), player(PONG, 10))

  //============================== parallel versions

  /**
   * version 1a: scala version with actors
   *
   * The disadvantage of this approach is that it is a black box at runtime: you don't know where it is stuck...
   */
  import scala.actors.Actor
  import scala.actors.Actor._

  val ver1a = { input: String =>
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

  println ("result is: " + ver1a(PING))
}
