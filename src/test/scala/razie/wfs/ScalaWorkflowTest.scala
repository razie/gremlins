/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wfs

import org.scalatest.junit._
import razie.wf.eng.{Engine, Threads}

class ScalaWorkflowTest extends JUnit3Suite {
  // import the wfs instead of the wf to get the scala workflows
  import wfs._

  // some functions to simplify the workflows
  def finc(in: Any): Any = {
    val x = in.asInstanceOf[Int] + 1;
    println ("------------- woohoo " + x);
    x
  }

  def fapp(app: String)(in: Any): Any = {
    val x = in.toString + "-" + app;
    println ("------------- woohoo " + x);
    x
  }

  val wss2 = seq { finc _ }
  //  val wss3 = seq { case i:Int => { println ("int") ; i+1} }

  val wss4 = seqf (finc)

  // sequence
  val wss8 = seq {
    println ("------------------woohoo start")
    seq {
      println ("------------------woohoo build a")
      fapp ("a") _
    }
    println ("------------------woohoo between")
    seq {
      println ("------------------woohoo build b")
      fapp ("b") _
    }
    println ("------------------woohoo end")
  }

  // preserving types
  val wss9 = seq {
    println ("------------------woohoo start")
    seq {
      finc _
    }
    println ("------------------woohoo between")
    seq {
      finc _
    }
    println ("------------------woohoo end")
  }

  // simplify running the workflow
  def prun(p: razie.wf.WfActivity, s: Any) = {
    val out = p.print run 1;
    p.print;
    out
  }

  def testwss2 = expect (2) { prun (wss2, 1) }
  //    def testwss3 = expect (1) { prun (wss3, 1) }
  def testwss4 = expect (2) { prun (wss4, 1) }

  def testwss8 = expect ("1-a-b") { prun (wss8, 1) }
  def testwss9 = expect (3) { wss9.print run 1 }

  // parallel
  val wsp1 =
    par {
      println ("------------------woohoo start")
      seq {
        println ("------------------woohoo build a")
        fapp ("a") _
      }
      println ("------------------woohoo between")
      seq {
        println ("------------------woohoo build b")
        fapp ("b") _
      }
      println ("------------------woohoo end")
    }

  def testwsp1 = expect (List("1-a", "1-b")) { razie.M anyOrder prun (wsp1, 1) }

  // multilevel
  def wsp3 =
    seq {
      println ("------------------woohoo start")
      par {
        seq {
          println ("------------------woohoo build a")
          fapp("a") _
        }
        seq {
          println ("------------------woohoo build b")
          fapp("b") _
        }
      }
    }

  // multilevel
  def wsp2 =
    seq {
      par {
        seq {
          par {
            seq {
              fapp("a") _
            }
            seq {
              fapp("b") _
            }
          }
          sort[String] (_ < _)
          later { case l: List[String] => l mkString "," }
        }
        seq {
          par {
            seq {
              fapp("a") _
            }
            seq {
              fapp("b") _
            }
          }
          sort[String] (_ < _)
          later {
            case l: List[String] => l mkString ","
          }
        }
      }
      foldLeft[String] ("folded:") (_ + "," + _)
    }

  def testwsp3 = expect (List("1-a", "1-b")) { razie.M anyOrder prun (wsp3, 1) }
  def testwsp2 = expect ("folded:,1-a,1-b,1-a,1-b") { prun (wsp2, 1) }

  override def setUp() = { razie.wf.Gremlins.liveInside (new Engine with Threads) }
  override def tearDown() = { razie.wf.Gremlins.die }
}
