/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wfstest

import org.scalatest.junit._
import razie.gremlins.eng.{Engine, Threads}

class ScalaWorkflowTest extends JUnit3Suite {
  // import the wfs instead of the wf to get the scala workflows
  import razie.wfs
  import razie.wfs._

  
  // some functions to simplify the workflows
  def finc(in: Any): Any = {
    val x = in.asInstanceOf[Int] + 1;
    println ("------------- woohoo " + x);
    x
  }

  def woohoo(app: String)(in: Any): Any = {
    val x = in.toString + "-" + app;
    println ("------------- woohoo " + x);
    x
  }

  // MISTAKE: finc is completely ignored
  val wss2 = seq { finc _ }
  // correct
  val wss3 = sync (finc _)
  val wss4 = async (finc _)
  
  def testwss2 = expect (1) { prun (wss2, 1) }
  def testwss3 = expect (2) { prun (wss3, 1) }
  def testwss4 = expect (2) { prun (wss4, 1) }

  // sequence - note the laziness in the printed messages
  def wss7 = seq {
    println ("------------------woohoo start")
    later {
      println ("------------------woohoo build a")
      woohoo ("a") _
    }
    println ("------------------woohoo between")
    later {
      println ("------------------woohoo build b")
      woohoo ("b") _
    }
    println ("------------------woohoo end")
  }

  // preserving types
  def wss9 = seq {
    println ("------------------woohoo start")
    later { finc _ }
    println ("------------------woohoo between")
    later { finc _ }
    println ("------------------woohoo end")
  }

  def testwss7 = expect ("1-a-b") { prun (wss7, 1) }
  def testwss9 = expect (3) { wss9.print run 1 }

  
  // parallel
  def wsp1 =
    par {
      println ("------------------woohoo start")
      later {
        println ("------------------woohoo build a")
        woohoo ("a") _
      }
      println ("------------------woohoo between")
      later {
        println ("------------------woohoo build b")
        woohoo ("b") _
      }
      println ("------------------woohoo end")
    }

  def testwsp1 = expect (List("1-a", "1-b")) { razie.M anyOrder prun (wsp1, 1) }

  // multilevel - simple scala code
  def wsp3 =
    seq {
      println ("------------------woohoo start")
      par {
        seq {
          println ("------------------woohoo build a")
          later { woohoo("a") _ }
        }
        seq {
          println ("------------------woohoo build b")
          later { woohoo("b") _ }
        }
      }
    }
  def testwsp3 = expect (List("1-a", "1-b")) { razie.M anyOrder prun (wsp3, 1) }

  // multilevel - workflow built and then ran: 
  // the woohoo is now invoked as a separate activity and others may cut in - note the sequence of messages is likely different
  def wsp4 =
    seq {
      println ("------------------woohoo start")
      par {
        seq {
          println ("------------------woohoo build a")
          later { woohoo("a") _ }
        }
        seq {
          println ("------------------woohoo build b")
          later { woohoo("b") _ }
        }
      }
    }
  def testwsp4 = expect (List("1-a", "1-b")) { razie.M anyOrder prun (wsp4, 1) }

  // strict - see the two distinct passes: one runs through the code to build the "later" lazy workflow
  // second pass runs those activities
  def wsp5 = wfs strict seq {
      println ("------------------woohoo start")
      par {
        seq {
          println ("------------------woohoo build a")
          later { woohoo("a") _ }
        }
        seq {
          println ("------------------woohoo build b")
          later { woohoo("b") _ }
        }
      }
    }
  def testwsp5 = expect (List("1-a", "1-b")) { razie.M anyOrder prun (wsp5, 1) }

  // multilevel
  def wsp2 =
    seq {
      par {
        seq {
          par {
            seq {
              wfs later woohoo("a") _
            }
            seq {
              wfs later woohoo("b") _
            }
          }
          sort[String] (_ < _)
          wfs matchLater { case l: List[String] => l mkString "," }
        }
        seq {
          par {
            seq {
              wfs later woohoo("a") _
            }
            seq {
              wfs later woohoo("b") _
            }
          }
          sort[String] (_ < _)
          matchLater {
            case l: List[String] => l mkString ","
          }
        }
      }
      foldLeft[String] ("folded:") (_ + "," + _)
    }

  def testwsp2 = expect ("folded:,1-a,1-b,1-a,1-b") { prun (wsp2, 1) }

  // just a DSL example of simulating the let! from F#
  def wfa1 = seq {
    val a = let! sync { _ + "-a" }
    matchLater { case _ => a.get + "-b" }
  }
  
  def testwfa1 = expect ("1-a-b") { prun (wfa1, 1) }
  
  // can't define activities in a sync/async leaf
  def wfcl1 = w {
    matchLater { case _ => "-b" }
    println ("xx") // need this due to signature
  }
// TODO  
//  def testwfcl1 = expect ("1-a-b") { prun (wfa1, 1) }
  
  override def setUp() = { razie.Gremlins.liveInside (new Engine with Threads) }
  override def tearDown() = { razie.Gremlins.die }
  
  // simplify running the workflow
  def prun(p: razie.gremlins.WfActivity, s: Any) = {
    val out = p.print run 1;
    p.print;
    out
  }

}
