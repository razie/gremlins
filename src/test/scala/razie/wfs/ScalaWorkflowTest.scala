/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wfs

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

import razie.wf._

class ScalaWorkflowTest extends JUnit3Suite {
  // import the wfs instead of the wf to get the scala workflows
  import wfs._

  def f(in: Any): Any = { println ("------------- woohoo " + in); in }
  def finc(in: Any): Any = { val x = in.asInstanceOf[Int] + 1; println ("------------- woohoo " + x); x }
  def fapp(app: String)(in: Any): Any = { val x = in.toString + "-" + app; println ("------------- woohoo " + x); x }

  val wss1 = seq {
    println ("------------------woohoo 1")
  }

  val wss2 = seq { f _ }
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

  def prun(p: WfActivity, s: Any) = { val out = p.print run 1; p.print; out }

//  def testwss1 = expect (1) { prun (wss1, 1) }
//  def testwss2 = expect (1) { prun (wss2, 1) }
  //    def testwss3 = expect (1) { prun (wss3, 1) }
//  def testwss4 = expect (2) { prun (wss4, 1) }

  //  def testwss8 = expect ("1-a-b") { prun (wss8, 1) }
  //  def testwss9 = expect (2) { wss9.print run 3 }

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
  //  def testwsp1 = expect (List("1-a", "1-b")) { razie.M anyOrder prun (wsp1, 1) }

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
        }
      }
    }

//    def testwsp3 = expect (List("1-a", "1-b")) { razie.M anyOrder prun (wsp3, 1) }
  def testwsp2 = expect (List(List("1-a", "1-b"), List("1-a", "1-b"))) { prun (wsp2, 1) }

  override def setUp () = { Gremlins.live }
  override def tearDown () = { Gremlins.die }
}
