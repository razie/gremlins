/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wfstest

import org.scalatest.junit._
import razie.gremlins.eng.{ Engine, Threads }

class LoopTestMain extends Application {
  val t = new LoopTest
  t.setUp()
//  t.testwif1
  t.tearDown()
}

class LoopTest extends JUnit3Suite {
  // import the wfs instead of the wf to get the scala workflows
  import razie.wfs
  import razie.wfs._

  def fapp(app: String)(in: Any): Any = {
    val x = in.toString + "-" + app;
    println ("------------- woohoo " + x);
    x
  }

  // simple if test
  def wif1 =
    seq {
      println ("------------------woohoo start")
      wif (_ == 1) {
        println ("------------------woohoo build a")
        sync { fapp("a") _ }
      } welse {
        println ("------------------woohoo build b")
        sync { fapp("b") _ }
      }
      wif (_ == 2) {
        println ("------------------woohoo build a")
        sync { fapp("a") _ }
      } welse {
        println ("------------------woohoo build b")
        sync { fapp("b") _ }
      }
    }
//  def testwif1  = expect ("1-a-b") { prun (wif1, 1) }
//  def testwif1s = expect ("1-a-b") { prun (wfs strict wif1, 1) }

  // simplest loop
  def wloop1 =
    seq {
      println ("------------------woohoo start")
      wforeach {
        println ("------------------woohoo build a")
        w { fapp("a") _ }
      }
    }
//    def testwloop1 = expect ("1-a-a-a") { prun (wloop1, List(1,2,3)) }
    def testwloop1s = expect ("1-a-a-a") { prun (wfs strict wloop1, List(1,2,3)) }

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

  // just a DSL example of simulating the let! from F#
  def wfa1 = seq {
    val a = let ! sync { _ + "-a" }
    later { case _ => a.get + "-b" }
  }

  override def setUp() = { razie.Gremlins.liveInside (new Engine with Threads) }
  override def tearDown() = { razie.Gremlins.die }

  // simplify running the workflow
  def prun(p: razie.gremlins.WfActivity, s: Any) = {
    val out = p.print run 1;
    p.print;
    out
  }

}
