/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wfstest

import org.scalatest.junit._
import razie.gremlins.eng.{ Engine, Threads }

class LoopTestMain extends App {
  val t = new LoopTest
  t.setUp()
//  t.testwif1
  t.tearDown()
}

class LoopTest extends JUnit3Suite {
  // import the wfs instead of the wf to get the scala workflows
  import razie.wfs
  import razie.wfs._

  razie.g.Graphs.maxDebugDepth = 50
 
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
        later { fapp("a") _ }
      } welse {
        println ("------------------woohoo build b")
        later { fapp("b") _ }
      }
      wif (_ == 2) {
        println ("------------------woohoo build a")
        later { fapp("a") _ }
      } welse {
        println ("------------------woohoo build b")
        later { fapp("b") _ }
      }
    }
  def testwif1  = expect ("1-a-b") { prun (wif1, 1) }
  def testwif1s = expect ("1-a-b") { prun (wfs strict wif1, 1) }

  // foreach loop
  def wforeach1 =
    seq {
      var x = 0 // bad - var in scala, no state captured in the workflow
      wforeach {
        matchLater { case i:Int => x += i }
      }
      w { _ => x } // another way to assign x to the default value
    }
    def testwforeach1 = expect (6) { prun (wforeach1, List(1,2,3)) }
    def testwforeach1s = expect (6) { prun (wfs strict wforeach1, List(1,2,3)) }

  // parallel map
  def wsmap1 =
    seq {
      wsmap[Int,Int] (3) { x:Int => x + 1 }
    }
    def testwsmap1 = expect (List(2,3,4)) { prun (wsmap1, List(1,2,3)) }
    def testwsmap1s = expect (List(2,3,4)) { prun (wfs strict wsmap1, List(1,2,3)) }

  // simplest loop
//  def wmap1 =
//    seq {
//      var x = 0 // bad - var in scala, no state captured in the workflow
//      wmap {
//        later { case i:Int => i+1 ; case s@_ => razie.Log ("unexpected value: " + s)}
//      }
//      w { _ => x } // another way to assign x to the default value
//    }
//    def testwloop1 = expect ("1-a-a-a") { prun (wloop1, List(1,2,3)) }
//    def testwmap1s = expect (List(2,3,4)) { prun (wfs strict wmap1, List(1,2,3)) }

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

  override def setUp() = { razie.Gremlins.liveInside (new Engine with Threads) }
  override def tearDown() = { razie.Gremlins.die() }

  // simplify running the workflow
  def prun(p: razie.gremlins.WfActivity, s: Any) = {
    val ctx = razie.base.scripting.ScriptFactory.mkContext("scala", null)
    val out = razie.Gremlins().exec (p.print, s)
    println ("============= RESULTS ===============")
    p.print;
    println ("context: " + ctx)
    println ("value: " + out)
    out
  }

}
