/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wfstest

import org.scalatest.junit._
import razie.gremlins.eng.{ Engine, Threads }
import razie.wfs
import razie.wfs._

// test wfs loop constructs and wif etc
class LoopTest extends JUnit3Suite {

  razie.g.Graphs.maxDebugDepth = 50

  def fapp(app: String)(in: Any): Any = {
    val x = in.toString + "-" + app;
    println("------------- woohoo " + x);
    x
  }

  // simple if test
  def wif1 =
    seq {
      wif(_ == 1) {
        wfs later fapp("a") 
      } welse {
        wfs later fapp("b") 
      }
      wif(_ == 2) {
        wfs later fapp("a") 
      } welse {
        wfs later fapp("b") 
      }
    }
  def testwif1 = expect("1-a-b") { wif1 printAndRun 1 }
  def testwif1s = expect("1-a-b") { wfs strict wif1 printAndRun 1 }

  // foreach loop
  def wforeach1 =
    seq {
      var x = 0 // bad - var in scala, no state captured in the workflow
      wforeach {
        matchLater { case i: Int => x += i }
      }
      w { _ => x } // another way to assign x to the default value
    }
  def testwforeach1 = expect(6) { wforeach1 printAndRun List(1, 2, 3) }
  def testwforeach1s = expect(6) { wfs strict wforeach1 printAndRun List(1, 2, 3) }

  // parallel map
  def wsmap1 =
    seq {
      wsmap[Int, Int](3) { x: Int => x + 1 }
    }
  def testwsmap1 = expect(List(2, 3, 4)) { wsmap1 printAndRun List(1, 2, 3) }
  def testwsmap1s = expect(List(2, 3, 4)) { wfs strict wsmap1 printAndRun List(1, 2, 3) }

  // simplest loop
//    def wmap1 =
//      seq {
//        var x = 0 // bad - var in scala, no state captured in the workflow
//        wmap {
//          later { 
//            i:Int => i+1 
//            }
//        }
//        w { _ => x } // another way to assign x to the default value
//      }
//      def testwmap1s = expect (List(2,3,4)) { wfs strict wmap1 printAndRun List(1,2,3) }

  // multilevel - workflow built and then ran: 
  // the fapp is now invoked as a separate activity and others may cut in - note the sequence of messages is likely different
  def wsp4 =
    seq {
      par {
        seq {
          w { fapp("a") _ }
        }
        seq {
          w { fapp("b") _ }
        }
      }
    }
   def testwsp4 = expect(List("1-a", "1-b")) { razie.M anyOrder (wsp4 printAndRun 1) }

  override def setUp() = { razie.Gremlins.liveInside(new Engine with Threads) }
  override def tearDown() = { razie.Gremlins.die() }

}
