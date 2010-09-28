/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

import razie.wfs._

class OtherTest extends JUnit3Suite {
  // import the wfs instead of the wf to get the scala workflows
  import wft._

  //-------------------------------- if
  
  lazy val wdefif1 = wft.wsif (2==1) {
     var lacc = ""
     lacc += "it's "
     lacc += "true " + "1 "
     lacc += "..."
     lacc
  } welse wft.waif (_==2) { v =>
     var lacc = ""
     lacc += "it's "
     lacc += "true " + v + " "
     lacc += "..."
     lacc
  } welse 
     wft.wa {_.toString + " it's"} + 
     wft.wa {s:Any => s.toString + " false"} + 
     wft.wa {s:Any => s.toString + " ..."} 

  def testIf1 = expect ("1 it's false ...") { wdefif1 run 1 }
  def testIf2 = expect ("it's true 2 ...") { wdefif1 run 2 }
  def testIf3 = expect ("3 it's false ...") { wdefif1 run 3 }

  override def setUp () = { Gremlins.live }
  override def tearDown () = { Gremlins.die }
}
