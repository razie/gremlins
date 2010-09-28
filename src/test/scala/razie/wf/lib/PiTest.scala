/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.lib

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

//import razie.wf._
import razie.wf._

/** PI examples */
class PiTest extends JUnit3Suite {
  import razie.wf.lib.PiCalc._
 
  def c = Channel("c", 1) // channel c - buffer size 1, i.e. non-blocking...
  def x = $("x") // variable x
  
  def myp11 = v(c) + P  // correct (v c) P
  def testmyp11 = expect ("1-P") { myp11.print run "1" }
  
  def myp21 = v(c) ( c.put($0) --> P )
  def myp22 = v(c) ( c -<- $0 + P )
  def myp23 = v(c)
  def testmyp21 = expect ("1-P") { myp21 run "1" }
  def testmyp22 = expect ("1-P") { myp22 run "1" }
  def testmyp23 = expect ("1") { myp23 run "1" }

  // channel exists, read+P : P waits to read a value and then continues
  def myp31  = c($0) + P        // correct x(0) P
  def myp32 = c ->- $0 + P  // correct x(0) P
  def testmyp31 = expect ("4-P") { (c.put(4) --> myp31).print run "1" }
  def testmyp32 = expect ("4-P") { (c.put(4) --> myp32).print run "1" }
 
  def myp41 = v(c) (c.get($0) --> P | c.put($0) <-- Q)  // correct v(c) ( c(0) P | c<0> Q )
  def testmyp41 = expect (true) { (myp41.print run "1").asInstanceOf[List[_]] contains "1-Q-P" }
  
  override def setUp () = { Gremlins.live; AllResources.clear }
  override def tearDown () = { Gremlins.die; AllResources.clear }
}
