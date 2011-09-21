/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins.lib

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._
import razie.Gremlins

import razie.gremlins._
import razie.wf

/** CSP examples */
class CspTest extends JUnit3Suite {
  import razie.gremlins.lib.PiCalc._
 
  def c = Channel("c", 0) 
  
  def myp41 = v(c) (c ? P | c ! Q)  // correct v(c) ( c(0) P | c<0> Q )
  
  def testmyp41  = expect (true) { ((myp41.print run "1").asInstanceOf[List[_]] contains "1-Q-P") }
  def testmyp41d = expect (true) { ((wf(wf toDsl myp41) run "1").asInstanceOf[List[_]] contains "1-Q-P") }
  
  // make sure it's the last test... - tests that there's no running processes
  def testDie = expect (true) { Gremlins.kill() }

  override def setUp () = { Gremlins.live() }
  override def tearDown () = { Gremlins.die() }
}

