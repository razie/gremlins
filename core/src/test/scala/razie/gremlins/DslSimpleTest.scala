/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._
import razie.Gremlins

import razie.gremlins._

class DslSimpleTest extends JUnit3Suite {
  import razie.wf
  import razie.wf._

  // simple sequence
  def w10 = wf.nop + wf.inc + wf.log($0)
  val w11 = wf.nop --> wf.inc --> wf.log($0) // trouble...
  val w12 = wf.nop --| wf.inc --| wf.log($0) // ok...
  def w1s = """nop; inc; log($0)"""
  def w2s = """seq { nop; inc; log($0) }"""
  def w1m = """
nop
inc
log($0)
"""
  def testw10 = expect (2) { w10 run 1 }
  def testw11 = expect (1) { w11 run 1 }
  def testw12 = expect (2) { w12 run 1 }
  def testw1s = expect (2) { wf(w1s) run 1 }
  def testw2s = expect (2) { wf(w2s) run 1 }
  def testw1m = expect (2) { wf(w1m) run 1 }
 
  // TODO work on the expressions with $0
//  def w20 = wf.nop + wf.log($0 + ".") + wf.log($0 + ".")
//  def testw20 = expect ("1..") { w20 run "1" }

    // make sure it's the last test... - tests that there's no running processes
  def testDie = expect (true) { Gremlins.kill() }

  override def setUp () = { Gremlins.live() }
  override def tearDown () = { Gremlins.die() }
}
