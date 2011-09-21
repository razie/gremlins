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

import razie.wf
import razie.wf._

class WfLibTest extends JUnit3Suite {

  def wnop = wf.nop
  def wlog = wf.log($0)
  
  def w1 = wf.nop + wf.log($0)
  def w1s = """nop; log($0)"""
  def w1p = """nop; log(1)""" // TODO enhance with parsing numbers
  def testw1 = expect (1) { w1 run 1 }
  def testw1s = expect (1) { wf(w1s) run 1 }
  def testw1p = expect ("1") { wf(w1p) run 1 }
  
  def testw11 = expect ("12") { wf.act("simple", "add", "a=1,b=2") run 1 }
  def testw12 = expect ("12") { wf ("act:simple:add(a=1,b=2)") run 1 }
  
  // test serialization
  def testserwnop = expect (true) { wf(wf toDsl wnop).isInstanceOf[WfActivity] }
  def testserwlog = expect (1) { wf(wf toDsl wlog) run 1 }
  def testw12s = expect ("12") { wf(wf toDsl wf("act:simple:add(a=1,b=2)")) run 1 }
  
  def wi1 = wf.inc(3) + wf.dec(2)
  def wi1s = """inc(3); dec (2)"""
  def testwi1 = expect (2) { wi1 run 1 }
  def testwi1s = expect (2) { wf(wi1s) run 1 }
  def testwi1ss = expect (2) { wf(wf toDsl wi1) run 1 }
//  
  // make sure it's the last test... - tests that there's no running processes
  def testDie = expect (true) { Gremlins.kill() }

  override def setUp () = { Gremlins.live() }
  override def tearDown () = { Gremlins.die() }

}

//class WfLibSpec extends Specification {
  //"1" should equal ("1")
//}
