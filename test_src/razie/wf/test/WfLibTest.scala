package razie.wf.test

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

//import razie.wf._
import razie.wf._

class WfLibTest extends JUnit3Suite {
  import wf._

  def wnop = wf.nop
  def wlog = wf.log($0)
  
  def w1 = wf.nop + wf.log($0)
  def w1s = """nop; log($0)"""
  def w1p = """nop; log(1)""" // TODO enhance with parsing numbers
  def testw1 = expect (1) { w1 run 1 }
//  def testw1s = expect (1) { wf(w1s) run 1 }
//  def testw1p = expect ("1") { wf(w1p) run 1 }
//  
//  def testw11 = expect ("12") { act("simple", "add", "a=1,b=2") run 1 }
//  def testw12 = expect ("12") { wf ("act:simple:add(a=1,b=2)") run 1 }
//  
//  // test serialization
//  def testserwnop = expect (true) { wf(wf toDsl wnop).isInstanceOf[WfActivity] }
//  def testserwlog = expect (1) { wf(wf toDsl wlog) run 1 }
//  def testw12s = expect ("12") { wf(wf toDsl wf("act:simple:add(a=1,b=2)")) run 1 }
//  
//  def wi1 = wf.inc(3) + wf.dec(2)
//  def wi1s = """inc(3); dec (2)"""
//  def testwi1 = expect (2) { wi1 run 1 }
//  def testwi1s = expect (2) { wf(wi1s) run 1 }
//  def testwi1ss = expect (2) { wf(wf toDsl wi1) run 1 }
//  
  override def setUp () = { Engines.start }
  override def tearDown () = { Engines().stop }
}
