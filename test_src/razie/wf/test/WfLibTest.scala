package razie.wf.test

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

//import razie.wf._
import razie.wf._

class WfLibTest extends JUnit3Suite {
  import wf._

  def w1 = wf.nop + wf.log($0)
  def w1s = """nop; log($0)"""
  def w1p = """nop; log(1)""" // TODO enhance with parsing numbers
  def testw1 = expect (1) { w1 run 1 }
  def testw1s = expect (1) { wf(w1s) run 1 }
  def testw1p = expect ("1") { wf(w1p) run 1 }
}
