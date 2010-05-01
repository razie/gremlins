package razie.wf.test

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

//import razie.wf._
import razie.wf._

class WfBaseTest extends JUnit3Suite {
  import wf._

  def wseq = seq(nop, log(1))
  def testwseq = expect (1) { wseq run 1 }

  def wpar = par(nop, log(1))
  def testwpar = expect (1::1::Nil) { wpar run 1 }

}
