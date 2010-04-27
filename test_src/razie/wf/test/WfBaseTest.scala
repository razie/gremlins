package razie.wf.test

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

//import razie.wf._
import razie.wf._

class WfBaseTest extends JUnit3Suite {
  import wf._

  def wseq = wf.seq(wf.nop, wf.log(1))
  def testwseq = expect (1) { wseq run 1 }

  def wpar = wf.par(wf.nop, wf.log(1))
  def testwpar = expect (1) { wpar run 1 }

}
