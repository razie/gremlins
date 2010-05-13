package razie.wf.test

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

import razie.wf._

class DslStructureTest extends JUnit3Suite {
  import wf._ // the workflow constructs library

  // ALL structure tests are in WfBaseTest - see that one
  
  def testNothing = expect (true) { true }
}
