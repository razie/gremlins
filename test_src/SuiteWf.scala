

import junit.framework.Test;
import junit.framework.TestSuite;

import org.scalatest.junit._
import org.scalatest.SuperSuite

/** one suite to rule them all - run all tests in all shared projects */
class SuiteWf () extends junit.framework.TestSuite(classOf[XNada]) {
  
  // this is where you list the tests...
  addTest(new junit.framework.TestSuite(classOf[razie.wf.test.WfLibTest]))
  addTest(new junit.framework.TestSuite(classOf[razie.wf.test.WfBaseTest]))
  addTest(new junit.framework.TestSuite(classOf[razie.wf.test.DslSimpleTest]))
  addTest(new junit.framework.TestSuite(classOf[razie.wf.test.DslStructureTest]))
  addTest(new junit.framework.TestSuite(classOf[razie.wf.test.MoreSamplesTest]))
   
   def test1() = 
     // don't touch this line
     addTest(new junit.framework.TestSuite(classOf[razie.wf.test.WfLibTest]))
     
}

class XXNada extends junit.framework.TestCase {
 def testNada : Unit =  {}
}
