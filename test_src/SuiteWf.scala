

import junit.framework.Test;
import junit.framework.TestSuite;

import org.scalatest.junit._
import org.scalatest.SuperSuite

class SuiteWf () extends junit.framework.TestSuite(classOf[XNadaWf]) {
  
  // this is where you list the tests...
  addTest(new junit.framework.TestSuite(classOf[razie.wf.test.WfLibTest]))
  addTest(new junit.framework.TestSuite(classOf[razie.wf.test.WfBaseTest]))
  addTest(new junit.framework.TestSuite(classOf[razie.wf.test.DslSimpleTest]))
  addTest(new junit.framework.TestSuite(classOf[razie.wf.test.DslStructureTest]))
  addTest(new junit.framework.TestSuite(classOf[razie.wf.test.MoreSamplesTest]))
  
  addTest(new junit.framework.TestSuite(classOf[razie.wf.lib.test.CspTest]))
  addTest(new junit.framework.TestSuite(classOf[razie.wf.lib.test.PiTest]))
  
  addTest(new junit.framework.TestSuite(classOf[razie.wf.lib.test.GTest]))
   
   def test1() = 
     // don't touch this line
     addTest(new junit.framework.TestSuite(classOf[razie.wf.test.WfLibTest]))
     
}

class XNadaWf extends junit.framework.TestCase {
 def testNada : Unit =  {}
}
