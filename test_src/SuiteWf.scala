

import junit.framework.Test;
import junit.framework.TestSuite;

import org.scalatest.junit._
import org.scalatest.SuperSuite

/** one suite to rule them all - run all tests in all shared projects */
class SuiteWf () extends junit.framework.TestSuite(classOf[XNada]) {
  
  // this is where you list the tests...
  addTest(new junit.framework.TestSuite(classOf[com.razie.pub.actionables.test.TestActionables]))
  addTest(new junit.framework.TestSuite(classOf[com.razie.pub.actionables.test.ExecFactoryTest]))
   
   def test1() = 
     // don't touch this line
     addTest(new junit.framework.TestSuite(classOf[com.razie.pub.actionables.test.TestActionables]))
     
}

class XXNada extends junit.framework.TestCase {
 def testNada : Unit =  {}
}
