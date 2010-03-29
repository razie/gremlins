/**
 * Razvan's code. Copyright 2008 based on Apache (share alike) see LICENSE.txt for details.
 */

import junit.framework.Test;
import junit.framework.TestSuite;

import com.razie.pub.actionables.test.ExecFactoryTest;
import com.razie.pub.actionables.test.TestActionables;

/**
 * suite 
 * 
 * @author razvanc99
 */
public class SuiteActionables extends TestSuite {
    public static Test suite() {
        TestSuite result = new TestSuite(SuiteActionables.class.getName());
          
        result.addTestSuite(ExecFactoryTest.class);
        result.addTestSuite(TestActionables.class);

        return result;
    }

}
