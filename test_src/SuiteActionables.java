/**
 * Razvan's code. Copyright 2008 based on Apache (share alike) see LICENSE.txt for details.
 */

import razie.actionables.test.TestActionables;
import junit.framework.Test;
import junit.framework.TestSuite;

import razie.actionables.test.ExecFactoryTest;

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
