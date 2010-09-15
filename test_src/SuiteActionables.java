/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
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
