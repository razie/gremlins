/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.test

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

import razie.wf._

class DslStructureTest extends JUnit3Suite {
  import wf._ // the workflow constructs library

  // ALL structure tests are in WfBaseTest - see that one
  
  def testNothing = expect (true) { true }
  
  override def setUp () = { Engines.start }
  override def tearDown () = { Engines().stop }
}
