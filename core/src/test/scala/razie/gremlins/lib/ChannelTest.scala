/** ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins.lib

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._
import razie.Gremlins

import razie.gremlins._
import razie.wf

class ChannelTest extends JUnit3Suite {
  import razie.gremlins.lib.PiCalc._
  import razie.gremlins.lib.PiCalc

  def t = 500

  def myp41(c: WfChannel) = v(c)((wf.sleep(t) + (c ? P)) | ((c ! Q) + wf.sleep(t))) 

  def testasync = expect(true) {
    Channel destroy "c"
    val c = Channel("c", 1) // asnc channel with 1 slot
    val res = razie.Timer { myp41(c).print run "1" }
    println("==================>>>>>>>>>>>>>" + res)
    res._1 < 2 * t
  }

  def testsync = expect(true) {
    Channel destroy "c"
    val c = Channel("c", 0) // channel with 0 slots becomes sync
    val res = razie.Timer { myp41(c).print run "1" }
    println("==================>>>>>>>>>>>>>" + res)
    res._1 > 2 * t
  }

  // make sure it's the last test... - tests that there's no running processes
  def testDie = expect(true) { Gremlins.kill() }

  override def setUp() = { Gremlins.live() }
  override def tearDown() = { Gremlins.die() }
}
