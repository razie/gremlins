/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins.lib

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._
import razie.gremlins.act.WfAssetCmd
import razie.Gremlins

import razie.wf
import razie.gremlins._
import razie.g.{GRef, GAct, GAMAct}
import razie.base.{ActionItem, ActionContext=>AC}

object TestGAM extends GAct {
  def actions (k:GRef) : Seq[ActionItem] = List (razie.AI("play"), razie.AI("stop"))
  def act     (k:GRef, a:String, ctx:AC) : Any = "snakked: " + k + " - " + a + " : " + ctx
}

/** PI examples */
class GTest extends JUnit3Suite {
  import razie.gremlins.lib.PiCalc._
  
  def myp11 = WfAssetCmd ("play", GRef.id("Miku", "13"), razie.AA(), Map("maki" -> $0))
  def testmyp11 = expect (true) { (myp11.print run "1").asInstanceOf[String] matches "snakked.*maki.*=1.*" }
  println(wf toDsl myp11)
  def testmyp12 = expect (true) { (wf(wf toDsl myp11).print run "1").asInstanceOf[String] matches "snakked.*maki.*=1.*" }
  
  override def setUp () = { Gremlins.live;  GAMAct.assetMgr = TestGAM }
  override def tearDown () = { Gremlins.die }
}
