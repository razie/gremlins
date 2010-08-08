package razie.wf.lib.test

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

//import razie.wf._
import razie.wf._
import razie.g.{GRef, GAct, GAMAct}
import razie.base.{ActionItem, ActionContext=>AC}

object TestGAM extends GAct {
  def actions (k:GRef) : Seq[ActionItem] = List (razie.AI("play"), razie.AI("stop"))
  def act     (k:GRef, a:String, ctx:AC) : Any = "snakked: " + k + " - " + a + " : " + ctx
}

/** PI examples */
class GTest extends JUnit3Suite {
  import razie.wf.lib.PiCalc._
  
  def myp11 = WfAssetCmd ("play", GRef.id("Miku", "13"), razie.AA(), Map("maki" -> $0))
  def testmyp11 = expect (true) { (myp11.print run "1").asInstanceOf[String] matches "snakked.*maki=1.*" }
  println(wf toDsl myp11)
  def testmyp12 = expect (true) { (wf(wf toDsl myp11).print run "1").asInstanceOf[String] matches "snakked.*maki=1.*" }
  
  override def setUp () = { Engines.start; GAMAct.assetMgr = TestGAM }
  override def tearDown () = { Engines().stop }
}
