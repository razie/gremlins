package razie.wf.test

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

import razie.wf._

class MoreSamplesTest extends JUnit3Suite {
  import wf._

  //-------------------------------- if
  
  lazy val wdefif1 = wfs.wsif (2==1) {
     var lacc = ""
     lacc += "it's "
     lacc += "true " + "1 "
     lacc += "..."
     lacc
  } welse wfs.waif (_==2) { v =>
     var lacc = ""
     lacc += "it's "
     lacc += "true " + v + " "
     lacc += "..."
     lacc
  } welse 
     wfs.wa {_.toString + " it's"} + 
     wfs.wa {s:Any => s.toString + " false"} + 
     wfs.wa {s:Any => s.toString + " ..."} 

  def testIf1 = expect ("1 it's false ...") { wdefif1 run 1 }
  def testIf2 = expect ("it's true 2 ...") { wdefif1 run 2 }
  def testIf3 = expect ("3 it's false ...") { wdefif1 run 3 }

  override def setUp () = { Engines.start }
  override def tearDown () = { Engines().stop }
}
