package razie.wf.test

import org.scalatest.junit._
import com.razie.pub.actionables._
import com.razie.pub.actionables.library._

//import razie.wf._
import razie.wf.study4._

class WfFunctionalTest extends JUnit3Suite {
//  import razie.wf.study4.Wf._
  import Wf._

  lazy val wdefif1 = wif (2==1) {
     var lacc = ""
     lacc += "it's "
     lacc += "true " + "1 "
     lacc += "..."
     lacc
  } welse waif (_==2) { v =>
     var lacc = ""
     lacc += "it's "
     lacc += "true " + v + " "
     lacc += "..."
     lacc
  } welse 
     waf {_.toString + " it's"} + 
     waf {s:Any => s.toString + " false"} + 
     waf {s:Any => s.toString + " ..."} 

  def testIf1 = expect ("1 it's false ...") { wdefif1 run 1 }
  def testIf2 = expect ("it's true 2 ...") { wdefif1 run 2 }
  def testIf3 = expect ("3 it's false ...") { wdefif1 run 3 }

}
