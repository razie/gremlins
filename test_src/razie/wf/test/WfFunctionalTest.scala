package razie.wf.test

import org.scalatest.junit._
import com.razie.pub.actionables._
import com.razie.pub.actionables.library._

import razie.wf._
import razie.wf.study2._

class WfFunctionalTest extends JUnit3Suite {

  def testIf1 :Unit = expect ("ExecTelnet") {
    val o = ExecutableFactory.instance.make("raziecmd:telnet host=localhost,port=4449,cmd=pause")
    o.getClass().getSimpleName
  }

  def testLog :Unit = expect ("ExecLog") {
    val o = ExecutableFactory.instance.make("raziecmd:log host=localhost,port=4449,cmd=pause")
    o.getClass().getSimpleName
  }

  def testAdd :Unit = expect ("ExecAdd") {
    val o = ExecutableFactory.instance.make("raziecmd:add a=2,b=3")
    o.getClass().getSimpleName
  }

  def testAdd1 :Unit = expect ("23") {
    val o = ActFactory.make("what?", "raziecmd:add a=2,b=3")
    o.execute a "result"
  }
 
  // test the url-like format
  def testAdd2 :Unit = expect ("23") {
    val o = ActFactory.make("what?", "raziecmd:add?a=2&b=3")
    o.execute a "result"
  }

}
