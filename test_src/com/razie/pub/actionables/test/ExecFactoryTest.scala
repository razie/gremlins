package com.razie.pub.actionables.test

import org.scalatest.junit._
import com.razie.pub.actionables._
import com.razie.pub.actionables.library._

class ExecFactoryTest extends JUnit3Suite {

  // this is also registered by default...this is just for show
  ScalaExecFactory.reg ("raziecmd", new RazbaseExecutables)
  ExecutableFactory.init (ScalaExecFactory)
  
  def testTelnet :Unit = expect ("ExecTelnet") {
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
