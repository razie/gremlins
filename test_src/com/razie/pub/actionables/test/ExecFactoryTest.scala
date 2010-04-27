package razie.actionables.test

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

class ExecFactoryTest extends JUnit3Suite {

  // this is also registered by default...this is just for show
  ScalaExecFactory.reg ("simple", new RazbaseExecutables)
  ExecutableFactory.init (ScalaExecFactory)
  
  def testTelnet :Unit = expect ("ExecTelnet") {
    val o = ExecutableFactory.instance.make("simple:telnet host=localhost,port=4449,cmd=pause")
    o.getClass().getSimpleName
  }

  def testLog :Unit = expect ("ExecLog") {
    val o = ExecutableFactory.instance.make("simple:log host=localhost,port=4449,cmd=pause")
    o.getClass().getSimpleName
  }

  def testAdd :Unit = expect ("ExecAdd") {
    ExecutableFactory.instance.make("simple:add a=2,b=3").getClass().getSimpleName
  }

  def testAdd1 :Unit = expect ("23") {
    ActFactory.make("what?", "simple:add a=2,b=3").execute 
  }
 
  // test the url-like format
  def testAdd2 :Unit = expect ("23") {
    ActFactory.make("what?", "simple:add?a=2&b=3").execute 
  }
  
  // test the func-like format
  def testAdd3 :Unit = expect ("23") {
    ActFactory.make("what?", "simple:add(a=2&b=3)").execute 
  }

  // test the func-like format
  def testAdd4 :Unit = expect ("23") {
    ActFactory.make("what?", "simple:add (a=2&b=3)").execute 
  }

}
