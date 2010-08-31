package razie.actionables.test

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

class ExecFactoryTest extends JUnit3Suite {
   import Actionables._

  // this is also registered by default...this is just for show
  Executables.reg ("simple", RazbaseExecutables)
  
  def testTelnet :Unit = expect ("ExecTelnet") {
    val o = Executables.make("simple:telnet host=localhost,port=4449,cmd=pause")
    o.getClass().getSimpleName
  }

  def testLog :Unit = expect ("ExecLog") {
    val o = Executables.make("simple:log host=localhost,port=4449,cmd=pause")
    o.getClass().getSimpleName
  }

  def testAdd :Unit = expect ("ExecAdd") {
    Executables.make("simple:add a=2,b=3").getClass().getSimpleName
  }

  def testAdd1 :Unit = expect ("23") {
    make("what?", "simple:add a=2,b=3").execute 
  }
 
  // test the url-like format
  def testAdd2 :Unit = expect ("23") {
    make("what?", "simple:add?a=2&b=3").execute 
  }
  
  // test the func-like format
  def testAdd3 :Unit = expect ("23") {
    make("what?", "simple:add(a=2&b=3)").execute 
  }

  // test the func-like format
  def testAdd4 :Unit = expect ("23") {
    make("what?", "simple:add (a=2&b=3)").execute 
  }

}
