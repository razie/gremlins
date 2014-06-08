/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._
import razie.AI.stoai
import razie.actionables.Actionables.make

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
