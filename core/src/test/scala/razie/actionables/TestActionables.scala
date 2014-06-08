/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables

import com.razie.pub.base.data._
import com.razie.pub.base._
import razie.actionables._
import razie.actionables.library._
import razie.base._
import org.scalatest.junit._
import razie.actionables.Actionables.make
import razie.actionables.Actionables.makeProxy

/** need this for dynamic proxies - these need an interface apparently...? */
trait SimpleTrait {
  def sayBubu (s:String):String
  def retTrue ():Boolean
}

/** actual class to wrap in a dynamic proxy */
class SimpleObjectImpl extends SimpleTrait {
  def sayBubu (s:String):String = s+"bubu"
  def retTrue ():Boolean = true
}

class TestActionables extends JUnit3Suite {
   import Actionables._
   
  def test1 :Unit = expect ("s2over") {
    val exec = new FunExec ((x:ActionContext) => x.getAttr("arg0") + "over")
   
    val aa = make ("sayBubu", null, null, exec)
    val hasa = StaticAct.maker(List(aa))
   
    val soso = makeProxy (new SimpleObjectImpl(), hasa).asInstanceOf[SimpleTrait]
    soso sayBubu "s2"
    }
  
  def testBoolean :Unit = expect (false) {
    val exec = new FunExec ((x:ActionContext) => new java.lang.Boolean(false))
   
    val aa = make ("retTrue", null, null, exec)
    val hasa = StaticAct.maker(List(aa))
   
    val soso = makeProxy (new SimpleObjectImpl(), hasa).asInstanceOf[SimpleTrait]
    soso.retTrue()
    }
}
