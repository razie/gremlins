package com.razie.pub.actionables.test

import com.razie.pub.base.data._
import com.razie.pub.base._
//import com.razie.pub.assets._
import com.razie.pub.actionables._
import com.razie.pub.actionables.library._
import razie.base._
import org.scalatest.junit._

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

     ScalaExecFactory.reg ("raziecmd", new RazbaseExecutables)
     ExecutableFactory.init (ScalaExecFactory)
     
  def test1 :Unit = expect ("s2over") {
    val exec = new FunExec ((x:AttrAccess) => new AttrAccessImpl("res", x.getAttr("arg0") + "over"))
   
    val aa = ActFactory.make ("sayBubu", null, null, exec)
    val hasa = StaticAct.maker(List(aa))
   
    val soso = ActionableFactory.makeProxy (new SimpleObjectImpl(), hasa).asInstanceOf[SimpleTrait]
    soso sayBubu "s2"
    }
  
  def testBoolean :Unit = expect (false) {
    val exec = new FunExec ((x:AttrAccess) => new AttrAccessImpl("res", new java.lang.Boolean(false)))
   
    val aa = ActFactory.make ("retTrue", null, null, exec)
    val hasa = StaticAct.maker(List(aa))
   
    val soso = ActionableFactory.makeProxy (new SimpleObjectImpl(), hasa).asInstanceOf[SimpleTrait]
    soso.retTrue()
    }
}
