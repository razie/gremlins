/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import razie.base.ActionItem;
import razie.base.AttrAccess;
import razie.base.AttrAccessImpl;

/**
 * simple proxy class to overload methods with actionables. Create one with a
 * call with your target and the overloading actionables:
 * <code>ActionableFactory.makeProxy(myObject, myActionables)</code>
 * 
 * TODO supports only methods with different names - cannot make a
 * distinction between two methods with same name but differnet arguments
 * 
 * TODO does not support getting interfaces from base classes - fairly stupid
 * imho...
 */
class DebugProxy(private val obj: AnyRef, private val hasa: HasActionables)
  extends java.lang.reflect.InvocationHandler {

  def invoke(proxy: AnyRef, m: Method, args: Array[AnyRef]): AnyRef = {
    var result: AnyRef = null
    try {
      System.out.println("before method " + m.getName());

      if (hasa.jactionables().containsKey(m.getName())) {
        val a: Actionable = hasa.jactionables().get(m.getName());
        val in: AttrAccess = new AttrAccessImpl(a.jinArgs());

        var arg = 0;
        if (args != null)
          for (o <- args) {
            in.setAttr("arg" + String.valueOf(arg), o)
            arg = arg + 1
          }

        result = a.jexecutable().apply(in, null).asInstanceOf[AnyRef]
      } else
        result = m.invoke(obj, args);
    } catch {
      case e: InvocationTargetException =>
        throw e.getTargetException();
      case e: Exception =>
        throw new RuntimeException("unexpected invocation exception: ", e);
    }
    finally {
    }

    result;
  }
}

object DebugProxy {
  def newInstance(obj: AnyRef, hasa: HasActionables): AnyRef = {
    java.lang.reflect.Proxy.newProxyInstance(
      obj.getClass().getClassLoader(),
      obj.getClass().getInterfaces(),
      new DebugProxy(obj, hasa))
  }
}
