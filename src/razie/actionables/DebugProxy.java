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
class DebugProxy implements java.lang.reflect.InvocationHandler {

   private Object obj;
   private HasActionables hasa;

   public static Object newInstance(Object obj, HasActionables hasa) {
      return java.lang.reflect.Proxy.newProxyInstance(obj.getClass().getClassLoader(), obj.getClass()
            .getInterfaces(), new DebugProxy(obj, hasa));
   }

   private DebugProxy(Object obj, HasActionables hasa) {
      this.hasa = hasa;
      this.obj = obj;
   }

   public Object invoke(Object proxy, Method m, Object[] args) throws Throwable {
      Object result;
      try {
         System.out.println("before method " + m.getName());

         if (hasa.jactionables().containsKey(m.getName())) {
            Actionable a = hasa.jactionables().get(m.getName());
            AttrAccess in = new AttrAccessImpl(a.jinArgs());

            int arg = 0;
            if (args != null)
               for (Object o : args)
                  in.setAttr("arg" + String.valueOf(arg++), o);

            result = a.jexecutable().apply(in, null);

         } else
            result = m.invoke(obj, args);
      } catch (InvocationTargetException e) {
         throw e.getTargetException();
      } catch (Exception e) {
         throw new RuntimeException("unexpected invocation exception: ", e);
      } finally {
      }

      return result;
   }
}
