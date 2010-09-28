package razie.actionables;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import razie.base.ActionItem;
import razie.base.AttrAccess;
import razie.base.AttrAccessImpl;

/**
 * simple actionable factory
 * 
 * TODO 1-1 detailed docs
 * 
 * @author razvanc
 * 
 */
public abstract class ActionableFactory {

   private static ActionableFactory singleton;

   public static void init(ActionableFactory f) {
      singleton = f;
   }

   public static ActionableFactory instance() {
      if (singleton == null) throw new IllegalStateException ("NEEDS initialized - I think ActFactory does it...");
      return singleton;
   }

   public abstract Actionable make(String name, ActionItem ai, AttrAccess in, IExecutable ex);

   /** standard make from factory... */
   public abstract Actionable make(String name, String unifiedstring);

   public Actionable make(Actionable act, IExecutable newex) {
      return make(act.name, act.item, act.inArgs, newex);
   }

   /** Foo = (Foo) makeProxy (foo, overloadingActionables) */
   public static Object makeProxy(Object o, HasActionables hasa) {
      return DebugProxy.newInstance(o, hasa);
   }

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
   private static class DebugProxy implements java.lang.reflect.InvocationHandler {

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

            if (hasa.getActionables().containsKey(m.getName())) {
               Actionable a = hasa.getActionables().get(m.getName());
               AttrAccess in = new AttrAccessImpl(a.inArgs);

               int arg = 0;
               if (args != null)
                  for (Object o : args)
                     in.setAttr("arg" + String.valueOf(arg++), o);

               result = a.executable.apply(in, null);

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
}
