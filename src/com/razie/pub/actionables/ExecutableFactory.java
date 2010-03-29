package com.razie.pub.actionables;

/**
 * this is important: all possible libraries of actions will register here to
 * create their own actions
 * 
 * The executables are identified by their domain and a name, unique inside the
 * domain.
 * 
 * @author razvanc
 */
public abstract class ExecutableFactory {

   private static ExecutableFactory singleton;

   /**
    * call in main() or alike with your actual factory. The default to use is
    * ScalaExecFactory
    */
   public static void init(ExecutableFactory f) {
      singleton = f;
   }

   public static ExecutableFactory instance() {
      return singleton;
   }

   /**
    * make an executable. Example unifiedstring is: [domain:command args] i.e.
    * "raziecmd:telnet host=localhost,port=4449,cmd=pause"
    * 
    * @param unifiedstring
    * @return
    */
   public abstract IExecutable make(String unifiedstring);

   /**
    * @return the domains currently registered
    */
   public abstract String[] domains();

   /**
    * @param domain
    * @return the actions in this domain
    */
   public abstract String[] commands(String domain);
}
