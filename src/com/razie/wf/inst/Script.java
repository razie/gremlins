/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package com.razie.wf.inst;

import razie.base.scripting.RazScript;
import razie.base.scripting.ScriptFactory;

/**
 * 
 * TODO remove class
 * 
 * @author razvanc
 * 
 */
public class Script {

   public static RazScript toScript(Object object) {
      if (object == null) {
         return null;
      } else if (object instanceof RazScript) {
         return (RazScript) object;
      } else if (object instanceof String) {
         return ScriptFactory.make(null, (String) object);
      }

      throw new IllegalArgumentException("can't make script from " + object.getClass().getName());
   }
}
