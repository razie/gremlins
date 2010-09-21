/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables

import org.w3c.dom._
import razie.base.data.RazElement

object StaticAct {
  /** make a list of actionables from a list of actionables :) */
  def maker (l:List[Actionable]) : THasActionables = {
    new DefaultAct(Map() ++ l.map(x=>(x.name, x)))
  }  
  
  /** make a list of actionables from an <actionables> tag */
  def maker (e:RazElement) : THasActionables = {
    val l = for (action <- e xpl "action") yield {
      val re : RazElement = action
      var s = re a "exec"
      
      if("" == s) 
        s = e nodeVal
          
      Actionables.make(action a "name", s)
    }
    
    maker(l)
  }  
}

/** simple default collcetion of actionables */
class DefaultAct (override val actionables:Map[String,Actionable]) extends THasActionables {
}
