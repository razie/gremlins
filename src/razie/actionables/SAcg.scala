package razie.actionables

import org.w3c.dom._
import com.razie.pub.base.data.RazElement

object StaticAct {
  /** make a list of actionables from a list of actionables :) */
  def maker (l:List[Actionable]) : THasActionables = {
    val actions = new DefaultAct(null)
    
    for (val a <- l.toList ) 
      actions add a
    
    actions
  }  
  
  /** make a list of actionables from an <actionables> tag */
  def maker (e:RazElement) : THasActionables = {
    val l = for (val action <- e xpl "action") yield {
      val re : RazElement = action
      var s = re a "exec"
      
      if("" == s) 
        s = e nodeVal
          
      ActFactory.make(action a "name", s)
    }
    
    maker(l)
  }  
}

/** simple default collcetion of actionables */
class DefaultAct (var actions:java.util.Map[String,Actionable]) extends THasActionables {
  if (actions == null)
    actions = new java.util.HashMap[String,Actionable]()
  
  override def getActionables() = {
    actions 
  }
  
  def add (x:Actionable) = actions.put(x.name, x)
}
