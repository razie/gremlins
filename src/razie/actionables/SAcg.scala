package razie.actionables

import org.w3c.dom._
import com.razie.pub.base.data.RazElement

object StaticAct {
  /** make a list of actionables from a list of actionables :) */
  def maker (l:List[Actionable]) : THasActionables = {
    new DefaultAct(Map() ++ l.map(x=>(x.name, x)))
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
class DefaultAct (override val actionables:Map[String,Actionable]) extends THasActionables {
}
