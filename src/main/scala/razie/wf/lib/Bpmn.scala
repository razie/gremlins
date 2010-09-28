/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.lib

import razie.AA
import razie.base.{ActionContext => AC}
import razie.wf._
import razie.wf.act._

/** bpmn basic constructs, implemented as wf templates 
 * 
 * http://en.wikipedia.org/wiki/Business_Process_Modeling_Notation
 */
object bpmn extends WfLib[WfActivity] {
   
  override def wrap (e:WfExec) : WfActivity = new WfWrapper (e)

  //-------------------------------- other

  def scope = todo
  
}
