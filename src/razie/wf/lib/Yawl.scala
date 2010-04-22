/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.AA
import razie.base.{ActionContext => AC}

/** bpel activities implemented as wf templates 
 * 
 * http://www.oasis-open.org/committees/download.php/23964/wsbpel-v2.0-primer.htm
 */
object yawl extends WfLib[WfAct] {
   
  override def wrap (e:WfExec) : WfAct = razie.wf.WfWrapper (e)

  //-------------------------------- basic activities

  def grawl = todo
  
}
