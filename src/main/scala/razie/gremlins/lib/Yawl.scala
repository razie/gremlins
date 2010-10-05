/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins.lib

import razie.AA
import razie.base.{ActionContext => AC}
import razie.gremlins._
import razie.gremlins.act._

/** bpel activities implemented as wf templates 
 * 
 * http://www.oasis-open.org/committees/download.php/23964/wsbpel-v2.0-primer.htm
 */
object yawl extends WfLib[WfActivity] {
   
  override def wrap (e:WfExec) : WfActivity = new WfWrapper (e)

  //-------------------------------- basic activities

  def grawl = todo
  
}
