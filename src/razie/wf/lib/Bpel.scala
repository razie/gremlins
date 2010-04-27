/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.lib

import razie.AA
import razie.base.{ActionContext => AC}
import razie.wf._

/** bpel activities implemented as wf templates 
 * 
 * http://www.oasis-open.org/committees/download.php/23964/wsbpel-v2.0-primer.htm
 */
object bpel extends WfLib[WfAct] {
   
  override def wrap (e:WfExec) : WfAct = razie.wf.WfWrapper (e)

  //-------------------------------- basic activities

  def invoke = todo
  def wwait = todo
  def receive = todo
  def wthrow = todo
  def reply = todo
  def empty = nop
  def assign = todo
  def terminate = todo

  //-------------------------------- structured activities

  def flow = todo
  def sequence = todo
  def pick = todo
  def switch = todo
  def whilee = todo
  def repeatUntil = todo
  def forEach = todo // parallel or seq...
  def wif = todo // if cond act elseif cond act else act
  
  //-------------------------------- other

  def scope = todo
  def compensate = todo
  
  def faultHandlers = todo
  
  
}
