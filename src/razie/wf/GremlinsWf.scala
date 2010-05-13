/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.g.GLoc
import razie.GPath

trait WfId

trait WfHandle {
  val eng : GremlinWf
  val me : WfId
}

trait WPath // syntax marker
case class RWPath (loc:GLoc, root:WfId, path:String) extends GPath (path) {} // the real McCoy

/** trait shared by gremlins - lifecycle management, basically */
trait GremlinWf {
  // workflow lifecycle
  def c () // TODO what signature?
  def r () // read all, curr state
  def u (who:WfId) // several types of udpate
  def d (who:WfId)
 
  // remote control
  // TODO should these be asynchronous or both versions?
  def play (who:WPath)   
  def pause (who:WPath) : WPath
  def breakpoint (who:WPath) 
  def stepOver (who:WPath) : WPath
  def stepInto (who:WPath) : WPath
  def stop (who:WPath) : WPath
  def rollback (who:WPath)   
  
  // travel api
  def supports (a:WfAct) : Boolean
  def receive (a:WfAct) : WfHandle
}
