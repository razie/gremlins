/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.g.{GLoc, GRef}
import razie.GPath

trait WfHandle {
  val eng : GremlinWf
  val me : GRef
}

trait WPath // syntax marker
case class RWPath (loc:GLoc, root:GRef, path:String) extends GPath (path) {} // the real McCoy

/** trait shared by gremlins - lifecycle management, basically */
trait GremlinWf {
  // CRUD workflow lifecycle
  def c (defn:String, startV:Any) : GRef // create a workflow instance from definition
  def c (defn:WfAct, startV:Any) : GRef  // create a workflow instance from definition
  def r (id:GRef) : Option[WfAct]        // TODO optimize - don't make user load wf just for state
  def r_list () : List[GRef] // read all, curr state
  def u (who:GRef) // several types of udpate
  def d (who:GRef)
 
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

/** trait shared by gremlins - lifecycle management, basically */
class GremlinWfImpl extends GremlinWf {
  def c (defn:String, startV:Any) : GRef = Engines() create (wf(defn), razie.base.scripting.ScriptFactory.mkContext(), startV)
  def c (defn:WfAct, startV:Any) : GRef = null
  def r (id:GRef) : Option[WfAct] = None
  def r_list () : List[GRef] = List()
  def u (who:GRef) {}
  def d (who:GRef) {}
 
  // remote control
  def play (who:WPath)   = null
  def pause (who:WPath) : WPath= null
  def breakpoint (who:WPath) = null
  def stepOver (who:WPath) : WPath= null
  def stepInto (who:WPath) : WPath= null
  def stop (who:WPath) : WPath= null
  def rollback (who:WPath)   = null
  
  // travel api
  def supports (a:WfAct) : Boolean = false
  def receive (a:WfAct) : WfHandle = null
}

object Gremlins {
  var api = new GremlinWfImpl()
  
}

