package razie.wf

trait WfId

trait WfHandle {
  val eng : GremlinWf
  val me : WfId
}

/** trait shared by gremlins - lifecycle management, basically */
trait GremlinWf {
  // workflow lifecycle
  def c ()   
  def r () // read all, curr state
  def u () // several types of udpate
  def d ()   
 
  // remote control
  def play (who:WfId)   
  def pause (who:WfId)   
  def breakpoint (who:WfId)   
  def stepOver (who:WfId)   
  def stepInto (who:WfId)   
  def stop (who:WfId)   
  
  // travel api
  def supports (a:WfAct) : Boolean
  def receive (a:WfAct) : WfHandle
}