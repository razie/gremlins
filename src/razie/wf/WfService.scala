package razie.wf

trait WfService {
  def name : String
  
  def init1
  def init2
  
  def shutdown
 
  // TODO may have internal state
  // TODO should have transactions
  
  
}

