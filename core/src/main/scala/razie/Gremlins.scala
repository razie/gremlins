package razie

import razie.gremlins.eng._

object Gremlins {
  var dflt: Option[Engine] = None

  /** cause all gremlins to die - throws exception if anyone still running 
   * 
   * @param timeout how many milisec to wait in case anyone is still running
   */
  def die(timeout: Int = 250) = synchronized { dflt map (_.stop(false, timeout)); dflt = None }
  
  /** cause all gremlins to die - 
   * 
   * it will kill any running process 
   * @return true if died nicely or false if it had to kill processes
   */
  def kill() = synchronized { 
    val ok = dflt map (_.stop(true))
    dflt = None 
    ok getOrElse true
    }
  
  /** allow gremlins to live in a default engine */
  def live() : Engine = liveInside (new Engine with Threads)
  
  /** allow gremlins to live in the given engine */
  def liveInside (e:Engine) : Engine = synchronized { 
    kill()
    dflt = Some(e)
    dflt.get 
    }
  
  def apply() : Engine = synchronized { val e = (dflt getOrElse live); if (e.stopped) live else e }

  def using (e: Engine) (work: => Any) = {
    razie.Gremlins.liveInside (e)
    val ret = work
    razie.Gremlins.die()
    ret
  }
}

