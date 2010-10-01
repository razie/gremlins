package razie

import razie.gremlins.eng._

object Gremlins {
  var dflt: Option[Engine] = None

  /** cause all gremlins to die */
  def die = synchronized { dflt map (_.stop()); dflt = None }
  
  /** allow gremlins to live in a default engine */
  def live : Engine = liveInside (new Engine with Threads)
  
  /** allow gremlins to live in the given engine */
  def liveInside (e:Engine) = synchronized { 
    die; 
    dflt = Some(e)
    dflt.get 
    }
  
  def apply() = synchronized { val e = (dflt getOrElse live); if (e.stopped) live else e }
}

