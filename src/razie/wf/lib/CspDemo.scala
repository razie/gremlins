/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.lib

import razie.AA
import razie.base.{ActionContext => AC}
import razie.wf._
import razie.wf.res._
import razie.g._

object SomePiSamples extends Application {
  import PiCalc._
 
  def P = log($0 + "-P")
  def Q = log($0 + "-Q")
  def c = Channel("c") // channel c
  
  def myp02 = v(c) (c ? P | c ! Q)  // correct v(c) ( c(0) P | c<0> Q )
  
  println (wf toDsl myp02)
  myp02.print 
  myp02 run "1"
  
  Engines().stop
}

