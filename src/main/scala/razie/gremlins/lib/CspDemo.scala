/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins.lib

import razie.AA
import razie.base.{ActionContext => AC}
import razie.gremlins._
import razie.gremlins.res._
import razie.g._
import razie.wf
import razie.gremlins.act._
import razie.Gremlins

object SimplestPiSample extends Application {
  import PiCalc._
 
  def P = wf.log($0 + "-P")
  def Q = wf.log($0 + "-Q")
  def c = Channel("c", 0) // channel c
  
  def myp02 = v(c) (c ? P | c ! Q)  // correct v(c) ( c(0) P | c<0> Q )
  
  println (wf toDsl myp02)
  myp02.print 
  println ("RRRRRRRRRRRRRESULT is: " + (myp02 run "1"))
  
  Gremlins.die
}

object CspSamples {
  import PiCalc._

  def c = Channel("c", 0) // channel c
  def x = $("x") // variable x

  // 1. sequence
  def csp1 = O --> O

  // 2. parallel (interleaving)
  def csp2 = O | O

  // 3. deterministic choice (match/case)

  // 4. TODO
}

object PiSamples extends Application {
  import PiCalc._
 
  def c = Channel("c", 0) // channel c
  def x = $("x") // variable x
  
  def myp01 = v(c) (P | Q)
  checkwf ("01", "1", myp01) {_.asInstanceOf[List[_]] contains "1-P"}
  
  def myp02 = v(c) (c ? P | c ! Q)  // correct v(c) ( c(0) P | c<0> Q )
  def myp41 = v(c) (c.get($0) --> P | c.put($0) <-- Q)  // correct v(c) ( c(0) P | c<0> Q )
  checkwf ("02", "1", myp02) {_.asInstanceOf[List[_]] contains "1-Q-P"}
  checkwf ("41", "1", myp41) {_.asInstanceOf[List[_]] contains "1-Q-P"}
  
  def myp11 = v(c) + P  // correct (v c) P
  checkwf ("11", "1", myp11) {_ == "1-P"}
  
  (v(c) ( c.put($0)) ).print

  def myp21 = v(c) ( c.put($0) --> P ) // never ends since c.size == 0
  def myp22 = v(c) ( c -<- $0 + P ) // never ends since c.size == 0
  def myp23 = v(c)
//  checkwf ("21", "1", myp21) {_ == "1-P"}
//  checkwf ("22", "1", myp22) {_ == "1-P"}
  checkwf ("23", "1", myp23) {_ == "1"}

  // channel exists, read+P : P waits to read a value and then continues
  def myp31  = c($0) + P        // correct x(0) P
  def myp32 = c ->- $0 + P  // correct x(0) P
//  require (((c.put(4) --> myp31).print run "1") == "4-P")
//  require (((c.put(4) --> myp32).print run "1") == "4-P")
 
  def checkwf (name:String, start:Any, root:WfActivity) (verify : Any => Boolean) = {
    println (">>>>>>>>>>>>>>>> Start test " + name)
    println (wf toDsl root)
    if (! verify ((wf(wf toDsl root).print run start)))
      throw new IllegalArgumentException ("Require failed on wf: " + (wf toDsl root))
  }

  Gremlins.die
}


