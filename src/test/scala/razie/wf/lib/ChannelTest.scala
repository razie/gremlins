/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf.lib

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

import razie.wf._

class ChannelTest extends JUnit3Suite {
  import razie.wf.lib.PiCalc._
  import razie.wf.lib.PiCalc

  def t = 10000
  var c = Channel("c", 1)
  
  def myp = v(c) (wf.sleep(t) | wf.sleep(t))  // correct v(c) ( c(0) P | c<0> Q )
  def xtesta = expect (true) { 
    val res = razie.Timer {myp.print run "1"}
    println ("==================>>>>>>>>>>>>>"+res)
    res._1 < 2*t
  }
  
//  def myp41 = v(c) ((wf.sleep(t) + (c ? P)) | ((c ! Q) + wf.sleep(t)))  // correct v(c) ( c(0) P | c<0> Q )
  def myp4P = wf.sleep(t) + (c ? P)
  def myp4Q = (c ! Q) //+ wf.sleep(t)
//  def myp41 = v(c) ((wf.sleep(t) + (c ? P)) | ((Q) + wf.sleep(t)))  // correct v(c) ( c(0) P | c<0> Q )
  def myp41 = v(c) (myp4P | myp4Q)  // correct v(c) ( c(0) P | c<0> Q )
  def testasync = expect (true) { 
    val res = razie.Timer {myp41.print run "1"}
    println ("==================>>>>>>>>>>>>>"+res)
    res._1 < 2*t
  }
  
  Channel destroy "c"
  c = Channel("c", 0)
  def xtestsync = expect (true) { 
    val res = razie.Timer {myp41.print run "1"}
    println ("==================>>>>>>>>>>>>>"+res)
    res._1 > 2*t
  }
  
  
  override def setUp () = { Gremlins.live }
  override def tearDown () = { Gremlins.die }
}
