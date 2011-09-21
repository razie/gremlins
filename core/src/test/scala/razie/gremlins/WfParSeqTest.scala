/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._
import razie.Gremlins

class WfParSeqTest extends JUnit3Suite {
  import razie.wf
  import razie.wf._

  // ---------------- seq
  
  def wseq1 = wf.seq(wf.inc :: wf.log($0) :: Nil)
  def wseq2 = wf.nop + inc + set(5) + inc + wf.log($0)
  def w2s = """seq { nop; inc; log($0) }"""
  def w3s = """seq { seq { nop; inc; log($0) }; seq { nop; inc; log($0) } }"""
  def w1m = """
nop
inc
log($0)
"""
  
  def testwseq1 = expect (2) { wseq1.print run 1 }
  def testwseq2 = expect (6) { wseq2.print run 1 }
  def testwseq1s = expect (2) { wf(wf toDsl wseq1) run 1 }
  def testw2s = expect (2) { wf(w2s) run 1 }
  def testw3s = expect (3) { wf(w3s) run 1 }
  def testw1m = expect (2) { wf(w1m) run 1 }

  // ---------------- par
    
  def wpar1 = wf.par(log(1) :: wf.log(2) :: Nil)

  def testwpar1  = expect (1::2::Nil) { razie.M.anyOrder (wpar1.print run 1) }

  def wpar2 = nop | (nop + inc + log($0)) | (inc + log($0))
  def wpar3 = inc + log($0) | inc + log($0)
  def wpar4 = """par { seq { inc; log($0) }; seq { inc; log($0) } }"""
  def wpar5 = """
par { 
  seq {
  	 inc
  	 log($0)
  	 } 
  seq { 
    inc
    log($0)
    }
  }"""
    
    // note this may get tricky...should test it every time you use it
  def wpar6 =
  seq {
  	 inc
  	 log($0)
  	 } 

  def wpar7 =
par { 
  seq {
  	 inc
  	 log($0)
  	 } 
  seq { 
    inc
    inc
    log($0)
    }
  }


  def testwpar2 = expect (1::2::2::Nil) { razie.M.anyOrder (wpar2.print run 1) }
  def testwpar3 = expect (2::2::Nil) { (wpar3 run 1) }
  def testwpar2s = expect (1::2::2::Nil) { razie.M.anyOrder (wf(wf toDsl wpar2.print) run 1) }
  def testwpar4 = expect (2::2::Nil) { (wf(wpar4).print run 1) }
  def testwpar5 = expect (2::2::Nil) { (wf(wpar5).print run 1) }
  def testwpar6 = expect (2) { wf(wf toDsl wpar6.print) run 1 }
  def testwpar7 = expect (2::3::Nil) { razie.M.anyOrder (wf(wf toDsl wpar7.print) run 1) }

  // make sure it's the last test... - tests that there's no running processes
  def testDie = expect (true) { Gremlins.kill() }

  override def setUp () = { Gremlins.live() }
  override def tearDown () = { Gremlins.die() }
}
