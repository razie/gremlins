/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins

import org.scalatest.junit._
import razie.wf
import razie.wf._
import razie.Gremlins

class PersistTest extends JUnit3Suite {
  import razie.g.Graphs._
  
  def wpl1 = wf.log($0)
  def testwif23s = expect (2) { p(wpl1) run 2 }

  def p (a:WfActivity) : WfActivity = {
    println (wf.toDsl(a))
    val persisted = persist (a)
    println (">>>PERSISTED:\n" + persisted)
    val ret = unpersist (persisted)
    println (wf.toDsl(ret))
    ret
  }

  def persist (a:WfActivity) = "1"
  def unpersist (s:Any) :WfActivity = wf.log($0)
    
  //-------------------------------- if
 
  // TODO problem: welse needs to be on the same line as the closing  } of its own wif
  
//  def wif3s = """if ($0 == 1) then inc else log ($0)"""
//  def testwif23s = expect (2) { wf(wif3s) run 2 }

  // ---------------- seq

  /*
  def wseq1 = wf.seq(wf.inc, wf.log($0))
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
    
  def wpar1 = wf.par(log(1), wf.log(2))

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

  def testwpar2 = expect (1::2::2::Nil) { razie.M.anyOrder (wpar2.print run 1) }
  def testwpar3 = expect (2::2::Nil) { (wpar3 run 1) }
  def testwpar2s = expect (1::2::2::Nil) { razie.M.anyOrder (wf(wf toDsl wpar2.print) run 1) }
  def testwpar4 = expect (2::2::Nil) { (wf(wpar4).print run 1) }
  def testwpar5 = expect (2::2::Nil) { (wf(wpar5).print run 1) }

  // make sure it's actually ran in paralel
  def wparp = sleep(1000) | sleep(1000)
  def testwparp = expect (true) { val i1 = System.currentTimeMillis; wparp.print run 1; System.currentTimeMillis - i1 < 1500 }
  
  
  //-------------------------- match/case
  
  def wm1 = wmatch2 ($0) {
    wcase2[Int] (1) {log ("m 1")} +
    wcase2[Int] (2) {log ("m 2")} +
    wcase2      ("Gigi") {log ("m Gigi")} +
    wcase2      (List(1,2,3)) {log ("m 1,2,3")} +
    wcaseany2   {log ("matched none")}
  }
    
  def testw1 = expect ("m 1") { wm1.print run 1 }
  def testw2 = expect ("m 2") { wm1 run 2 }
  def testw3 = expect ("m Gigi") { wm1 run "Gigi" }
  def testw4 = expect ("m 1,2,3") { wm1.print run List(1,2,3) }

  val inc1 = inc
  def skip1 = inc + cancel(inc1) + inc1 + inc
  def testskip = expect (3) { skip1.print run 1 }

  def wt1 = timeout (1000) { sleep(5000) }
  def testwt1 = expect (true) { razie.Timer { wt1.print run 1 } ._1 < 2000 }
*/

  override def setUp () = { Gremlins.live }
  override def tearDown () = { Gremlins.die }
}
