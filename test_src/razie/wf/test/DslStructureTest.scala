package razie.wf.test

import org.scalatest.junit._
import razie.actionables._
import razie.actionables.library._

import razie.wf._

class DslStructureTest extends JUnit3Suite {
  import wf._ // the workflow constructs library

  //-------------------------------- if
 
  // TODO problem: welse needs to be on the same line as the closing  } of its own wif
  
  def wif1 = wif (_ == 1) {  // no good, really - condition not serializable
     wf.inc + wf.log ($0) 
   } welse { 
     wf.inc + wf.inc + wf.log ($0) 
   }
   
  def wif2 = wif ("$0 == 1") {  // good, condition serializable
     wf.inc + wf.log ($0) 
   } welse { 
     wf.inc + wf.log ($0) 
   }

  def testwif11 = expect (2) { wif1.print run 1 }
  def testwif12 = expect (4) { wif1.print run 2 }
  def testwif21 = expect (2) { wif2.print run 1 }
  def testwif22 = expect (3) { wif2.print run 2 } // TODO should be 4...currently not implemented properly :)

  // ---------------- seq
  
  def wseq1 = wf.seq(wf.inc, wf.log($0))
  def wseq2 = wf.nop + inc + set(5) + inc + wf.log($0)
  
  def testwseq1 = expect (2) { wseq1.print run 1 }
  def testwseq2 = expect (6) { wseq2.print run 1 }

  // ---------------- seq
    
  def wpar1 = wf.par(log(1), wf.log(2))

  def testwpar1  = expect (1::2::Nil) { razie.M.any (wpar1 run 1) anyOrder }

  def wpar2 = nop | (nop + inc + log($0)) | (inc + log($0))
  def wpar3 = inc + log($0) | inc + log($0)

  def testwpar2 = expect (1::2::2::Nil) { razie.M.any (wpar2.print run 1) anyOrder }
//  def testwpar3 = expect (2::2::Nil) { (wpar3 run 1) }

  //-------------------------- match/case
  
  def wm1 = wmatch2 ($0) {
    wcase2[Int] (1) {log ("m 1")} +
    wcase2[Int] (2) {log ("m 2")} +
    wcase2      ("Gigi") {log ("m Gigi")} +
    wcase2      (List(1,2,3)) {log ("m 1,2,3")} +
    wcase2      {l:Seq[Int] => l(2) == 2} {log ("matched list with secnod elem 2")} +
    wcaseany2   {log ("matched none")}
  }
    
//  def testw1 = expect ("m 1") { println (wm1.mkString); wm1 run 1 }
//  def testw2 = expect ("m 2") { wm1 run 2 }
//  def testw3 = expect ("m Gigi") { wm1 run "Gigi" }
//  def testw4 = expect ("m 1,2,3") { println (wm1.mkString); wm1 run List(1,2,3) }

}
