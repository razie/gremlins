/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.AA
import razie.base.{ActionContext => AC}
import razie.wf.lib._
import razie.wfs._

//--------------------- samples

object Wf5Main extends Application {
   import wf._

   var acc = ""

  // test
  lazy val if1 = wft.wuif (1==1) {
     acc += "it's "
     acc += "true "
     acc += "..."
  } 
  
  lazy val if2 = wft.wsif (1==2) {
     var lacc = ""
     lacc += "it's "
     lacc += "true "
     lacc += "..."
     lacc
  } welse 
     wft {println ("it's")} + 
     wft {println ("false")} +
     wft {println ("!!!")}
  
  lazy val if3a = wft.wsif (1==2) {
     var lacc = ""
     lacc += "it's "
     lacc += "true "
     lacc += "..."
     lacc
  } welse wft.wsif (3==2) {
     var lacc = ""
     lacc += "it's "
     lacc += "true "
     lacc += "..."
     lacc
  } welse 
     wft.wa {_.toString + " it's"} + 
     wft.wa {s:Any => s.toString + " false"} + 
     wft.wa {s:Any => s.toString + " ..."} 

  // the trouble with this is that the branches are only known as it runs...can't see the defn
  lazy val match1 = wft.wmatch1 ("stuka") {
     wft.wcase1 {case 1 => log ("matched 1")} +
     wft.wcase1 {case 2 => log ("matched 2")} +
     wft.wcase1 {case 3 => log ("matched 2")} +
     wft.wcase1 {case 4 => log ("matched 2")} +
     wft.wcase1 {case s:String => log ("matched s=" + s)} +
     wft.wcase1 {case l@List(1,2,3) => log ("matched list " + l)} +
     wft.wcaseany1 {log ("matched none")}
  }
  
  lazy val match2 = wmatch2 ("stuka") {
     wcase2[Int] (1) {log ("matched 1")} +
     wcase2[Int] (2) {log ("matched 2")} +
     wcase2      ("Gigi") {log ("matched Gigi")} +
//     wft.wcase2a {s:String => log ("matched s=" + s)} +
     wcase2      (List(1,2,3)) {log ("matched list 1,2,3")} +
     wcase2      {l:Seq[Int] => l(2) == 2} {log ("matched list with secnod elem 2")} +
//     wcase2a     {l:Seq[Int] => l == List(1,2,3)} {l:Seq[Int] => log ("matched list " + l)} +
     wcaseany2   {log ("matched none")}
  }
  
  this e if1
  this e if2
  this e if3a
  this e match1
  this e match2
  
  def e (w : WfActivity) = {
    acc = ""
    println ("")
    println ("Workflow is: " + w.mkString)
    acc += "Running: "
    println (">>>>>>>> RESULT is " + (w run ""))
    println (acc)
    println ("=========================================================")
  }
  
}
