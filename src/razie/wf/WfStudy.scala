/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wf

import razie.AA
import razie.base.{ActionContext => AC}
import razie.wf.lib._

//--------------------- samples

object Wf5Main extends Application {
   import wf._

   var acc = ""

  // test
  lazy val if1 = wfs.wuif (1==1) {
     acc += "it's "
     acc += "true "
     acc += "..."
  } 
  
  lazy val if2 = wfs.wsif (1==2) {
     var lacc = ""
     lacc += "it's "
     lacc += "true "
     lacc += "..."
     lacc
  } welse 
     wfs {println ("it's")} + 
     wfs {println ("false")} +
     wfs {println ("!!!")}
  
  lazy val if3a = wfs.wsif (1==2) {
     var lacc = ""
     lacc += "it's "
     lacc += "true "
     lacc += "..."
     lacc
  } welse wfs.wsif (3==2) {
     var lacc = ""
     lacc += "it's "
     lacc += "true "
     lacc += "..."
     lacc
  } welse 
     wfs.wa {_.toString + " it's"} + 
     wfs.wa {s:Any => s.toString + " false"} + 
     wfs.wa {s:Any => s.toString + " ..."} 

  // the trouble with this is that the branches are only known as it runs...can't see the defn
  lazy val match1 = wfs.wmatch1 ("stuka") {
     wfs.wcase1 {case 1 => log ("matched 1")} +
     wfs.wcase1 {case 2 => log ("matched 2")} +
     wfs.wcase1 {case 3 => log ("matched 2")} +
     wfs.wcase1 {case 4 => log ("matched 2")} +
     wfs.wcase1 {case s:String => log ("matched s=" + s)} +
     wfs.wcase1 {case l@List(1,2,3) => log ("matched list " + l)} +
     wfs.wcaseany1 {log ("matched none")}
  }
  
  lazy val match2 = wmatch2 ("stuka") {
     wcase2[Int] (1) {log ("matched 1")} +
     wcase2[Int] (2) {log ("matched 2")} +
     wcase2      ("Gigi") {log ("matched Gigi")} +
//     wfs.wcase2a {s:String => log ("matched s=" + s)} +
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
    println (">>>>>>>> RESULT is " + Engines().exec(w, razie.base.scripting.ScriptFactory.mkContext(), ""))
    println (acc)
    println ("=========================================================")
  }
  
}
