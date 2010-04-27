package razie.wf.study5

import razie.AA
import razie.base.{ActionContext => AC}

//--------------------- samples

object Wf5Main extends Application {
   import wf._

   var acc = ""

  // test
  lazy val if1 = wuif (1==1) {
     acc += "it's "
     acc += "true "
     acc += "..."
  } 
  
  lazy val if2 = wif (1==2) {
     var lacc = ""
     lacc += "it's "
     lacc += "true "
     lacc += "..."
     lacc
  } welse 
     wf {println ("it's")} + 
     wf {println ("false")} +
     wf {println ("!!!")}
  
  lazy val if3a = wif (1==2) {
     var lacc = ""
     lacc += "it's "
     lacc += "true "
     lacc += "..."
     lacc
  } welse wif (3==2) {
     var lacc = ""
     lacc += "it's "
     lacc += "true "
     lacc += "..."
     lacc
  } welse 
     wa {_.toString + " it's"} + 
     wa {s:Any => s.toString + " false"} + 
     wa {s:Any => s.toString + " ..."} 

  // the trouble with this is that the branches are only known as it runs...can't see the defn
  lazy val match1 = wmatch1 ("stuka") {
     wcase1 {case 1 => log ("matched 1")} +
     wcase1 {case 2 => println ("matched 2")} +
     wcase1 {case 3 => println ("matched 2")} +
     wcase1 {case 4 => println ("matched 2")} +
     wcase1 {case s:String => println ("matched s=" + s)} +
     wcase1 {case l@List(1,2,3) => println ("matched list " + l)} +
     wcaseany1 {println ("matched none")}
  }
  
  lazy val match2 = wmatch2 ("stuka") {
     wcase2[Int] (1) {println ("matched 1")} +
     wcase2[Int] (2) {println ("matched 2")} +
     wcase2 ("Gigi") {println ("matched Gigi")} +
     wcase2a {s:String => println ("matched s=" + s)} +
     wcase2 (List(1,2,3)) {println ("matched list 1,2,3")} +
     wcase2  {l:Seq[Int] => l(2) == 2} {println ("matched list with secnod elem 2")} +
     wcase2a {l:Seq[Int] => l == List(1,2,3)} {l:Seq[Int] => println ("matched list " + l)} +
     wcaseany2 {println ("matched none")}
  }
  
  this e if1
  this e if2
  this e if3a
  this e match1
  this e match2
  
  def e (w : WfAct) = {
    acc = ""
    println ("")
    println ("Workflow is: " + w.mkString)
    acc += "Running: "
    println (">>>>>>>> RESULT is " + Engines().exec(w, razie.base.scripting.ScriptFactory.mkContext(), ""))
    println (acc)
    println ("=========================================================")
  }
  
}
