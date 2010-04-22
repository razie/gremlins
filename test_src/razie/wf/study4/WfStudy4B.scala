package razie.wf.study4

object WfStudy4B {

class wa {}

def wf (f : => Unit) = new wa
def wflog (f : => String) = new wa

class pnwc(f : => PartialFunction[Any, wa]) extends wa { 
   def + (b:pnwc) : plnwc = new plnwc (List(this) ::: List(b))
   def + (b:List[pnwc]) : plnwc = new plnwc(List(this) ::: b)
}

class plnwc (val l : List[pnwc]) extends wa {
  def + (b:pnwc) : plnwc = new plnwc (l ::: List(b))
}

//def wmatch (t: =>Any) (a:Seq[nwc[_ <: Any]]) = nwfm(() => t, a)
def wmatch (t: =>Any) (l:plnwc) = nwfm(() => t, l.l)
//def wcase[T] (t:T) (a:wa) = new nwc[T](t)(a)

abstract class nwfmb extends wa {
  val expr : () => Any
  val branches : Seq[pnwc]
}

case class nwfm (
      val expr : () => Any, 
      val branches : Seq[pnwc]
      ) extends nwfmb {
}

//class nif (cond: => Boolean, a:wa, b:wa) extends nwfmb {
//  override val expr = () => cond
//  override val branches = wocase (true) (a) :: wocase(false) (b) :: Nil
//}

//def wocase[T] (t:T) (f: => Unit) = new nwc[T](t)(wf(f))
def wcase (f: => PartialFunction[Any, wa]) = new pnwc(f)
//def wcase[T] (t:T) = new nwc[T](t)(null)

case class Student(val s:String)

//  val test1 : wa = 
//     wmatch (34) {
//       wcase[Int] (45) ({ }) ::
//       wcase[String] ("1234") ({ }) :: 
//       wcase[Student] (Student("1234")) ({ }) :: 
//       Nil
//  }

  val test2 : wa = 
     wmatch (34) {
       wcase { case i:Int => wflog ("i")} +
       wcase { case s:String => wflog ("s") } +
       wcase { case Student(t) => wflog ("t") }
      }


}