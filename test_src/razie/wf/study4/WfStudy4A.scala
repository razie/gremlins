package razie.wf.study4

object WfStudy4A {

class wa {}

def wf (f : => Unit) = new wa

class nwc[T <: Any] (val t : T) (a:wa)  extends wa { 
   def + (b:nwc[_ <: Any]) : lnwc = new lnwc (List(this) ::: List(b))
   def + (b:List[nwc[_ <: Any]]) : lnwc = new lnwc(List(this) ::: b)
}

class lnwc (val l : List[nwc[_ <: Any]]) extends wa {
  def + (b:nwc[_ <: Any]) : lnwc = new lnwc (l ::: List(b))
}

//def wmatch (t: =>Any) (a:Seq[nwc[_ <: Any]]) = nwfm(() => t, a)
def wmatch (t: =>Any) (l:lnwc) = nwfm(() => t, l.l)
//def wcase[T] (t:T) (a:wa) = new nwc[T](t)(a)

abstract class nwfmb extends wa {
  val expr : () => Any
  val branches : Seq[nwc[_]]
}

case class nwfm (
      val expr : () => Any, 
      val branches : Seq[nwc[_]]
      ) extends nwfmb {
}

class nif (cond: => Boolean, a:wa, b:wa) extends nwfmb {
  override val expr = () => cond
  override val branches = wcase (true) (a) :: wcase(false) (b) :: Nil
}

def wcase[T] (t:T) (f: => Unit) = new nwc[T](t)(wf(f))
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
       wcase[Int] (45) { } +
       wcase[Int] (45) { } +
       wcase[Int] (45) { } +
       wcase ("1234") { } +
       wcase ("1234".r) { } +
       wcase (Student("1234")) { } 
      }


}