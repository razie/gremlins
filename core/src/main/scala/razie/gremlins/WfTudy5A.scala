/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.gremlins

import razie.g._

object GStudy5 {
  val a = Array.tabulate(100) {i => new GN5(i.toString)}
   
  val w1 =  a(0) --> a(1)
  val w11 = a(0) +-> a(5)
  
  val w2 = a(2) --> a(3) --> a(4)
  
  val w3 = a(0) ==> a(1) ==> a(2)
  
  val wif1 = a(10) --> Map( true -> a(11), false -> a(12))

  val vv = true -> a(31)
  
case class GL5 (a:GN5, z:GN5) extends GLink[GN5]
case class vGL5 (ia:GN5, iz:GN5, v:Any) extends GL5 (ia,iz)

class GN5 (val name : String) extends GNode[GN5, GL5] {
  var nodes : Seq[GN5] = Nil
  var links : Seq[GL5] = Nil
//  override def gnodes = nodes
  override def glinks = links

   /** reroute */
   def --> (z:GN5) = {
     links = GL5(this,z) :: Nil
     this
     }
   
   /** add a new dependency */
   def +-> (z:GN5) = {
      links = GL5 (this, z) :: links.toList
      this
   }
   
  /** par depy a -> (b,c) */
   def --> (z:Seq[GN5]) =
      if (links.isEmpty) {
         links = z.map (GL5(this,_)).toList
         this
      } else {
        new SubG (this,z) 
      }
   
   /** par depy a -> (b,c) */
   def --> [T <: Any] (z:Map[T,GN5]) =
      if (links.isEmpty) {
         links = z.map (p => vGL5(this,p._2, p._1)).toList
         this
      } else {
//        new SubG (this, z.map (p => vGL5(this,p.a, p.x))z) 
      }
   
   /** proxy on top of me */
   def ==> (z:GN5) = new GP5 (this,z::Nil) 

}
   
class GP5 (a:GN5,z:Seq[GN5]) extends GN5 ("GP-"+a.name) {
}

class SubG (a:GN5,z:Seq[GN5]) extends GN5 ("SubG") {
  nodes = a :: Nil
  
  def findEnds (z:GN5) = {
     Graphs.filterNodes[GN5, GL5](z) {a => a.links.isEmpty}   
  }
}


//  case class gif (c:String,t:GN5,f:GN5) = 
     
}
