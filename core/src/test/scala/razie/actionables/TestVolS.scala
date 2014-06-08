object TestVolS {
  @volatile var x = 5
  (0 to 10 toList).par foreach { t =>
    var i:Int = 0; while (i < 100) {
      x += 1
      i += 1
    }
  }
  
  def main (argv:Array[String]) {
    println (x)
  }
}