package razie.dsl.test

import org.scalatest.junit._
import razie.dsl._

/** simple collector example */
class DslCollectorTest extends JUnit3Suite {

  case class A(i: Int) {
    // assume there is a collector and collect myself
    DslCollector.current.map { _ collect this }
  }

  def test1 = expect(A(1) :: A(2) :: Nil) {
    val collected = new collection.mutable.ListBuffer[Any]()

    DslCollector.collect { collected += _ }(1) { // start a collector level
      A(1) //collects itself
      A(2) //collects itself
    } // collector ends

    collected
  }
}

