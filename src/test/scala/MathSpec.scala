import org.scalatest.FunSuite
import Math.{argMax, argMin}

/**
  * Created by benjaminsmith on 10/29/17.
  */
class MathSpec extends FunSuite{
  test("argmax should return the index of the max int in a list"){
   assert(argMax(List(1, 2, 3, 4)).contains(3))
   assert(argMax(List(5, 2, 3, 4)).contains(0))
   assert(argMax(List(5, 2, 20, 4, 10)).contains(2))
   assert(argMax(Nil).isEmpty)
  }
  test("arg min should return the index of the min int in a list"){
    assert(argMin(List(1, 2, 3, 4)).contains(0))
    assert(argMin(List(5, 2, 3, 4)).contains(1))
    assert(argMin(List(5, 2, 20, 4, 10, -5)).contains(5))
    assert(argMin(Nil).isEmpty)
  }
}
