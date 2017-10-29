import org.scalatest.FunSuite
import Math.argMax

/**
  * Created by benjaminsmith on 10/29/17.
  */
class MathSpec extends FunSuite{
 test("argmax should return the index of the max int in a list"){
   assert(argMax(List(1,2,3,4)) == 3)
   assert(argMax(List(5,2,3,4)) == 0)
   assert(argMax(List(5,2,20,4,10)) == 2)
   assert(argMax(Nil) == -1)
 }
}
