import org.scalatest.{FlatSpec, FunSuite}
import Math.{argMax, argMin}

/**
  * Created by benjaminsmith on 10/29/17.
  */
class MathSpec extends FlatSpec{
  behavior of "argMax"

  it should "find the max in an increasing list" in {
    assert(argMax(List(1, 2, 3, 4)).contains(3))
  }
  it should "find the first element if it is the biggest even if the list continues on" in {
    assert(argMax(List(5, 2, 3, 4)).contains(0))
  }
  it should "find the middle element if it is the biggest" in {
    assert(argMax(List(5, 2, 20, 4, 10)).contains(2))
  }
  it should "not return an index if the max element is duplicated in the list" in {
    assert(argMax(List(1,1)).isEmpty)
  }
  it should "not return an index if the list is empty" in {
    assert(argMax(Nil).isEmpty)
  }

  behavior of "argMin"
  it should "find the min element if it is the first element" in {
    assert(argMin(List(1, 2, 3, 4)).contains(0))
  }
  it should "find the min element if it is somewhere in the middle of the list" in {
    assert(argMin(List(5, 2, 3, 4)).contains(1))
  }
  it should "find negative numbers" in {
    assert(argMin(List(5, 2, 20, 4, 10, -5)).contains(5))
  }
  it should "return a None if the list is empty" in {
    assert(argMin(Nil).isEmpty)
  }
  it should "return a None if the min element is duplicated" in {
    assert(argMin(List(0,0)).isEmpty)

  }
}
