/**
  * Created by benjaminsmith on 11/8/17.
  */
object Math {
  // argMax returns a None if there are no elements in the list or if there is no sole max
  def argMax(l:Seq[Double]):Option[Int] = {
    val (maxes, i) = l.zipWithIndex.foldLeft((List(0.0),None:Option[Int])){ case (prev@(prevV, prevI), (vAndi)) =>
      val (value, i ) = vAndi
      prevV.headOption match {
        case Some( h ) if value == h => (List(h, value), Some(i)) // do this to keep track if there is more than one max
        case Some( h ) if value > h => (List(value), Some(i))
        case Some( h ) if value < h => (List(h), prevI)
        case None => (List( value ), Some(i))
      }
    }
    if (maxes.length > 1) None // if there were more than one maxes, then this function should return a None
    else i
  }

  // argMin returns a None if there are no elements in the list or if there is no sole min
  def argMin(l:Seq[Double]):Option[Int] = {
    l match {
      case Nil => None
      case h :: Nil => Some(0)
      case h :: t =>
        val (mins, i) = t.zipWithIndex.foldLeft((List(h), 0)) { case (prev@(prevV, prevI), (vAndi)) =>
          val (value, i) = vAndi
          prevV.headOption match {
            case Some( h ) if value == h => (List(h, value), i + 1) // do this to keep track if there is more than one max
            case Some( h ) if value < h => (List(value), i + 1)
            case Some( h ) if value > h => (List(h), prevI)
            case None => (List( value ), i + 1)
          }
        }

        if (mins.length > 1) None // if there were more than one maxes, then this function should return a None
        else Some(i)
    }
  }
}
