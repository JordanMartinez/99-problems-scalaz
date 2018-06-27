import scala.Option
import scala.None
import scala.Some
import scala.Int
import scalaz._
import Scalaz._
import org.scalacheck._

import scala.annotation.tailrec

object a03_Find_kth_element_in_list extends Properties("Kth Element in List") {

  property("last is last") = {
    val l = IList(1, 1, 2, 3, 5, 8)
    (Some(5): Option[Int]) === penultimate(l)
  }

  @tailrec
  def penultimate[A](l: IList[A]): Option[A] = l match {
    case INil() => None
    case ICons(h@_, INil()) => None
    case ICons(h, ICons(_, INil())) => Some(h)
    case ICons(h@_, tail) => penultimate(tail)

  }
}
