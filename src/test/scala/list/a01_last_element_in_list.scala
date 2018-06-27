import scala.Option
import scala.None
import scala.Some
import scalaz._
import Scalaz._
import org.scalacheck._

import scala.annotation.tailrec

object a01_last_element_in_list extends Properties("Last Element in List") {

  property("last is last") = {
    val l = IList(1, 1, 2, 3, 5, 8)
    l.lastOption === last(l)
  }

  @tailrec
  def last[A](l: IList[A]): Option[A] = l match {
    case INil() => None
    case ICons(h, INil()) => Some(h)
    case ICons(h@_, tail) => last(tail)
  }
}
