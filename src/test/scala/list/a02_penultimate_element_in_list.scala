package list

import scala.Option
import scala.None
import scala.Some
import scala.Int
import scalaz._
import Scalaz._
import org.scalacheck._

import scala.annotation.tailrec

object a02_penultimate_element_in_list extends Properties("Penultimate Element in List") {

  property("") = {
    val l = IList(1, 1, 2, 3, 5, 8)
    (Some(2) : Option[Int]) === nth(2, l)
  }

  @tailrec
  def nth[A](index: Int, l: IList[A]): Option[A] = (index, l) match {
    case (_, INil()) => None
    case (0, ICons(h, _)) => Some(h)
    case (_, ICons(h@_, tail)) => nth(index - 1, tail)
  }
}
