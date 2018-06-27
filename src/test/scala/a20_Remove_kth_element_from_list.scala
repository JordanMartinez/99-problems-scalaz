import scala.Int
import scala.Symbol
import org.scalacheck._
import scalaz._
import Scalaz._
import scala.{Option, Some, None}

import scala.annotation.tailrec

object a20_Remove_kth_element_from_list extends Properties("Remove kth element from list") {

  implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })

  property("") = {
    val expected: (IList[Symbol], Option[Symbol]) = (IList('a, 'c, 'd), Some('b))
    expected === removeAt(1, IList('a, 'b, 'c, 'd))
  }

  def removeAt(n: Int, l: IList[Symbol]): (IList[Symbol], Option[Symbol]) = {
    @tailrec
    def remove(n: Int, l: IList[Symbol], value: Option[Symbol], acc: IList[Symbol]): (IList[Symbol], Option[Symbol]) = {
      if (n == 0) (acc.reverse |+| l, value)
      else l match {
        case INil() => (acc, None)
        case ICons(h, tail) if n == 1 => remove(n - 1, tail, Some(h), acc)
        case ICons(h, tail) => remove(n - 1, tail, None, h +: acc)
      }
    }
    remove(n + 1, l, None, INil())
  }
}
