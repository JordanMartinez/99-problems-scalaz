package list

import scala.Symbol
import scala.Int
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

import scala.annotation.tailrec

object a17_Split_List_into_Two extends Properties("Split list into two") {

  property("") = {
    val expected = (IList('a, 'b, 'c), IList('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })
    expected === split(3, IList('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  def split(n: Int, l: IList[Symbol]): (IList[Symbol], IList[Symbol]) = {
    @tailrec
    def go[A](n: Int, right: IList[A], left: IList[A]): (IList[A], IList[A]) = {
      if (n == 0) (left.reverse, right) else right match {
        case INil() => (INil(), INil())
        case ICons(h, tail) => go(n - 1, tail, h +: left)
      }
    }
    go(n, l, INil())
  }
}
