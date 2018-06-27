import scala.Int
import scala.Symbol
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

import scala.annotation.tailrec

object a15_Duplicate_elements_in_list_N_times extends Properties("duplicate elements in list N times") {

  property("") = {
    val expected = IList('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })
    expected === duplicateN(3, IList('a, 'b, 'c, 'c, 'd))
  }

  def duplicateN(n: Int, l: IList[Symbol]): IList[Symbol] = {
    @tailrec
    def go(i: Int, a: Symbol, acc: IList[Symbol]): IList[Symbol] =
      if (i == 0) acc
      else go(i - 1, a, a :: acc)

    l.foldRight(IList.empty[Symbol])((next, acc) => go(n, next, acc))
  }
}
