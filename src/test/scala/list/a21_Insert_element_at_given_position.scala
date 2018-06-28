package list

import scala.Int
import scala.Symbol
import org.scalacheck._
import scalaz._
import Scalaz._

import scala.annotation.tailrec

object a21_Insert_element_at_given_position extends Properties("Insert element at given position") {

  implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })
  property("") = {
    val expected: IList[Symbol] = IList('a, 'new, 'b, 'c, 'd)
    expected === insertAt('new, 1, IList('a, 'b, 'c, 'd))

    val expected2: IList[Symbol] = IList('a, 'b, 'new, 'c, 'd)
    expected2 === insertAt('new, 2, IList('a, 'b, 'c, 'd))
  }

  def insertAt(s: Symbol, n: Int, l: IList[Symbol]): IList[Symbol] = {
    @tailrec
    def insert(n: Int, l: IList[Symbol], acc: IList[Symbol]): IList[Symbol] = {
      if (n == 0) acc.reverse |+| s +: l
      else l match {
        case INil() => INil()
        case ICons(h, tail) => insert(n - 1, tail, h +: acc)
      }
    }
    insert(n, l, INil())
  }
}
