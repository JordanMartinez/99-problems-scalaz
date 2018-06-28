package list

import scala.Int
import scala.Symbol
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

import scala.annotation.tailrec

object a18_Extract_slice_from_list extends Properties("Extract slice from list") {

  property("") = {
    val expected = IList('d, 'e, 'f, 'g)
    implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })
    expected === slice(3, 7, IList('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  def slice(fromInclusive: Int, toExclusive: Int, l: IList[Symbol]): IList[Symbol] = {
    @tailrec
    def findEnd(n: Int, l: IList[Symbol], acc: IList[Symbol]): IList[Symbol] =
      if (n == 0) acc
      else l match {
        case INil() => INil()
        case ICons(h, tail) => findEnd(n - 1, tail, acc :+ h)
      }

    @tailrec
    def findStart(n: Int, list: IList[Symbol]): IList[Symbol] =
      if (n == 0) findEnd(toExclusive - fromInclusive, list, INil())
      else list match {
        case INil() => INil()
        case ICons(_, tail) => findStart(n - 1, tail)
    }
    findStart(fromInclusive, l)
  }
}
