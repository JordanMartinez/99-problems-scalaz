package list

import scala.Int
import scala.Symbol
import org.scalacheck._
import scalaz._
import Scalaz._

object a16_Drop_every_Nth_element_from_list extends Properties("duplicate elements in list N times") {

  property("") = {
    val expected = IList('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })
    expected === drop(3, IList('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  def drop(n: Int, l: IList[Symbol]): IList[Symbol] = {
    val index = n - 1
    l.foldLeft((IList.empty[Symbol], index))((acc, next) =>
      if (acc._2 == 0) (acc._1, index)
      else (acc._1 :+ next, acc._2 - 1)
    )._1
  }
}
