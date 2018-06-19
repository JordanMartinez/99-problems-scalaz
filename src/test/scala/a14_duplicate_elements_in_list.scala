import scala.Symbol
import org.scalacheck._
import scalaz._
import Scalaz._

object a14_duplicate_elements_in_list extends Properties("duplicate elements in list") {

  property("") = {
    val expected = IList('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })
    expected === duplicate(IList('a, 'b, 'c, 'c, 'd))
  }

  def duplicate(l: IList[Symbol]): IList[Symbol] =
    l.foldRight(IList.empty[Symbol])((next, acc) => next :: next :: next :: acc)
}
