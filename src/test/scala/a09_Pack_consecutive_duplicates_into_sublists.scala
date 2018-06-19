import scala.Symbol
import org.scalacheck._
import scalaz._
import Scalaz._

object a09_Pack_consecutive_duplicates_into_sublists extends Properties("eliminate consecutive duplicates") {

  property("") = {
    val expected: IList[NonEmptyList[Symbol]] = IList(
      NonEmptyList('a, 'a, 'a, 'a),
      NonEmptyList('b),
      NonEmptyList('c, 'c),
      NonEmptyList('a, 'a),
      NonEmptyList('d),
      NonEmptyList('e, 'e, 'e, 'e)
    )
    implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })
    expected === pack(IList('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }


  def pack(l: IList[Symbol]): IList[NonEmptyList[Symbol]] =
    l.foldRight(IList.empty[NonEmptyList[Symbol]])((next, acc) =>
      acc match {
          // outer list
        case INil() => IList(NonEmptyList(next))
        case listTail@ICons(latestList, tail) =>
          if (latestList.head == next) ICons(next <:: latestList, tail)
          else ICons(NonEmptyList(next), listTail)
      }
    )

}
