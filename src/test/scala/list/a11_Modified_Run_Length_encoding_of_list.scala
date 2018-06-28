package list

import scala.Int
import scala.Symbol
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

object a11_Modified_Run_Length_encoding_of_list extends Properties("run-length encoding of list") {

  property("") = {
    val expected: IList[Symbol \/ (Int, Symbol)] = IList(
      \/-[Symbol, (Int, Symbol)]((4,'a)),
      -\/[Symbol, (Int, Symbol)]('b),
      \/-[Symbol, (Int, Symbol)]((2,'c)),
      \/-[Symbol, (Int, Symbol)]((2,'a)),
      -\/[Symbol, (Int, Symbol)]('d),
      \/-[Symbol, (Int, Symbol)]((4,'e))
    )
    implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })
    expected === pack(IList('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }


  def pack(l: IList[Symbol]): IList[Symbol \/ (Int, Symbol)] =
    l.foldRight(IList.empty[Symbol \/ (Int, Symbol)])((next, acc) =>
      acc match {
        case INil() => IList(-\/(next))
        case listTail@ICons(latestTuple, tail) => latestTuple match {
          case -\/(a) => if (a == next) ICons(\/-((2, next)), tail) else ICons(-\/(next), listTail)
          case \/-((i, a)) => if (a == next) ICons(\/-((i + 1, next)), tail) else ICons(-\/(next), listTail)
        }
      }
    )

}
