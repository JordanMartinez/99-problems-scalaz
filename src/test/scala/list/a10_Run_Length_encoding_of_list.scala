import scala.Int
import scala.Symbol
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

object a10_Run_Length_encoding_of_list extends Properties("run-length encoding of list") {

  property("") = {
    val expected: IList[(Int, Symbol)] = IList(
      (4,'a),
      (1,'b),
      (2,'c),
      (2,'a),
      (1,'d),
      (4,'e)
    )
    implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })
    expected === pack(IList('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }


  def pack(l: IList[Symbol]): IList[(Int, Symbol)] =
    l.foldRight(IList.empty[(Int, Symbol)])((next, acc) =>
      acc match {
          // outer list
        case INil() => IList((1, next))
        case listTail@ICons(latestTuple, tail) =>
          if (latestTuple._2 == next) ICons((latestTuple._1 + 1, next), tail)
          else ICons((1, next), listTail)
      }
    )

}
