import scala.Symbol
import scala.Int
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

import scala.annotation.tailrec

object a12_Decode_run_length_list extends Properties("run-length decoding of list") {

  property("") = {
    val expected: IList[Symbol] = IList('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val coded: IList[Symbol \/ (Int, Symbol)] = IList(
      \/-[Symbol, (Int, Symbol)]((4,'a)),
      -\/[Symbol, (Int, Symbol)]('b),
      \/-[Symbol, (Int, Symbol)]((2,'c)),
      \/-[Symbol, (Int, Symbol)]((2,'a)),
      -\/[Symbol, (Int, Symbol)]('d),
      \/-[Symbol, (Int, Symbol)]((4,'e))
    )
    implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })
    expected === decode(coded)
  }

  def decode(l: IList[Symbol \/ (Int, Symbol)]): IList[Symbol] = {
    @tailrec
    def go(i: Int, n: Symbol, acc: IList[Symbol]): IList[Symbol] =
      if (i == 0) acc
      else go(i - 1, n, n :: acc)

    l.foldRight(IList.empty[Symbol])((next, acc) => next match {
      case -\/(a) => a :: acc
      case \/-((i, a)) => go(i, a, acc)
    })
  }

}
