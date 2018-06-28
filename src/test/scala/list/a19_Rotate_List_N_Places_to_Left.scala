package list

import scala.Int
import scala.Symbol
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

import scala.annotation.tailrec

object a19_Rotate_List_N_Places_to_Left extends Properties("Rotate N places") {

  implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })

  property("rotate 3 to left") = {
    val expected = IList('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    expected === rotate(3, IList('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  property("rotate 2 to right") = {
    val expected = IList('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    expected === rotate(-2, IList('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  def rotate(leftAmount: Int, l: IList[Symbol]): IList[Symbol] = {
    @tailrec
    def rotateLeft(n: Int, l: IList[Symbol], acc: IList[Symbol]): IList[Symbol] =
      if (n == 0) l |+| acc
      else l match {
        case INil() => INil()
        case ICons(h, tail) => rotateLeft(n - 1, tail, acc :+ h)
      }

    @tailrec
    def rotateRight[A](n: Int, tail: IList[A], heads: IList[A]): IList[A] = {
      tail match {
        case INil() => INil()
        case ICons(_, _) if n == 0 => tail |+| heads.reverse
        case ICons(h, t) => rotateRight(n - 1, t, h +: heads)
      }
    }

    if (leftAmount > 0) rotateLeft(leftAmount, l, INil())
    else rotateRight(leftAmount + l.length, l, INil())
  }
}
