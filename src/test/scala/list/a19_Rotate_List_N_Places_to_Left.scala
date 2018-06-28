package list

import scala.Int
import scala.Symbol
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

import scala.annotation.tailrec

object a19_Rotate_List_N_Places_to_Left extends Properties("Extract slice from list") {

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
    def rotateLeft(n: Int, l: IList[Symbol], rotatedAcc: IList[Symbol]): IList[Symbol] =
      if (n == 0) l ++ rotatedAcc
      else l match {
        case INil() => INil()
        case ICons(h, tail) => rotateLeft(n - 1, tail, rotatedAcc :+ h)
      }

    def rotateRight(n: Int, l: IList[Symbol]): IList[Symbol] = {
      val (_, front, back) = l.foldRight((n, IList.empty[Symbol], IList.empty[Symbol]))((next, acc) => {
        if (acc._1 > 0) {
          (acc._1 - 1, acc._2, ICons(next, acc._3))
        } else {
          (acc._1, next +: acc._2, acc._3)
        }
      })
      back |+| front
    }

    if (leftAmount > 0) rotateLeft(leftAmount, l, INil())
    else rotateRight(-leftAmount, l)
  }
}
