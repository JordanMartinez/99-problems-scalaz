package list

import scala.Int

import org.scalacheck.Prop._
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

import scala.annotation.tailrec

object a27_Group_elements_of_set_into_disjoint_subsets extends Properties(
  "Group elements of a set into disjoint subsets") {

  property("") = {
    val originalList = IList("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    val result = group(2, 3, originalList)

    // print results using hack
    result.map(group => java.lang.System.out.println("One group: " + group))

    result.length === originalList.length
    result.all(group => group.length === 3)
  }

  def group[A](firstSize: Int, secondSize: Int, l: IList[A]): IList[IList[IList[A]]] = {
    @tailrec
    def getSecond[C](n: Int, acc: IList[C], l: IList[C]): IList[IList[C]] = {
      l match {
        case INil() => INil()
        case ICons(h, tail) if n == 0 => IList((h +: acc).reverse, tail)
        case ICons(h, tail) => getSecond[C](n - 1, h +: acc, tail)
      }
    }

    @tailrec
    def getFirst[B](n: Int, second: Int, acc: IList[B], l: IList[B]): IList[IList[IList[B]]] = {
      l match {
        case INil() => INil()
        case ICons(h, tail) if n == 0 => IList(IList((h +: acc).reverse) |+| getSecond[B](second, INil(), tail))
        case ICons(h, tail) => getFirst[B](n - 1, second, h +: acc, tail)
      }
    }

    // NOTE: NOT STACK SAFE | TODO: Make Stack-Safe
    def rotateAndConcat(n: Int, l: IList[A]): IList[IList[IList[A]]] = {
      if (n == 0) getFirst(firstSize - 1, secondSize - 1, INil(), l)
      else getFirst(firstSize - 1, secondSize - 1, INil(), l) |+| rotateAndConcat(n - 1, rotateLeft(1, l))
    }

    rotateAndConcat(l.length - 1, l).reverse
  }

  def rotateLeft[A](n: Int, l: IList[A]): IList[A] = {
    @tailrec
    def go(n: Int, l: IList[A], rotatedAcc: IList[A]): IList[A] =
      if (n == 0) l |+| rotatedAcc.reverse
      else l match {
        case INil() => INil()
        case ICons(h, tail) => go(n - 1, tail, h +: rotatedAcc)
      }
    go(n, l, INil())
  }

}