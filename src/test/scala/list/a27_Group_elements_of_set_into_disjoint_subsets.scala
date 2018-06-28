package list

import scala.{Int, StringContext}

import org.scalacheck.Prop._
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

import scala.annotation.tailrec

object a27_Group_elements_of_set_into_disjoint_subsets extends Properties(
  "Group elements of a set into disjoint subsets") {

  property("") = {
    val sizeList = IList(1, 3, 2)
    val originalList = IList("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    val result = group(sizeList, originalList)

    // print results using hack
    result.map(group => java.lang.System.out.println(s"One group: $group"))

    result.length === originalList.length
    val expectedLength = sizeList.length
    result.all(group => group.length === expectedLength)
  }

  def group[A](sizes: IList[Int], l: IList[A]): IList[IList[IList[A]]] = {
    // Note: this code is only in case one happens to input in invalid "sizes" argument
    // --------------------------------------------------------------------------------
    val listSlots = l.length
    val desiredSlots = sizes.suml
    if (listSlots < desiredSlots)
      scala.sys.error(s"Your request wants $desiredSlots total people but original list only has $listSlots (${desiredSlots - listSlots} additional people required)")
    // --------------------------------------------------------------------------------

    def getGroup[B](sizes: IList[Int], acc: IList[B], l: IList[B]): IList[IList[B]] = {
      (l, sizes) match {
        case (INil(), _) => INil()

        // final part found, return but without the empty tail
        case (ICons(h, INil()), ICons(1, INil())) => IList((h +: acc).reverse)
        // final part found, return
        case (ICons(h, tail), ICons(1, INil())) => IList((h +: acc).reverse, tail)
        // current part found and finished, reset loop to build next part
        case (ICons(h, tail), ICons(1, sizeTail)) => IList((h +: acc).reverse) |+| getGroup(sizeTail, INil(), tail)
        // recursive case: building accumulation for current part
        case (ICons(h, tail), ICons(current, sizeTail)) => getGroup[B](ICons(current - 1, sizeTail), h +: acc, tail)
      }
    }

    // NOTE: NOT STACK SAFE | TODO: Make Stack-Safe
    def rotateAndConcat(n: Int, l: IList[A]): IList[IList[IList[A]]] = {
      if (n == 0) IList(getGroup(sizes, INil(), l))
      else rotateAndConcat(n - 1, rotateLeft(1, l)) |+| IList(getGroup(sizes, INil(), l))
    }

    rotateAndConcat(l.length - 1, l)
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