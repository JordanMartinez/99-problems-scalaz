package list

import scala.Int
import scala.Symbol
import org.scalacheck.Prop._
import org.scalacheck._
import scalaz._
import Scalaz._
import scalaz.Ordering._

object a28_Sort_Nested_List_by_Length_of_Sublists extends Properties(
  "Sort Nested Lists by length of sublists (and practice using Tags)") {

  implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })

  // helper method for creating a list whose contents have been tagged with T
  def taggedList[A, T](as: IList[A]): IList[A @@ T] = as.map(Tag[A, T])

  // Sorted by length version
  sealed trait ByLength
  val ByLength = Tag.of[ByLength]
  implicit def byLengthOrderEqual[A: Equal]: Order[IList[A] @@ ByLength] =
    Order.order[IList[A] @@ ByLength]({ case (x, y) => Tag.unwrap(x).length ?|? Tag.unwrap(y).length })

  sealed trait ByFrequency
  val ByFrequencyTag = Tag.of[ByFrequency]
  implicit def byFrequencyOrderAndEqual[A: Equal]: Order[(Int, IList[A]) @@ ByFrequency] =
    Order.order({ case (x, y) => Tag.unwrap(x)._1 ?|? Tag.unwrap(y)._1 })

  val originalList: IList[IList[Symbol]] = IList(
    IList('a, 'b, 'c),
    IList('d, 'e),
    IList('f, 'g, 'h),
    IList('d, 'e),
    IList('i, 'j, 'k, 'l),
    IList('m, 'n),
    IList('o)
  )

  property("Sort by length") = {
    val expectedByLengthList: IList[IList[Symbol]] = IList(
      IList('o),
      IList('d, 'e),
      IList('d, 'e),
      IList('m, 'n),
      IList('a, 'b, 'c),
      IList('f, 'g, 'h),
      IList('i, 'j, 'k, 'l)
    )
    expectedByLengthList === sort[IList[Symbol] @@ ByLength](taggedList(originalList)).map(Tag.unwrap)
  }
  property("Sort by frequency") = {
    val expectedByFrequencyList: IList[IList[Symbol]] = IList(
      IList('o),
      IList('d, 'e),
      IList('d, 'e),
      IList('m, 'n),
      IList('a, 'b, 'c),
      IList('f, 'g, 'h),
      IList('i, 'j, 'k, 'l)
    )
    expectedByFrequencyList === sort[(Int, IList[Symbol]) @@ ByFrequency](taggedList(originalList.map(l => (l.length, l))))
      .map(Tag.unwrap(_)._2)
  }

  def sort[Z: Order](l: IList[Z]): IList[Z] = {
    def combine[A: Order](l1: IList[A], l2: IList[A]): IList[A] = {
      (l1, l2) match {
        case (INil(), list) => list
        case (list, INil()) => list
        case (first@ICons(h1, tail1), second@ICons(h2, tail2)) => h1 ?|? h2 match {
          case LT => ICons(h1, combine(tail1, second))
          case GT => ICons(h2, combine(first, tail2))
          case EQ => ICons(h1, ICons(h2, combine(tail1, tail2)))
        }
      }
    }

    def mergeSort[A: Order](l: IList[A]): IList[A] = {
      l.splitAt(l.length / 2) match {
        case (INil(), list) => list
        case (list, INil()) => list
        case (left, right) =>
          val sortedLeft = mergeSort(left)
          val sortedRight = mergeSort(right)
          combine(sortedLeft, sortedRight)
      }
    }

    mergeSort(l)
  }

}