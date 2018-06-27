package list

import scala.{Symbol, Int, Option, None, Some, sys}
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

import scala.annotation.tailrec
import scala.util.Random

object a25_Generate_random_permutation_of_elements_of_a_list extends Properties("Lotto: Drawn N different random numbers from the set 1..M") {

  implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })
  implicit val symbolOrder = Order.order[Symbol]({case (a1, a2) => a1.name ?|? a2.name})

  property("") = {
    val originalList: IList[Symbol] = IList('a, 'b, 'c, 'd, 'e, 'f)
    val permutedList: IList[Symbol] = randomPermute(originalList)
    permutedList.distinct.length === originalList.length
    permutedList =/= originalList
  }

  def randomPermute[A](l: IList[A]): IList[A] = randomSelect(l.length, l)

  def randomSelect[A](numOfElems: Int, l: IList[A]): IList[A] = {
    val length = l.length
    val r: Random = Random
    @tailrec
    def go(loopsRemaining: Int, length: Int, l: IList[A], acc: IList[A]): IList[A] = {
      if (loopsRemaining == 0) acc
      else {
        val index = r.nextInt(length)
        val (value, updatedList) = removeAt(index, l)
        go(loopsRemaining - 1, length - 1, updatedList, value +: acc)
      }
    }
    go(numOfElems, length, l, INil())
  }

  def removeAt[A](n: Int, l: IList[A]): (A, IList[A]) = {
    @tailrec
    def remove(n: Int, l: IList[A], value: Option[A], acc: IList[A]): (A, IList[A]) = {
      if (n == 0) value match {
        case None => sys.error("Impossible to reach")
        case Some(v) => (v, acc.reverse |+| l)
      }
      else l match {
        case ICons(h, tail) if n == 1 => remove(n - 1, tail, Some(h), acc)
        case ICons(h, tail) => remove(n - 1, tail, None, h +: acc)
      }
    }
    remove(n + 1, l, None, INil())
  }

}