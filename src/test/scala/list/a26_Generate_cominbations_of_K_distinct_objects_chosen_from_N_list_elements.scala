package list

import scala.{Symbol, Int, Option, Some, None}
import org.scalacheck.Prop._
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

import scala.annotation.tailrec

object a26_Generate_cominbations_of_K_distinct_objects_chosen_from_N_list_elements extends Properties(
  "Generate combinations of K distinct objects chosen from N elements of a list") {

  implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })
  implicit val symbolOrder = Order.order[Symbol]({case (a1, a2) => a1.name ?|? a2.name})

  property("Solution to problem is correct") = {
    val chooseK = 3
    val originalList: IList[Symbol] = IList('a, 'b, 'c, 'd, 'e, 'f)
    val comboList: IList[IList[Symbol]] = combinations(chooseK, originalList)
    all(
      comboList.distinct.all(list => list.length === chooseK),
      comboList.length === totalCombinations(originalList.length, chooseK)
    )
  }

  property("Total Number of combinations is correct") = {
    totalCombinations(12, 3) === 220
  }

  def totalCombinations(n: Int, k: Int): Int =
    factorial(n)/(factorial(k) * factorial(n - k))

  private val memo = Memo.immutableHashMapMemo[Int, Int]
  private val factorial = memo(realFactorial)

  def realFactorial(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case x => x * factorial(x - 1)
  }

  // NOTE: THIS IS NOT STACK SAFE | TODO: Make this stack safe
  def combinations[A](n: Int, l: IList[A]): IList[IList[A]] = {
    def go(n: Int, l: IList[A], acc: IList[A]): IList[IList[A]] = {
      if (n == 0) l.map(a => (a +: acc).reverse)
      else l match {
        case INil() => INil()
        case ICons(h, tail) => go(n - 1, tail, h +: acc) |+| go(n, tail, acc)
      }
    }
    go(n - 1, l, INil())
  }

  def removeAt[A](n: Int, l: IList[A]): (Option[A], IList[A]) = {
    @tailrec
    def remove(n: Int, l: IList[A], value: Option[A], acc: IList[A]): (Option[A], IList[A]) = {
      if (n == 0) (value, acc.reverse |+| l)
      else l match {
        case ICons(h, tail) if n == 1 => remove(n - 1, tail, Some(h), acc)
        case ICons(h, tail) => remove(n - 1, tail, None, h +: acc)
        case INil() => (None, INil())
      }
    }
    remove(n + 1, l, None, INil())
  }

}