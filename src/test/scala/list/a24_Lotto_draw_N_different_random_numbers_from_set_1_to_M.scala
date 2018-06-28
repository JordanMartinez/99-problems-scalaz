package list

import scala.{Int, Option, Some, None, sys}
import org.scalacheck._
import scalaz._
import Scalaz._

import scala.annotation.tailrec
import scala.util.Random

object a24_Lotto_draw_N_different_random_numbers_from_set_1_to_M extends Properties("Lotto: Drawn N different random numbers from the set 1..M") {

  property("") = {
    val expectedLength = 6
    val lottery: IList[Int] = lotto(expectedLength, 49)
    lottery.distinct.length === expectedLength
  }

  def lotto(totalNum: Int, setEnd: Int): IList[Int] = {
    randomSelect(totalNum, range(1, setEnd))
  }

  def range(from: Int, to: Int): IList[Int] = {
    @tailrec
    def go(from: Int, to: Int, acc: IList[Int]): IList[Int] = {
      if (from == to) from +: acc
      else go(from, to - 1, to +: acc)
    }
    go(from, to, INil())
  }

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