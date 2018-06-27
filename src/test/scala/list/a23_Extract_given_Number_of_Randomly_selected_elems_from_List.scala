import scala.Int
import scala.Symbol
import scala.{Option, Some, None, sys}
import org.scalacheck._
import scalaz._
import Scalaz._

import scala.annotation.tailrec
import scala.util.Random

object a23_Extract_given_Number_of_Randomly_selected_elems_from_List extends Properties("Extract a given number of randomly selected elements from list") {

  property("") = {
    val expectedLength = 4
    expectedLength === randomSelect(expectedLength, IList('a, 'b, 'c, 'd, 'f, 'g, 'h)).length
  }

  def randomSelect(numOfElems: Int, l: IList[Symbol]): IList[Symbol] = {
    val length = l.length
    val r: Random = Random
    @tailrec
    def go(loopsRemaining: Int, length: Int, l: IList[Symbol], acc: IList[Symbol]): IList[Symbol] = {
      if (loopsRemaining == 0) acc
      else {
        val index = r.nextInt(length)
        val (value, updatedList) = removeAt(index, l)
        go(loopsRemaining - 1, length - 1, updatedList, value +: acc)
      }
    }
    go(numOfElems, length, l, INil())
  }

  def removeAt(n: Int, l: IList[Symbol]): (Symbol, IList[Symbol]) = {
    @tailrec
    def remove(n: Int, l: IList[Symbol], value: Option[Symbol], acc: IList[Symbol]): (Symbol, IList[Symbol]) = {
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
