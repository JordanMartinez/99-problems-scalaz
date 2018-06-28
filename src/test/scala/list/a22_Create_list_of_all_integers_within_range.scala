package list

import scala.Int
import org.scalacheck._
import scalaz._
import Scalaz._

import scala.annotation.tailrec

object a22_Create_list_of_all_integers_within_range extends Properties("Create list of all integers within range") {

  property("") = {
    IList(4, 5, 6, 7, 8, 9) === range(4, 9)
  }

  def range(from: Int, to: Int): IList[Int] = {
    @tailrec
    def go(from: Int, to: Int, acc: IList[Int]): IList[Int] = {
      if (from == to) from +: acc
      else go(from, to - 1, to +: acc)
    }
    go(from, to, INil())
  }

}
