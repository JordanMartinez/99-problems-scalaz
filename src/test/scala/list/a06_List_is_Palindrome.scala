package list

import scala.Int
import scala.Boolean
import org.scalacheck._
import scalaz._
import Scalaz._

object a06_List_is_Palindrome extends Properties("list is palindrome") {

  property("palindrome is true") = true === isPalindrome(IList(1, 2, 3, 2, 1))
  property("Nil is false") = false === isPalindrome(INil())
  property("not one is false") = false === isPalindrome(IList(1, 2, 3, 2, 2))

  def isPalindrome(l: IList[Int]): Boolean =
    if (l.isEmpty) false
    else l === l.reverse
}
