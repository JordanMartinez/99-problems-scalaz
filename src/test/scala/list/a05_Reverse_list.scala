import scalaz._
import Scalaz._
import org.scalacheck._

object a05_Reverse_list extends Properties("reverse list") {

  property("last is last") = {
    val l = IList(1, 1, 2, 3, 5, 8)
    l.reverse === reverse(l)
  }

  def reverse[A](l: IList[A]): IList[A] =
    l.foldLeft(IList.empty[A])((acc, next) => ICons(next, acc))
}
