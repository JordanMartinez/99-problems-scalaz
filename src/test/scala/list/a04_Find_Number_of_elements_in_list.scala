import scala.Int
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

object a04_Find_Number_of_elements_in_list extends Properties("size of list") {

  property("last is last") = {
    val l = IList(1, 1, 2, 3, 5, 8)
    6 === length(l)
  }

  def length[A](l: IList[A]): Int =
    l.foldLeft(0)((acc, _) => acc + 1)
}