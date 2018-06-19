import scala.Any
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

object a07_Flatten_Nested_List_Structure extends Properties("flatten nested list structure") {

  property("flatten list structure") = {
    implicit val anyEqual = Equal.equal[Any]({case (a1, a2) => a1 == a2})
    (IList(1, 1, 2, 3, 5, 8): IList[Any]) === flatten(IList(IList(1, 1), 2, IList(3, IList(5, 8))))
  }

  def flatten(a: Any): IList[Any] = a match {
    case INil() => INil()
    case ICons(h, tail) => flatten(h) ++ flatten(tail)
    case _ => IList(a)
  }

}
