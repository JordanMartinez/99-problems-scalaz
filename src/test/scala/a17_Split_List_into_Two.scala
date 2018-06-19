import scala.Symbol
import scala.Int
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

object a17_Split_List_into_Two extends Properties("Split list into two") {

  property("") = {
    val expected = (IList('a, 'b, 'c), IList('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    implicit val symbolEqual = Equal.equal[Symbol]({case (a1, a2) => a1 == a2 })
    expected === split(3, IList('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  def split(n: Int, l: IList[Symbol]): (IList[Symbol], IList[Symbol]) = {
    val (_, left, right) = l.foldLeft((n, IList.empty[Symbol], IList.empty[Symbol]))((acc, next) =>
      if (acc._1 == 0) (acc._1, acc._2, acc._3 :+ next)
      else (acc._1 - 1, acc._2 :+ next, acc._3)
    )
    (left, right)
  }
}
