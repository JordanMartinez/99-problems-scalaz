import scala.Char
import scala.Option
import scala.None
import scala.Some
import org.scalacheck._
import scalaz.Scalaz._
import scalaz._

object a08_Eliminate_consecutive_duplicates_of_list_elements extends Properties("eliminate consecutive duplicates") {

  property("") = IList('a', 'b', 'c', 'a', 'd', 'e') === compress(IList('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))

  def compress(l: IList[Char]): IList[Char] =
    l.foldRight((IList.empty[Char], Option.empty[Char]))((nextChar, acc) => acc._2 match {
      case None => (ICons(nextChar, acc._1), Some(nextChar))
      case Some(c) => if (c == nextChar) acc else (ICons(nextChar, acc._1), Some(nextChar))
    })._1

}
