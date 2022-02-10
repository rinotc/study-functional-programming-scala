package chapter7
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DefaultSumTest extends AnyWordSpec with Matchers {

  import DefaultSum._

  "DefaultSumTest" should {
    "sum" in {
      val list = Seq(1, 2, 3, 4, 5)
      sum(list) mustBe 15
    }
  }
}
