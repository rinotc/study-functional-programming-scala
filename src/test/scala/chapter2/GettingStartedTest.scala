package chapter2

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GettingStartedTest extends AnyWordSpec with Matchers {

  import GettingStarted._

  "GettingStartedTest" should {
    "abs" in {
      abs(-33) mustBe 33
      abs(23) mustBe 23
    }

    "formatAbs" in {
      formatAbs(-99) mustBe "The absolute value of -99 is 99"
    }

    "factorial" in {
      factorial(5) mustBe 120
    }

    "fib" in {
      fib(10) mustBe 55
      // 末尾再帰じゃない書き方だと、fib(100)なんて計算できないが、末尾再帰なら一瞬
      fib(100) mustBe 3736710778780434371L
    }

    "formatFactorial" in {
      formatFactorial(5) mustBe "The factorial of 5 is 120."
    }

    "formatResult" in {
      formatResult("absolute value", -42, abs) mustBe "The absolute value of -42 is 42."
      formatResult("factorial value", 5, factorial) mustBe "The factorial value of 5 is 120."
    }
  }
}
