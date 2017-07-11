package greengrocer

import org.scalatest.{FlatSpec, Matchers}
import greengrocer.Greengrocer.Money

class MoneyTest extends FlatSpec with Matchers {

  behavior of "Money"

  it should "show zero pounds when is zero" in {
    val price = Money(0)
    price.toString shouldBe "£0"
  }

  it should "show pence when is less than 100" in {
    val price = Money(99)
    price.toString shouldBe "99p"
  }

  it should "show decimal part when there is" in {
    val price = Money(115)
    price.toString shouldBe "£1.15"
  }

  it should "show one pound when is 100" in {
    val price = Money(100)
    price.toString shouldBe "£1"
  }

  it should "show decimal part with 0 when is less than 10" in {
    val price = Money(105)
    price.toString shouldBe "£1.05"
  }
}
