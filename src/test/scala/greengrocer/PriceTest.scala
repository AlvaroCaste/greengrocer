package greengrocer

import org.scalatest.{FlatSpec, Matchers}
import greengrocer.Greengrocer.Price

class PriceTest extends FlatSpec with Matchers {

  behavior of "Price"

  it should "show pence when is less than 100" in {
    val price = Price(99)
    price.toString shouldBe "99p"
  }

  it should "show decimal part when there is" in {
    val price = Price(115)
    price.toString shouldBe "£1.15"
  }

  it should "show one pound when is 100" in {
    val price = Price(100)
    price.toString shouldBe "£1"
  }

  it should "show decimal part with 0 when is less than 10" in {
    val price = Price(105)
    price.toString shouldBe "£1.05"
  }
}
