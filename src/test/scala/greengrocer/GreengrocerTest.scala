package greengrocer

import org.scalatest.{FlatSpec, Matchers}

class GreengrocerTest extends FlatSpec with Matchers {

  behavior of "Greengrocer"

  it should "be sum zero when there are no items" in {
    Greengrocer.getTotalPrice("") shouldBe "£0"
  }

  it should "be sum items' price" in {
    Greengrocer.getTotalPrice("Apple", "Apple", "Orange", "Apple") shouldBe "£1.45"
  }

  it should "not sum unrecognized items" in {
    Greengrocer.getTotalPrice("Lemon") shouldBe "£0"
    Greengrocer.getTotalPrice("Apple", "Lemon") shouldBe "60p"
  }

}
