package greengrocer

import org.scalatest.{FlatSpec, Matchers}

class GreengrocerTest extends FlatSpec with Matchers {

  behavior of "Greengrocer"

  it should "be sum items' price" in {
    Greengrocer.getTotalPrice("Apple", "Apple", "Orange", "Apple") shouldBe "£1.45"
  }

}
