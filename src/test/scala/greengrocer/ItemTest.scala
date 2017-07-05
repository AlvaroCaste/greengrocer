package greengrocer

import greengrocer.Greengrocer.Item.{Apple, Orange}
import greengrocer.Greengrocer.Price
import org.scalatest.{FlatSpec, Matchers}

class ItemTest extends FlatSpec with Matchers {

  behavior of "Apple"

  it should "not have offer when there is one" in {
    val appleOffer = Apple().offer(1)
    appleOffer shouldBe Price(60)
  }

  it should "have one apple free when there are two" in {
    val appleOffer = Apple().offer(2)
    appleOffer shouldBe Price(60)
  }

  it should "have one apple free when there are three" in {
    val appleOffer = Apple().offer(3)
    appleOffer shouldBe Price(120)
  }

  behavior of "Orange"

  it should "not have offer when there is one" in {
    val orangeOffer = Orange().offer(1)
    orangeOffer shouldBe Price(25)
  }

  it should "not have offer when there are two" in {
    val orangeOffer = Orange().offer(2)
    orangeOffer shouldBe Price(50)
  }

  it should "have one orange free when there are three" in {
    val orangeOffer = Orange().offer(3)
    orangeOffer shouldBe Price(50)
  }

  it should "have one orange free when there are four" in {
    val orangeOffer = Orange().offer(4)
    orangeOffer shouldBe Price(75)
  }

}
