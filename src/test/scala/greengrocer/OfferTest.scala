package greengrocer

import greengrocer.Greengrocer.{Money, Offer}
import org.scalatest.{FlatSpec, Matchers}

class OfferTest extends FlatSpec with Matchers{

  behavior of "Offer 3 x 2"

  private val price60 = Money(60)
  private val offer3x2 = Offer(3, 2)
  it should "not have offer when there is one" in {
    val total = offer3x2.discount(1, price60)
    total shouldBe  price60
  }

  it should "not have offer when there are two" in {
    val total = offer3x2.discount(2, price60)
    total shouldBe Money(120)
  }

  it should "have one free when there are three" in {
    val total = offer3x2.discount(3, price60)
    total shouldBe Money(120)
  }

  it should "have one free and pay one more when there are four" in {
    val total = offer3x2.discount(4, price60)
    total shouldBe Money(180)
  }

  behavior of "Offer 2 x 1"

  private val price25 = Money(25)
  private val offer2x1 = Offer(2, 1)
  it should "not have offer when there is one" in {
    val appleOffer = offer2x1.discount(1, price25)
    appleOffer shouldBe Money(25)
  }

  it should "have one free when there are two" in {
    val appleOffer = offer2x1.discount(2, price25)
    appleOffer shouldBe Money(25)
  }

  it should "have one free and pay one when there are three" in {
    val appleOffer = offer2x1.discount(3, price25)
    appleOffer shouldBe Money(50)
  }

  it should "have two free when there are four" in {
    val appleOffer = offer2x1.discount(4, price25)
    appleOffer shouldBe Money(50)
  }
}
