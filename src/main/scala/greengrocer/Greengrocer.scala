package greengrocer

import greengrocer.Greengrocer.Item.{Apple, Orange}

object Greengrocer extends App {

  case class Price(amount: Int) {
    def +(price: Price) = Price(this.amount + price.amount)

    override def toString = {
      if (amount < 100) s"${amount}p"
      else {
        val integerPart = amount / 100
        val decimalPart = amount % 100
        decimalPart match {
          case zero if zero == 0 => s"£$integerPart"
          case lessThan if lessThan < 10 => s"£$integerPart.0$decimalPart"
          case moreThan => s"£$integerPart.$decimalPart"
        }
      }
    }
  }

  sealed trait Item extends Offer {
    val name: String
    val price: Price
  }

  sealed trait Offer {
    def offer(nItem: Int): Price
  }

  object Item {
    case class Orange(name: String = "Orange", price: Price = Price(25)) extends Item {
      override def offer(nItem: Int): Price = {
        val amount = this.price.amount
        val remainder = nItem % 3
        val price = amount * nItem
        val discount = price / 3
        val discountNotExactly = amount * (nItem - remainder)

        if (remainder == 0) Price(price - discount)
        else Price(discountNotExactly - (discountNotExactly / 3) + (amount * remainder))
      }
    }
    case class Apple(name: String = "Apple", price: Price = Price(60)) extends Item {
      override def offer(nItem: Int): Price = {
        val amount = this.price.amount
        val remainder = nItem % 2

        if (remainder == 0) Price(amount * nItem / 2)
        else Price((amount * (nItem - 1) / 2) + amount)
      }
    }
  }

  def getTotalPrice(fruit: String*) = fruit.map {
    case apple if apple == "Apple" => Apple()
    case _ => Orange()
  }.groupBy(identity).mapValues(_.size).map(i => i._1.offer(i._2))
    .reduce(_ + _).toString

}
