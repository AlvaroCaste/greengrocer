package greengrocer

import greengrocer.Greengrocer.Item.{Apple, Orange}

object Greengrocer extends App {

  case class Price(price: Int) {
    def +(price: Price) = Price(this.price + price.price)

    override def toString = {
      if (price < 100) s"${price}p"
      else {
        val integerPart = price / 100
        val decimalPart = price % 100
        decimalPart match {
          case zero if zero == 0 => s"£$integerPart"
          case lessThan if lessThan < 10 => s"£$integerPart.0$decimalPart"
          case moreThan => s"£$integerPart.$decimalPart"
        }
      }
    }
  }

  sealed trait Item {
    val name: String
    val price: Price
  }
  object Item {
    case class Orange(name: String = "Orange", price: Price = Price(25)) extends Item
    case class Apple(name: String = "Apple", price: Price = Price(60)) extends Item
  }

  def getTotalPrice(fruit: String*): String = fruit.map {
    case apple if apple == "Apple" => Apple().price
    case _ => Orange().price
  }.reduce(_ + _).toString

}
