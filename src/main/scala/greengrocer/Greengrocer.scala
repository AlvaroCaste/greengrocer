package greengrocer

import greengrocer.Greengrocer.Item.{Apple, Orange}

object Greengrocer extends App {

  case class Money(amount: Int) {
    def +(money: Money) = Money(this.amount + money.amount)

    override def toString = amount match {
      case zero if zero == 0 => s"£$amount"
      case lessThanHundred if lessThanHundred < 100 => s"${amount}p"
      case _ => {
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

  sealed trait Item {
    val name: String
    val price: Money
  }
  object Item {
    case class Orange(name: String = "Orange", price: Money = Money(25)) extends Item
    case class Apple(name: String = "Apple", price: Money = Money(60)) extends Item
  }

  def getTotalPrice(fruit: String*): String = fruit.map {
    case apple if apple == "Apple" => Apple().price
    case _ => Orange().price
  }.reduce(_ + _).toString

}
