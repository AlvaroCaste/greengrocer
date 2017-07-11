package greengrocer

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

    def parseItem(name: String): Option[Item] = name match {
      case apple if apple == "Apple" => Some(Apple())
      case orange if orange == "Orange" => Some(Orange())
      case _ => None
    }

    implicit class Total(items: Seq[Item]) {
      def getTotal: Money = items match {
        case empty if empty.isEmpty => Money(0)
        case _ => items.map (_.price).reduce (_+ _)
      }
    }
  }

  import greengrocer.Greengrocer.Item.parseItem
  def getTotalPrice(fruit: String*): String = fruit.flatMap(parseItem).getTotal.toString
}
