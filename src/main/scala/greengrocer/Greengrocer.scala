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

  sealed trait Item extends Offer {
    val name: String
    val price: Money
  }

  sealed trait Offer {
    def offer(nItem: Int): Money
  }

  object Item {
    case class Orange(name: String = "Orange", price: Money = Money(25)) extends Item {
      override def offer(nItem: Int): Money = {
        val amount = this.price.amount
        val remainder = nItem % 3
        val price = amount * nItem
        val discount = price / 3
        val discountNotExactly = amount * (nItem - remainder)

        if (remainder == 0) Money(price - discount)
        else Money(discountNotExactly - (discountNotExactly / 3) + (amount * remainder))
      }
    }
    case class Apple(name: String = "Apple", price: Money = Money(60)) extends Item {
      override def offer(nItem: Int): Money = {
        val amount = this.price.amount
        val remainder = nItem % 2

        if (remainder == 0) Money(amount * nItem / 2)
        else Money((amount * (nItem - 1) / 2) + amount)
      }
    }

    def parseItem(name: String): Option[Item] = name match {
      case apple if apple == "Apple" => Some(Apple())
      case orange if orange == "Orange" => Some(Orange())
      case _ => None
    }

    implicit class Total(items: Seq[Item]) {
      def getTotal: Money = items match {
        case empty if empty.isEmpty => Money(0)
        case _ => items.groupBy(identity).mapValues(_.size).map(i => i._1.offer(i._2)).reduce(_ + _)
      }
    }
  }

  import greengrocer.Greengrocer.Item.parseItem
  def getTotalPrice(fruit: String*): String = fruit.flatMap(parseItem).getTotal.toString
}
