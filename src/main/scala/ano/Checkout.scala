package ano

import scalaz._
import Scalaz._

sealed trait Item
object Apple extends Item
object Orange extends Item

object Item {
  def all: Seq[Item] = List(Apple, Orange)
}

case class Checked(sum: BigDecimal, scanned: Map[Item, Int])

object Checked {
  val ZERO: Checked = Checked(sum = BigDecimal("0.0"), scanned = Map())
  def apply(item: Item): Checked = Checked(sum = Checkout.price(item), scanned = Map(item -> 1))
}

object Checkout {

  def price(item: Item) = item match {
    case Apple => BigDecimal("0.6")
    case Orange => BigDecimal("0.25")
  }

  def totalCost(items: Seq[Item])(implicit monoid: Monoid[Checked]): BigDecimal = {
    import monoid._
    val checked = items.foldLeft(monoid.zero) {
      case (acc, item) => acc |+| Checked(item)
    }
    checked.sum
  }
}

object Step1Strategy {
  implicit def strategy: Monoid[Checked] = new Monoid[Checked]() {
    def zero = Checked.ZERO
    implicit def append(c1: Checked, c2: => Checked): Checked =
      c1.copy(sum = c1.sum + c2.sum, scanned = c1.scanned |+| c2.scanned)
  }
}

object Step2Strategy {
  implicit def strategy: Monoid[Checked] = new Monoid[Checked]() {
    def zero = Checked.ZERO
    implicit def append(c1: Checked, c2: => Checked): Checked = {
      val initial = c1.copy(sum = c1.sum + c2.sum, scanned = c1.scanned |+| c2.scanned)
      initial
    }
  }
}
