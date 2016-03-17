package ano

sealed trait Item
object Apple extends Item
object Orange extends Item

object Item {
  def all: Seq[Item] = List(Apple, Orange)
}

object Checkout {

  def price(item: Item) = item match {
    case Apple => BigDecimal("0.6")
    case Orange => BigDecimal("0.25")
  }

  def totalCost(items: Seq[Item]): BigDecimal = items.foldLeft(BigDecimal("0.0")) {
    case (acc, item) => acc + price(item)
  }

}
