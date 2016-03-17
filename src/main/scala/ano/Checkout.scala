package ano

trait Item
object Apple extends Item
object Orange extends Item

object Checkout {
  def totalCost(items: Seq[Item]): BigDecimal = ???
}
