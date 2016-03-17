package ano

import org.scalatest._

class CheckoutTest extends FunSpec with Matchers {

  import Checkout._

  describe("Checkout.totalCost") {
    it("should return zero for empty list") {
      totalCost(Seq.empty[Item]) should equal(BigDecimal("0"))
    }

    it("should return item value for one-element list") {
      totalCost(List(Apple)) should equal(price(Apple))
      totalCost(List(Orange)) should equal(price(Orange))
    }

    it("should return sum of all items in list") {
      val items = List(Apple, Apple, Orange, Apple)
      totalCost(items) should equal(BigDecimal("2.05"))
    }
  }
}
