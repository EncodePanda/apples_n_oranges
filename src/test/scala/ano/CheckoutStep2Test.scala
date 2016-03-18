package ano

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class CheckoutStep2Test extends FunSpec with Matchers {

  import Checkout._

  implicit def strategy = Step2Strategy.strategy

  describe("Checkout.totalCost") {

    describe("scenarios: ") {

      it("should return zero for empty list") {
        totalCost(List.empty[Item]) should equal(BigDecimal("0"))
      }

      it("should return item value for one-element list") {
        totalCost(List(Apple)) should equal(price(Apple))
        totalCost(List(Orange)) should equal(price(Orange))
      }

      it("should return sum of all items in list") {
        val items = List(Apple, Orange, Orange)
        totalCost(items) should equal(BigDecimal("1.1"))
      }

      it("should return with discounted apples") {
        val items1 = List(Apple, Apple, Orange, Apple, Apple, Apple, Apple)
        val items2 = List(Apple, Apple, Orange, Apple, Apple, Apple)
        totalCost(items1) should equal(BigDecimal("2.05"))
        totalCost(items2) should equal(BigDecimal("2.05"))
      }

      it("should return discounted oranges") {
        val items1 = List(Orange, Orange, Apple, Orange, Orange, Orange, Orange)
        val items2 = List(Orange, Orange, Apple, Orange, Orange, Orange)
        totalCost(items1) should equal(BigDecimal("1.6"))
        totalCost(items2) should equal(BigDecimal("1.6"))
      }
    }
  }
}
