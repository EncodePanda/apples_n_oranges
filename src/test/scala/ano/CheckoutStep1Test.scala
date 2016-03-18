package ano

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen


class CheckoutStep1Test extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import Checkout._

  val itemGen: Gen[Item] = Gen.oneOf(Apple, Orange)
  val itemsGen: Gen[List[Item]] = Gen.listOf(itemGen)

  implicit def strategy = Step1Strategy.strategy

  describe("Checkout.totalCost") {

    describe("properties: ") {
      it("value is always higher or equal to zero") {
        forAll(itemsGen) { items =>
          totalCost(items) should be >=(BigDecimal("0.0"))
        }
      }
      it("value should not be higher then product of list size and most exensive item") {
        val expensive = Item.all.map(price).map(_.toDouble).foldLeft(0.0)(Math.max)
        forAll(itemsGen) { items =>
          totalCost(items) should be <=(BigDecimal(expensive) * items.size)
        }
      }
    }

    describe("scenarios: ") {

      it("should return zero for empty list") {
        totalCost(List.empty[Item]) should equal(BigDecimal("0"))
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
}
