package ano

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

class CheckoutTest extends FunSpec with Matchers {

  import Checkout._

  describe("Checkout") {
    it("should return zero for empty list") {
      totalCost(Seq.empty[Item]) should equal(BigDecimal("0"))
    }

    it("should return item value for one-element list") {
      totalCost(List(Apple)) should equal(price(Apple))
      totalCost(List(Orange)) should equal(price(Orange))
    }
  }
}
