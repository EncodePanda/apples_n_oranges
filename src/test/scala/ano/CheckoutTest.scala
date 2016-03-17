package ano

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

class CheckoutTest extends FunSpec with Matchers {

  describe("Checkout") {
    it("should return zero for empty list") {
      Checkout.totalCost(Seq.empty[Item]) should equal(BigDecimal("0"))
    }

  }
}
