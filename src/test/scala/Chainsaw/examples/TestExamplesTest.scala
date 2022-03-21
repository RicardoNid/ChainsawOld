package Chainsaw.examples

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.matchers.should._
import org.scalactic._

class TestExamplesTest extends AnyFlatSpec with Matchers {

  "examples" should "show you how to use should matchers" in {
    Set[Int]() shouldBe 'empty
    Set[Int]() shouldBe 'empty

    7 shouldEqual 6 +- 2

  }

}

import org.scalatest._
import matchers._
import prop._
import scala.collection.immutable._

class SetSpec extends AnyPropSpec with TableDrivenPropertyChecks with should.Matchers {

  val examples =
    Table(
      "set",
      BitSet.empty,
      HashSet.empty[Int],
      TreeSet.empty[Int]
    )

  property("an empty Set should have size 0") {
    forAll(examples) { set =>
      set.size should be(0)
    }
  }

  property("invoking head on an empty set should produce NoSuchElementException") {
    forAll(examples) { set =>
      a[NoSuchElementException] should be thrownBy { set.head }
    }
  }
}

import org.scalatest._

class TVSet {
  private var on: Boolean = false
  def isOn: Boolean       = on
  def pressPowerButton() {
    on = !on
  }
}

class TVSetSpec extends AnyFeatureSpec with GivenWhenThen {

  info("As a TV set owner")
  info("I want to be able to turn the TV on and off")
  info("So I can watch TV when I want")
  info("And save energy when I'm not watching TV")

  Feature("TV power button") {
    Scenario("User presses power button when TV is off") {

      Given("a TV set that is switched off")
      val tv = new TVSet
      assert(!tv.isOn)

      When("the power button is pressed")
      tv.pressPowerButton()

      Then("the TV should switch on")
      assert(tv.isOn)
    }

    Scenario("User presses power button when TV is on") {

      Given("a TV set that is switched on")
      val tv = new TVSet
      tv.pressPowerButton()
      assert(tv.isOn)

      When("the power button is pressed")
      tv.pressPowerButton()

      Then("the TV should switch off")
      assert(!tv.isOn)
    }
  }
}
