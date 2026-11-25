package pl.iterators.baklava

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

object SchemaEnumSumFixtures {
  opaque type Meters <: Double = Double
  object Meters {
    given Schema[Meters] = Schema.doubleSchema.withDescription("meters")
  }

  final class Celsius(val value: Double) extends AnyVal
  object Celsius {
    given Schema[Celsius] = Schema.doubleSchema.asInstanceOf[Schema[Celsius]].withDescription("celsius")
  }

  final class Kelvin(val value: Double) extends AnyVal

  enum Shape {
    case Circle(radius: Double)
    case Rectangle(width: Double, height: Double)
    case Dot
  }
}

class SchemaEnumSumSpec extends AnyFunSpec with Matchers {
  import SchemaEnumSumFixtures.*

  describe("derivedSum on a Scala 3 enum") {
    it("describes data-carrying and parameterless cases") {
      val schema = Schema.derivedSum[Shape](SumEncoding.Discriminator("kind"))
      schema.oneOf.map(_.className) shouldBe Seq("Circle", "Dot", "Rectangle")
      schema.discriminator shouldBe Some(SchemaDiscriminator("kind", Seq("Circle", "Dot", "Rectangle")))
      schema.oneOf.head.properties.keySet shouldBe Set("radius")
      schema.oneOf(1).properties shouldBe empty
    }
  }

  describe("the value-class instance") {
    it("leaves instances defined for bounded opaque types and plain value classes in charge") {
      summon[Schema[Meters]].description shouldBe Some("meters")
      summon[Schema[Celsius]].description shouldBe Some("celsius")
      summon[Schema[Option[Celsius]]].description shouldBe Some("celsius")
      summon[Schema[Long]] shouldBe Schema.longSchema
    }

    it("does not invent a schema for a value class that is not a case class") {
      assertDoesNotCompile("summon[Schema[Kelvin]]")
    }
  }
}
