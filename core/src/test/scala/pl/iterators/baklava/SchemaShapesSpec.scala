package pl.iterators.baklava

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import pl.iterators.baklava.BaklavaSerialize.serializableCallCodec

object SchemaShapesFixtures {
  sealed trait Payment
  final case class Card(last4: String)    extends Payment
  final case class Transfer(iban: String) extends Payment
  case object Cash                        extends Payment

  object Payment {
    implicit val schema: Schema[Payment] = Schema.derivedSum[Payment](SumEncoding.Discriminator("type"))
  }

  final case class Order(payment: Payment, previous: Option[Payment])

  sealed trait Expr
  final case class Lit(value: Int)              extends Expr
  final case class Add(left: Expr, right: Expr) extends Expr

  final case class Category(name: String, children: List[Category], parent: Option[Category])

  final case class UserId(value: Long)        extends AnyVal
  final case class Tags(values: List[String]) extends AnyVal
  final case class HasIds(id: UserId, ids: List[UserId], maybe: Option[UserId], tags: Tags)

  final case class Problem(message: String)
  final case class Done(id: UserId)
  final case class WithEither(plain: Either[String, Int], typed: Either[Problem, Done])

  object SnakeSchemas extends SchemaDerivation with SchemaDefaults {
    override def transformConstructorName(name: String): String = SchemaNameTransform.snakeCase(name)
  }
  sealed trait Event
  final case class UserSignedUp(email: String) extends Event
  case object SessionExpired                   extends Event
}

class SchemaShapesSpec extends AnyFunSpec with Matchers {
  import SchemaShapesFixtures._

  private def classNames(schemas: Seq[Schema[?]]): Seq[String] = schemas.map(_.className)

  describe("derivedSum") {
    it("describes a discriminated sealed trait as oneOf its subtypes, with the tag property and values") {
      val schema = Schema.derivedSum[Payment](SumEncoding.Discriminator("type"))
      schema.className shouldBe "Payment"
      schema.`type` shouldBe SchemaType.ObjectType
      classNames(schema.oneOf) shouldBe Seq("Card", "Cash", "Transfer")
      schema.discriminator shouldBe Some(SchemaDiscriminator("type", Seq("Card", "Cash", "Transfer")))
      schema.oneOf.head.properties.keySet shouldBe Set("last4")
      schema.oneOf(1).properties shouldBe empty
    }

    it("wraps each subtype in an object keyed by its name for the wrapper-object encoding") {
      val schema = Schema.derivedSum[Payment](SumEncoding.WrapperObject)
      schema.discriminator shouldBe None
      schema.oneOf.map(_.properties.keySet) shouldBe Seq(Set("Card"), Set("Cash"), Set("Transfer"))
      schema.oneOf.head.properties("Card").properties.keySet shouldBe Set("last4")
      schema.oneOf.foreach(_.className shouldBe "")
    }

    it("lists the plain subtype schemas for the untagged encoding") {
      val schema = Schema.derivedSum[Payment](SumEncoding.Untagged)
      schema.discriminator shouldBe None
      classNames(schema.oneOf) shouldBe Seq("Card", "Cash", "Transfer")
    }

    it("maps the tags through transformConstructorName") {
      SnakeSchemas.derivedSum[Event](SumEncoding.Discriminator("kind")).discriminator shouldBe
      Some(SchemaDiscriminator("kind", Seq("session_expired", "user_signed_up")))
      SnakeSchemas.derivedSum[Event](SumEncoding.WrapperObject).oneOf.map(_.properties.keySet) shouldBe
      Seq(Set("session_expired"), Set("user_signed_up"))
    }

    it("is picked up by derived case classes that contain the sealed trait") {
      val order = implicitly[Schema[Order]]
      order.properties("payment") should be theSameInstanceAs Payment.schema
      order.properties("previous").nullable shouldBe true
      classNames(order.properties("previous").oneOf) shouldBe Seq("Card", "Cash", "Transfer")
    }

    it("handles recursive sealed traits") {
      val schema = Schema.derivedSum[Expr](SumEncoding.Discriminator("op"))
      val add    = schema.oneOf.find(_.className == "Add").get
      add.properties("left") should be theSameInstanceAs schema
      schema.toString shouldBe "oneOf(object(left*: ref(Expr), right*: ref(Expr)), object(value*: integer))"
    }

    it("only accepts sealed traits and enums, and sealed traits still need an explicit instance") {
      assertDoesNotCompile("Schema.derivedSum[Card](SumEncoding.Untagged)")
      assertDoesNotCompile("implicitly[Schema[Event]]")
    }
  }

  describe("recursive case classes") {
    it("derive without overflowing, referring back to the same instance") {
      val schema = implicitly[Schema[Category]]
      schema.properties("children").items.get should be theSameInstanceAs schema
      schema.properties("parent").nullable shouldBe true
      schema.toString shouldBe "object(name*: string, children*: array(ref(Category)), parent: ref(Category))"
    }

    it("serialize the back references as refs to the enclosing class") {
      val serialized = BaklavaSchemaSerializable(implicitly[Schema[Category]])
      serialized.ref shouldBe None
      serialized.properties("name").`type` shouldBe SchemaType.StringType
      val child = serialized.properties("children").items.get
      child.ref shouldBe Some("Category")
      child.properties shouldBe empty
      val parent = serialized.properties("parent")
      parent.ref shouldBe Some("Category")
      parent.required shouldBe false
      parent.nullable shouldBe true
    }

    it("serialize a type that only recurses through another type") {
      val serialized = BaklavaSchemaSerializable(Schema.derivedSum[Expr](SumEncoding.Untagged))
      val add        = serialized.oneOf.find(_.className == "Add").get
      add.properties("left").ref shouldBe Some("Expr")
      serialized.oneOf.find(_.className == "Lit").get.properties("value").`type` shouldBe SchemaType.IntegerType
    }
  }

  describe("value classes") {
    it("are documented as the value they wrap") {
      val schema = implicitly[Schema[HasIds]]
      schema.properties("id").`type` shouldBe SchemaType.IntegerType
      schema.properties("id").format shouldBe Some("int64")
      schema.properties("ids").items.get.format shouldBe Some("int64")
      schema.properties("maybe").nullable shouldBe true
      schema.properties("maybe").`type` shouldBe SchemaType.IntegerType
      schema.properties("tags").`type` shouldBe SchemaType.ArrayType
    }

    it("get an instance of their own") {
      implicitly[Schema[UserId]].format shouldBe Some("int64")
      Schema.derived[UserId].`type` shouldBe SchemaType.IntegerType
    }
  }

  describe("Either") {
    it("is documented untagged, as oneOf both sides") {
      val schema = implicitly[Schema[Either[String, Int]]]
      schema.oneOf.map(_.`type`) shouldBe Seq(SchemaType.StringType, SchemaType.IntegerType)
      schema.`type` shouldBe SchemaType.ObjectType
      implicitly[Schema[Either[String, String]]].`type` shouldBe SchemaType.StringType
    }

    it("is derived inside case classes, with derived types on either side") {
      val schema = implicitly[Schema[WithEither]]
      schema.properties("plain").oneOf.map(_.`type`) shouldBe Seq(SchemaType.StringType, SchemaType.IntegerType)
      classNames(schema.properties("typed").oneOf) shouldBe Seq("Problem", "Done")
      schema.properties("typed").oneOf(1).properties("id").format shouldBe Some("int64")
    }

    it("can be described as circe's wrapper objects") {
      val schema = Schema.eitherWrappedSchema[Problem, Done]()
      schema.oneOf.map(_.properties.keySet) shouldBe Seq(Set("Left"), Set("Right"))
    }
  }

  describe("serialized sum types") {
    it("keep alternatives and discriminator through the call store codec") {
      val serialized = BaklavaSchemaSerializable(Schema.derivedSum[Payment](SumEncoding.Discriminator("type")))
      serialized.oneOf.map(_.className) shouldBe Seq("Card", "Cash", "Transfer")
      serialized.discriminator shouldBe Some(SchemaDiscriminator("type", Seq("Card", "Cash", "Transfer")))

      val base = CallFixtures.callAt("/orders", "/orders")
      val call = base.copy(request = base.request.copy(bodySchema = Some(serialized)))
      readFromArray[BaklavaSerializableCall](writeToArray(call)) shouldBe call

      val recursive = BaklavaSchemaSerializable(implicitly[Schema[Category]])
      val withRefs  = base.copy(request = base.request.copy(bodySchema = Some(recursive)))
      val decoded   = readFromArray[BaklavaSerializableCall](writeToArray(withRefs)).request.bodySchema.get
      decoded.properties("children").items.get.ref shouldBe Some("Category")
      decoded.properties("parent").ref shouldBe Some("Category")
    }
  }
}
