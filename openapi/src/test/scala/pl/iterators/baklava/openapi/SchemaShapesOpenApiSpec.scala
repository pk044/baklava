package pl.iterators.baklava.openapi

import io.swagger.v3.core.util.Yaml
import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.info.Info
import io.swagger.v3.oas.models.media.{ComposedSchema, Schema => OpenApiSchema}
import io.swagger.v3.parser.OpenAPIV3Parser
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import pl.iterators.baklava.*
import sttp.model.{Method, StatusCode}

import scala.jdk.CollectionConverters.*

object SchemaShapesOpenApiFixtures {
  sealed trait Payment
  final case class Card(last4: String)    extends Payment
  final case class Transfer(iban: String) extends Payment
  case object Cash                        extends Payment

  sealed trait Expr
  final case class Lit(value: Int)              extends Expr
  final case class Add(left: Expr, right: Expr) extends Expr

  final case class Category(name: String, children: List[Category], parent: Option[Category])

  final case class UserId(value: Long) extends AnyVal
  final case class Account(id: UserId, balance: Either[String, Int])
}

class SchemaShapesOpenApiSpec extends AnyFunSpec with Matchers {
  import SchemaShapesOpenApiFixtures._

  describe("a discriminated sum type") {
    val openAPI = generate(body(Schema.derivedSum[Payment](SumEncoding.Discriminator("type"))))
    val schema  = requestSchema(openAPI).asInstanceOf[ComposedSchema]

    it("is oneOf $refs to hoisted subtypes, with discriminator and mapping") {
      schema.getType shouldBe null
      schema.getOneOf.asScala.map(_.get$ref) shouldBe Seq(
        "#/components/schemas/Card",
        "#/components/schemas/Cash",
        "#/components/schemas/Transfer"
      )
      schema.getDiscriminator.getPropertyName shouldBe "type"
      schema.getDiscriminator.getMapping.asScala shouldBe Map(
        "Card"     -> "#/components/schemas/Card",
        "Cash"     -> "#/components/schemas/Cash",
        "Transfer" -> "#/components/schemas/Transfer"
      )
    }

    it("hoists each subtype with the tag as a required single-value property") {
      val card = component(openAPI, "Card")
      card.getProperties.keySet.asScala shouldBe Set("type", "last4")
      card.getProperties.get("type").getEnum.asScala shouldBe Seq("Card")
      card.getRequired.asScala.toSet shouldBe Set("type", "last4")
      component(openAPI, "Cash").getRequired.asScala shouldBe Seq("type")
    }
  }

  describe("the wrapper-object and untagged encodings") {
    it("stay inline as oneOf") {
      val wrapped = requestSchema(generate(body(Schema.derivedSum[Payment](SumEncoding.WrapperObject)))).asInstanceOf[ComposedSchema]
      wrapped.getDiscriminator shouldBe null
      wrapped.getOneOf.asScala.map(_.getProperties.keySet.asScala.toSet) shouldBe Seq(Set("Card"), Set("Cash"), Set("Transfer"))
      wrapped.getOneOf.asScala.foreach(_.getRequired.asScala should have size 1)

      val untagged = requestSchema(generate(body(Schema.derivedSum[Payment](SumEncoding.Untagged)))).asInstanceOf[ComposedSchema]
      untagged.getOneOf.asScala.map(_.getExtensions.get("x-class")) shouldBe Seq("Card", "Cash", "Transfer")
    }
  }

  describe("a recursive case class") {
    val openAPI = generate(body(implicitly[Schema[Category]]))

    it("is hoisted to components and referenced by $ref, also from itself") {
      requestSchema(openAPI).get$ref shouldBe "#/components/schemas/Category"
      val category = component(openAPI, "Category")
      category.getProperties.get("children").getItems.get$ref shouldBe "#/components/schemas/Category"
      val parent = category.getProperties.get("parent").asInstanceOf[ComposedSchema]
      parent.getNullable shouldBe true
      parent.getAllOf.asScala.map(_.get$ref) shouldBe Seq("#/components/schemas/Category")
      category.getRequired.asScala.toSet shouldBe Set("name", "children")
    }
  }

  describe("a recursive sum type") {
    it("references the hoisted sum from its subtypes") {
      val openAPI = generate(body(Schema.derivedSum[Expr](SumEncoding.Discriminator("op"))))
      requestSchema(openAPI).get$ref shouldBe "#/components/schemas/Expr"
      component(openAPI, "Expr").getDiscriminator.getPropertyName shouldBe "op"
      component(openAPI, "Add").getProperties.get("left").get$ref shouldBe "#/components/schemas/Expr"
    }
  }

  describe("value classes and Either") {
    it("render as the wrapped value and as oneOf") {
      val schema  = requestSchema(generate(body(implicitly[Schema[Account]])))
      val id      = schema.getProperties.get("id")
      val balance = schema.getProperties.get("balance").asInstanceOf[ComposedSchema]
      id.getType shouldBe "integer"
      id.getFormat shouldBe "int64"
      balance.getOneOf.asScala.map(_.getType) shouldBe Seq("string", "integer")
    }
  }

  describe("the generated document") {
    it("is accepted by the OpenAPI parser with every $ref resolving") {
      val openAPI = generate(
        body(Schema.derivedSum[Payment](SumEncoding.Discriminator("type")), "/payments"),
        body(Schema.derivedSum[Expr](SumEncoding.Discriminator("op")), "/expressions"),
        body(implicitly[Schema[Category]], "/categories")
      ).info(new Info().title("shapes").version("1"))

      val parsed = new OpenAPIV3Parser().readContents(Yaml.pretty(openAPI))
      Option(parsed.getMessages).map(_.asScala.toList).getOrElse(Nil) shouldBe empty
      parsed.getOpenAPI.getComponents.getSchemas.keySet.asScala shouldBe Set("Add", "Card", "Cash", "Category", "Expr", "Lit", "Transfer")
    }
  }

  private def component(openAPI: OpenAPI, name: String): OpenApiSchema[?] =
    openAPI.getComponents.getSchemas.get(name)

  private def requestSchema(openAPI: OpenAPI): OpenApiSchema[?] =
    openAPI.getPaths.asScala.values.head.getPost.getRequestBody.getContent.get("application/json").getSchema

  private def generate(calls: BaklavaSerializableCall*): OpenAPI = {
    val openAPI = new OpenAPI()
    BaklavaDslFormatterOpenAPIWorker.generateForCalls(openAPI, calls)
    openAPI
  }

  private def body(schema: Schema[?], path: String = "/shapes"): BaklavaSerializableCall =
    BaklavaSerializableCall(
      request = BaklavaRequestContextSerializable(
        symbolicPath = path,
        path = path,
        pathDescription = None,
        pathSummary = None,
        method = Some(Method("POST")),
        operationDescription = None,
        operationSummary = None,
        operationId = None,
        operationTags = Nil,
        securitySchemes = Nil,
        bodySchema = Some(BaklavaSchemaSerializable(schema)),
        bodyString = "{}",
        headersSeq = Nil,
        pathParametersSeq = Nil,
        queryParametersSeq = Nil,
        responseDescription = Some("ok"),
        responseHeaders = Nil
      ),
      response = BaklavaResponseContextSerializable(
        protocol = BaklavaHttpProtocol("HTTP/1.1"),
        status = StatusCode(200),
        headers = Seq.empty,
        bodyString = "",
        requestContentType = Some("application/json"),
        responseContentType = None,
        bodySchema = None
      )
    )
}
