package pl.iterators.baklava.openapi

import io.swagger.v3.core.util.Yaml
import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.info.Info
import io.swagger.v3.oas.models.parameters.Parameter
import io.swagger.v3.parser.OpenAPIV3Parser
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import pl.iterators.baklava.*
import sttp.model.{Method, StatusCode}

import scala.jdk.CollectionConverters.*

class ParameterStyleSpec extends AnyFunSpec with Matchers {

  private val ints   = BaklavaSchemaSerializable(Schema.seqSchema(Schema.intSchema))
  private val int    = BaklavaSchemaSerializable(Schema.intSchema)
  private val string = BaklavaSchemaSerializable(Schema.stringSchema)

  describe("OpenAPI style and explode for query parameters (#90)") {

    it("emits the pair matching every CollectionStyle") {
      val emitted = CollectionStyle.values.map { style =>
        val parameter = queryParameter(BaklavaQueryParamSerializable("ids", None, ints, collectionStyle = style))
        (style, parameter.getStyle.toString, parameter.getExplode.booleanValue)
      }

      emitted shouldBe Seq(
        (CollectionStyle.Exploded, "form", true),
        (CollectionStyle.CommaSeparated, "form", false),
        (CollectionStyle.PipeDelimited, "pipeDelimited", false),
        (CollectionStyle.SpaceDelimited, "spaceDelimited", false)
      )
    }

    it("emits form and explode for a parameter that is not a collection") {
      val parameter = queryParameter(BaklavaQueryParamSerializable("page", None, int))

      parameter.getStyle shouldBe Parameter.StyleEnum.FORM
      parameter.getExplode shouldBe true
    }
  }

  describe("OpenAPI style and explode for path parameters (#90)") {

    it("emits the pair matching every PathStyle") {
      val emitted = PathStyle.values.map { style =>
        val parameter = pathParameter(BaklavaPathParamSerializable("ids", None, ints, style = style))
        (style, parameter.getStyle.toString, parameter.getExplode.booleanValue)
      }

      emitted shouldBe Seq(
        (PathStyle.Simple, "simple", false),
        (PathStyle.Label, "label", false),
        (PathStyle.LabelExploded, "label", true),
        (PathStyle.Matrix, "matrix", false),
        (PathStyle.MatrixExploded, "matrix", true)
      )
    }

    it("emits simple and no explode for a default path parameter") {
      val parameter = pathParameter(BaklavaPathParamSerializable("id", None, string))

      parameter.getStyle shouldBe Parameter.StyleEnum.SIMPLE
      parameter.getExplode shouldBe false
    }
  }

  describe("examples of collection parameters (#90)") {

    it("emits the captured values as an array coerced to the item type") {
      val parameter = queryParameter(
        BaklavaQueryParamSerializable("ids", None, ints, Some("1,2"), CollectionStyle.PipeDelimited, Seq("1", "2"))
      )

      parameter.getExample shouldBe java.util.Arrays.asList[Object](java.lang.Long.valueOf(1), java.lang.Long.valueOf(2))
    }

    it("emits one named array example per scenario when the captured values differ") {
      val first = call("/items", query = Seq(BaklavaQueryParamSerializable("ids", None, ints, Some("1,2"), exampleValues = Seq("1", "2"))))
      val second = call("/items", query = Seq(BaklavaQueryParamSerializable("ids", None, ints, Some("3"), exampleValues = Seq("3"))))
      val named  = generate(
        Seq(scenario(first, "two ids"), scenario(second, "one id"))
      ).getPaths.get("/items").getGet.getParameters.asScala.head.getExamples.asScala

      named("two ids").getValue shouldBe java.util.Arrays.asList[Object](java.lang.Long.valueOf(1), java.lang.Long.valueOf(2))
      named("one id").getValue shouldBe java.util.Arrays.asList[Object](java.lang.Long.valueOf(3))
    }

    it("emits a path collection as an array too") {
      val parameter = pathParameter(
        BaklavaPathParamSerializable("ids", None, ints, Some("3,4"), PathStyle.MatrixExploded, Seq("3", "4"))
      )

      parameter.getExample shouldBe java.util.Arrays.asList[Object](java.lang.Long.valueOf(3), java.lang.Long.valueOf(4))
    }

    it("keeps the joined string for a call stored before item values were captured") {
      val parameter = queryParameter(BaklavaQueryParamSerializable("ids", None, ints, Some("1,2")))

      parameter.getExample shouldBe "1,2"
    }
  }

  describe("generated document (#90)") {

    it("is accepted by the OpenAPI parser for every style") {
      val calls = CollectionStyle.values.map { style =>
        call(s"/query/$style", query = Seq(BaklavaQueryParamSerializable("ids", None, ints, Some("1,2"), style, Seq("1", "2"))))
      } ++ PathStyle.values.map { style =>
        call(s"/path/$style/{ids}", path = Seq(BaklavaPathParamSerializable("ids", None, ints, Some("3,4"), style, Seq("3", "4"))))
      }
      val openAPI = generate(calls).info(new Info().title("styles").version("1"))

      val parsed = new OpenAPIV3Parser().readContents(Yaml.pretty(openAPI))

      Option(parsed.getMessages).map(_.asScala.toList).getOrElse(Nil) shouldBe empty
      parsed.getOpenAPI.getPaths.get("/query/PipeDelimited").getGet.getParameters.asScala.head.getStyle shouldBe
      Parameter.StyleEnum.PIPEDELIMITED
      parsed.getOpenAPI.getPaths.get("/path/MatrixExploded/{ids}").getGet.getParameters.asScala.head.getStyle shouldBe
      Parameter.StyleEnum.MATRIX
    }
  }

  private def queryParameter(parameter: BaklavaQueryParamSerializable): Parameter =
    generate(Seq(call("/items", query = Seq(parameter)))).getPaths.get("/items").getGet.getParameters.asScala.head

  private def pathParameter(parameter: BaklavaPathParamSerializable): Parameter =
    generate(Seq(call("/items/{" + parameter.name + "}", path = Seq(parameter)))).getPaths
      .get("/items/{" + parameter.name + "}")
      .getGet
      .getParameters
      .asScala
      .head

  private def generate(calls: Seq[BaklavaSerializableCall]): OpenAPI = {
    val openAPI = new OpenAPI()
    BaklavaDslFormatterOpenAPIWorker.generateForCalls(openAPI, calls)
    openAPI
  }

  private def scenario(call: BaklavaSerializableCall, name: String): BaklavaSerializableCall =
    call.copy(request = call.request.copy(responseDescription = Some(name)))

  private def call(
      symbolicPath: String,
      query: Seq[BaklavaQueryParamSerializable] = Nil,
      path: Seq[BaklavaPathParamSerializable] = Nil
  ): BaklavaSerializableCall =
    BaklavaSerializableCall(
      request = BaklavaRequestContextSerializable(
        symbolicPath = symbolicPath,
        path = symbolicPath,
        pathDescription = None,
        pathSummary = None,
        method = Some(Method("GET")),
        operationDescription = None,
        operationSummary = None,
        operationId = None,
        operationTags = Nil,
        securitySchemes = Nil,
        bodySchema = None,
        bodyString = "",
        headersSeq = Nil,
        pathParametersSeq = path,
        queryParametersSeq = query,
        responseDescription = Some("ok"),
        responseHeaders = Nil
      ),
      response = BaklavaResponseContextSerializable(
        protocol = BaklavaHttpProtocol("HTTP/1.1"),
        status = StatusCode(200),
        headers = Seq.empty,
        bodyString = "",
        requestContentType = None,
        responseContentType = None,
        bodySchema = None
      )
    )
}
