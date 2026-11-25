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

class MultipleBodySchemasSpec extends AnyFunSpec with Matchers {

  case class VoiceSessionResponse(voice: String)
  case class TextSessionResponse(text: String)
  case class Ping(at: Long)

  private val voice = BaklavaSchemaSerializable(Schema.derived[VoiceSessionResponse])
  private val text  = BaklavaSchemaSerializable(Schema.derived[TextSessionResponse])
  private val ping  = BaklavaSchemaSerializable(Schema.derived[Ping])

  describe("OpenAPI response schema when one status returns several body types (#139)") {

    it("documents every distinct schema as oneOf, in example order") {
      val openAPI = generate(
        call(description = "Voice session", response = Some(voice)),
        call(description = "Text session", response = Some(text))
      )

      xClasses(responseSchema(openAPI, "200", "application/json")) shouldBe Some(Seq("TextSessionResponse", "VoiceSessionResponse"))
      responseExamples(openAPI, "200", "application/json") shouldBe Seq("Text session", "Voice session")
    }

    it("keeps a single plain schema when every call returns the same type") {
      val openAPI = generate(
        call(description = "First", response = Some(voice)),
        call(description = "Second", response = Some(voice))
      )

      val schema = responseSchema(openAPI, "200", "application/json")
      schema should not be a[ComposedSchema]
      schema.getExtensions.get("x-class") shouldBe "VoiceSessionResponse"
    }

    it("keeps schemas of different statuses apart") {
      val openAPI = generate(
        call(description = "Voice session", response = Some(voice)),
        call(description = "Not found", status = 404, response = Some(text))
      )

      responseSchema(openAPI, "200", "application/json").getExtensions.get("x-class") shouldBe "VoiceSessionResponse"
      responseSchema(openAPI, "404", "application/json").getExtensions.get("x-class") shouldBe "TextSessionResponse"
    }

    it("keeps schemas of different content types apart") {
      val openAPI = generate(
        call(description = "Voice session", response = Some(voice)),
        call(description = "Plain", response = Some(BaklavaSchemaSerializable(Schema.stringSchema)), responseContentType = "text/plain")
      )

      responseSchema(openAPI, "200", "application/json").getExtensions.get("x-class") shouldBe "VoiceSessionResponse"
      responseSchema(openAPI, "200", "text/plain").getType shouldBe "string"
    }

    it("ignores calls that captured no response body when combining") {
      val openAPI = generate(
        call(description = "Voice session", response = Some(voice)),
        call(description = "Nothing", response = None)
      )

      responseSchema(openAPI, "200", "application/json").getExtensions.get("x-class") shouldBe "VoiceSessionResponse"
    }
  }

  describe("OpenAPI request body schema when calls send several body types (#139)") {

    it("documents every distinct schema as oneOf") {
      val openAPI = generate(
        call(description = "Voice session", request = Some(voice)),
        call(description = "Text session", request = Some(text)),
        call(description = "Ping", request = Some(ping))
      )

      xClasses(requestSchema(openAPI)) shouldBe Some(Seq("Ping", "TextSessionResponse", "VoiceSessionResponse"))
    }

    it("keeps a single plain schema when every call sends the same type") {
      val openAPI = generate(
        call(description = "First", request = Some(ping)),
        call(description = "Second", request = Some(ping))
      )

      requestSchema(openAPI) should not be a[ComposedSchema]
      requestSchema(openAPI).getExtensions.get("x-class") shouldBe "Ping"
    }
  }

  describe("generated document (#139)") {

    it("is accepted by the OpenAPI parser with oneOf on both request and response") {
      val openAPI = generate(
        call(description = "Voice session", request = Some(ping), response = Some(voice)),
        call(description = "Text session", request = Some(text), response = Some(text))
      ).info(new Info().title("bodies").version("1"))

      val parsed = new OpenAPIV3Parser().readContents(Yaml.pretty(openAPI))

      Option(parsed.getMessages).map(_.asScala.toList).getOrElse(Nil) shouldBe empty
      val operation = parsed.getOpenAPI.getPaths.get("/session").getPost
      operation.getRequestBody.getContent.get("application/json").getSchema.getOneOf should have size 2
      operation.getResponses.get("200").getContent.get("application/json").getSchema.getOneOf should have size 2
    }
  }

  private def xClasses(schema: OpenApiSchema[?]): Option[Seq[String]] =
    Option(schema.getOneOf).map(_.asScala.toSeq.map(_.getExtensions.get("x-class").toString))

  private def responseSchema(openAPI: OpenAPI, status: String, contentType: String): OpenApiSchema[?] =
    openAPI.getPaths.get("/session").getPost.getResponses.get(status).getContent.get(contentType).getSchema

  private def responseExamples(openAPI: OpenAPI, status: String, contentType: String): Seq[String] =
    openAPI.getPaths.get("/session").getPost.getResponses.get(status).getContent.get(contentType).getExamples.asScala.keys.toSeq

  private def requestSchema(openAPI: OpenAPI): OpenApiSchema[?] =
    openAPI.getPaths.get("/session").getPost.getRequestBody.getContent.get("application/json").getSchema

  private def generate(calls: BaklavaSerializableCall*): OpenAPI = {
    val openAPI = new OpenAPI()
    BaklavaDslFormatterOpenAPIWorker.generateForCalls(openAPI, calls)
    openAPI
  }

  private def call(
      description: String,
      status: Int = 200,
      request: Option[BaklavaSchemaSerializable] = None,
      response: Option[BaklavaSchemaSerializable] = None,
      responseContentType: String = "application/json"
  ): BaklavaSerializableCall =
    BaklavaSerializableCall(
      request = BaklavaRequestContextSerializable(
        symbolicPath = "/session",
        path = "/session",
        pathDescription = None,
        pathSummary = None,
        method = Some(Method("POST")),
        operationDescription = None,
        operationSummary = None,
        operationId = None,
        operationTags = Nil,
        securitySchemes = Nil,
        bodySchema = request,
        bodyString = request.fold("")(_ => "{}"),
        headersSeq = Nil,
        pathParametersSeq = Nil,
        queryParametersSeq = Nil,
        responseDescription = Some(description),
        responseHeaders = Nil
      ),
      response = BaklavaResponseContextSerializable(
        protocol = BaklavaHttpProtocol("HTTP/1.1"),
        status = StatusCode(status),
        headers = Seq.empty,
        bodyString = response.fold("")(_ => "{}"),
        requestContentType = request.map(_ => "application/json"),
        responseContentType = response.map(_ => responseContentType),
        bodySchema = response
      )
    )
}
