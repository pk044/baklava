package pl.iterators.baklava.openapi

import io.swagger.v3.oas.models.OpenAPI
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.marshalling.ToEntityMarshaller
import org.apache.pekko.http.scaladsl.model.HttpMethods.GET
import org.apache.pekko.http.scaladsl.model.StatusCodes.OK
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import org.apache.pekko.stream.Materializer
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import pl.iterators.baklava.pekkohttp.BaklavaPekkoHttp
import pl.iterators.baklava.scalatest.{BaklavaScalatest, ScalatestAsExecution}
import pl.iterators.baklava.{BaklavaTestFrameworkDslInMemory, CollectionStyle, PathStyle}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}

class PekkoCollectionStyleSpec
    extends AnyFunSpec
    with Matchers
    with BaklavaPekkoHttp[Unit, Unit, ScalatestAsExecution]
    with BaklavaScalatest[Route, ToEntityMarshaller, FromEntityUnmarshaller]
    with BaklavaTestFrameworkDslInMemory[Route, ToEntityMarshaller, FromEntityUnmarshaller, Unit, Unit, ScalatestAsExecution] {

  private implicit val system: ActorSystem                        = ActorSystem("pekko-collection-style")
  implicit val executionContext: ExecutionContext                 = system.dispatcher
  implicit val materializer: Materializer                         = Materializer(system)
  implicit val stringUnmarshaller: FromEntityUnmarshaller[String] = Unmarshaller.stringUnmarshaller

  override def strictHeaderCheckDefault: Boolean = false

  private val routes: Route = PekkoCollectionStyleSpec.routes

  override def performRequest(routes: Route, request: HttpRequest): HttpResponse =
    Await.result(Route.toFunction(routes).apply(request), 10.seconds)

  private def queryCase(name: String, style: CollectionStyle, wire: String, received: String): Unit =
    path(s"/query/$name")(
      supports(GET, queryParameters = q[Seq[Int]]("ids", collectionStyle = style))(
        onRequest(queryParameters = Seq(1, 2, 3))
          .respondsWith[String](OK, description = s"values as the route receives them for $style")
          .assert { ctx =>
            val response = ctx.performRequest(routes)
            response.rawRequest.uri.rawQueryString shouldBe Some(wire)
            response.body shouldBe received
          }
      )
    )

  private def pathCase(name: String, style: PathStyle, segment: String): Unit =
    path(s"/path/$name/{ids}")(
      supports(GET, pathParameters = p[Seq[Int]]("ids", style = style))(
        onRequest(pathParameters = Seq(3, 4, 5))
          .respondsWith[String](OK, description = s"segment as the route receives it for $style")
          .assert { ctx =>
            val response = ctx.performRequest(routes)
            response.rawRequest.uri.path.toString shouldBe s"/path/$name/$segment"
            response.body shouldBe segment
          }
      )
    )

  private def typedCase(name: String, style: CollectionStyle, directive: String): Unit =
    path(s"/typed/$name")(
      supports(GET, queryParameters = q[Seq[Int]]("ids", collectionStyle = style))(
        onRequest(queryParameters = Seq(1, 2, 3))
          .respondsWith[String](OK, description = s"$style decoded by $directive")
          .assert { ctx =>
            ctx.performRequest(routes).body shouldBe "1+2+3"
          }
      )
    )

  typedCase("exploded", CollectionStyle.Exploded, directive = "parameters(\"ids\".as[Int].*)")
  typedCase("comma", CollectionStyle.CommaSeparated, directive = "parameter(\"ids\".as(CsvSeq[Int]))")

  queryCase("exploded", CollectionStyle.Exploded, wire = "ids=1&ids=2&ids=3", received = "1\n2\n3")
  queryCase("comma", CollectionStyle.CommaSeparated, wire = "ids=1,2,3", received = "1,2,3")
  queryCase("pipe", CollectionStyle.PipeDelimited, wire = "ids=1%7C2%7C3", received = "1|2|3")
  queryCase("space", CollectionStyle.SpaceDelimited, wire = "ids=1%202%203", received = "1 2 3")

  pathCase("simple", PathStyle.Simple, "3,4,5")
  pathCase("label", PathStyle.Label, ".3,4,5")
  pathCase("label-exploded", PathStyle.LabelExploded, ".3.4.5")
  pathCase("matrix", PathStyle.Matrix, ";ids=3,4,5")
  pathCase("matrix-exploded", PathStyle.MatrixExploded, ";ids=3;ids=4;ids=5")

  override def afterAll(): Unit = {
    val openAPI = new OpenAPI()
    BaklavaDslFormatterOpenAPIWorker.generateForCalls(openAPI, listCalls)
    CollectionStyleAssertions.assertDocumented(openAPI)
    val _ = system.terminate()
    super.afterAll()
  }
}

object PekkoCollectionStyleSpec {
  import org.apache.pekko.http.scaladsl.server.Directives.*
  import org.apache.pekko.http.scaladsl.unmarshalling.PredefinedFromStringUnmarshallers.CsvSeq

  val routes: Route = concat(
    (path("typed" / "exploded") & parameters("ids".as[Int].*)) { ids =>
      complete(ids.toList.sorted.mkString("+"))
    },
    (path("typed" / "comma") & parameter("ids".as(CsvSeq[Int]))) { ids =>
      complete(ids.mkString("+"))
    },
    (pathPrefix("query") & parameterSeq) { parameters =>
      complete(parameters.collect { case ("ids", value) => value }.mkString("\n"))
    },
    path("path" / Segment / Segment) { (_, segment) =>
      complete(segment)
    }
  )
}
