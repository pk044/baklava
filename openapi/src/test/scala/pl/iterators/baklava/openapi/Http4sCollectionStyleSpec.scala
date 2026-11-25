package pl.iterators.baklava.openapi

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import io.swagger.v3.oas.models.OpenAPI
import org.http4s.Method.GET
import org.http4s.dsl.io.*
import org.http4s.{HttpRoutes, Request, Response}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import pl.iterators.baklava.http4s.BaklavaHttp4s
import pl.iterators.baklava.scalatest.{BaklavaScalatest, ScalatestAsExecution}
import pl.iterators.baklava.{BaklavaTestFrameworkDslInMemory, CollectionStyle, PathStyle}

class Http4sCollectionStyleSpec
    extends AnyFunSpec
    with Matchers
    with BaklavaHttp4s[Unit, Unit, ScalatestAsExecution]
    with BaklavaScalatest[HttpRoutes[IO], BaklavaHttp4s.ToEntityMarshaller, BaklavaHttp4s.FromEntityUnmarshaller]
    with BaklavaTestFrameworkDslInMemory[
      HttpRoutes[IO],
      BaklavaHttp4s.ToEntityMarshaller,
      BaklavaHttp4s.FromEntityUnmarshaller,
      Unit,
      Unit,
      ScalatestAsExecution
    ] {

  override implicit val runtime: IORuntime       = IORuntime.global
  override def strictHeaderCheckDefault: Boolean = false

  private val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case request @ GET -> Root / "query" / _ => Ok(request.multiParams.getOrElse("ids", Nil).mkString("\n"))
    case GET -> Root / "path" / _ / segment  => Ok(segment)
  }

  override def performRequest(routes: HttpRoutes[IO], request: Request[IO]): Response[IO] =
    routes.orNotFound.run(request).unsafeRunSync()

  private def queryCase(name: String, style: CollectionStyle, wire: String, received: String): Unit =
    path(s"/query/$name")(
      supports(GET, queryParameters = q[Seq[Int]]("ids", collectionStyle = style))(
        onRequest(queryParameters = Seq(1, 2, 3))
          .respondsWith[String](Ok, description = s"values as the route receives them for $style")
          .assert { ctx =>
            val response = ctx.performRequest(routes)
            response.rawRequest.uri.query.renderString shouldBe wire
            response.body shouldBe received
          }
      )
    )

  private def pathCase(name: String, style: PathStyle, segment: String): Unit =
    path(s"/path/$name/{ids}")(
      supports(GET, pathParameters = p[Seq[Int]]("ids", style = style))(
        onRequest(pathParameters = Seq(3, 4, 5))
          .respondsWith[String](Ok, description = s"segment as the route receives it for $style")
          .assert { ctx =>
            val response = ctx.performRequest(routes)
            response.rawRequest.uri.path.renderString shouldBe s"/path/$name/$segment"
            response.body shouldBe segment
          }
      )
    )

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
    super.afterAll()
  }
}
