package pl.iterators.baklava

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import pl.iterators.baklava.BaklavaSerialize.serializableCallCodec

class PathStyleSpec extends AnyFunSpec with Matchers with BaklavaPathParams {

  private val template = "/users/{id}/posts"

  private def resolve[P, V](parameters: P, provided: V, uri: String = template)(implicit provide: ProvidePathParams[P, V]): String =
    provide(parameters, provided, uri)

  private def captured(parameter: PathParam[?], path: String): BaklavaPathParamSerializable =
    BaklavaRequestContextSerializable(
      CallFixtures.requestContextAt(template, path, pathParameters = Seq(parameter)),
      ""
    ).pathParametersSeq.head

  private val scalarSegments = Map[PathStyle, String](
    PathStyle.Simple         -> "5",
    PathStyle.Label          -> ".5",
    PathStyle.LabelExploded  -> ".5",
    PathStyle.Matrix         -> ";id=5",
    PathStyle.MatrixExploded -> ";id=5"
  )

  private val collectionSegments = Map[PathStyle, String](
    PathStyle.Simple         -> "3,4,5",
    PathStyle.Label          -> ".3,4,5",
    PathStyle.LabelExploded  -> ".3.4.5",
    PathStyle.Matrix         -> ";id=3,4,5",
    PathStyle.MatrixExploded -> ";id=3;id=4;id=5"
  )

  describe("PathStyle") {

    it("maps every style to its OpenAPI style and explode pair") {
      PathStyle.values.map(s => (s, s.style, s.explode)) shouldBe Seq(
        (PathStyle.Simple, "simple", false),
        (PathStyle.Label, "label", false),
        (PathStyle.LabelExploded, "label", true),
        (PathStyle.Matrix, "matrix", false),
        (PathStyle.MatrixExploded, "matrix", true)
      )
    }

    it("defaults to Simple") {
      p[Int]("id").style shouldBe PathStyle.Simple
      PathParam[Int]("id", None).style shouldBe PathStyle.Simple
    }
  }

  describe("path built for a single value") {

    it("renders every style") {
      PathStyle.values.foreach { style =>
        resolve(p[Int]("id", style = style), 5) shouldBe s"/users/${scalarSegments(style)}/posts"
      }
    }

    it("percent-encodes the value") {
      resolve(p[String]("id", style = PathStyle.Matrix), "a;b=c") shouldBe "/users/;id=a%3Bb%3Dc/posts"
    }

  }

  describe("path built for a collection") {

    it("renders every style") {
      PathStyle.values.foreach { style =>
        resolve(p[Seq[Int]]("id", style = style), Seq(3, 4, 5)) shouldBe s"/users/${collectionSegments(style)}/posts"
      }
    }

    it("percent-encodes each value on its own so the delimiter stays distinguishable") {
      resolve(p[Seq[String]]("id"), Seq("a,b", "c")) shouldBe "/users/a%2Cb,c/posts"
    }

    it("supports List, Vector, Set and Array") {
      resolve(p[List[Int]]("id"), List(1, 2)) shouldBe "/users/1,2/posts"
      resolve(p[Vector[Int]]("id"), Vector(1, 2)) shouldBe "/users/1,2/posts"
      resolve(p[Set[Int]]("id"), Set(3, 1, 2)) shouldBe "/users/1,2,3/posts"
      resolve(p[Array[Int]]("id"), Array(1, 2)) shouldBe "/users/1,2/posts"
    }

    it("applies each parameter's own style in a tuple") {
      val parameters = (p[Seq[Int]]("ids", style = PathStyle.MatrixExploded), p[Int]("version", style = PathStyle.Label))
      resolve(parameters, (Seq(1, 2), 3), "/v/{version}/items/{ids}") shouldBe "/v/.3/items/;ids=1;ids=2"
    }
  }

  describe("ToPathParam") {

    it("treats an instance that only defines apply as a single value") {
      case class UserId(value: Int)
      implicit val userIdSchema: Schema[UserId]           = Schema.intSchema.asInstanceOf[Schema[UserId]]
      implicit val userIdToPathParam: ToPathParam[UserId] = new ToPathParam[UserId] {
        override def apply(t: UserId): String = t.value.toString
      }
      resolve(p[UserId]("id", style = PathStyle.Matrix), UserId(7)) shouldBe "/users/;id=7/posts"
    }
  }

  describe("captured path parameter") {

    it("carries the style and the unwrapped value for a single value") {
      PathStyle.values.foreach { style =>
        val parameter  = p[Int]("id", style = style)
        val serialized = captured(parameter, resolve(parameter, 5))
        serialized.style shouldBe style
        serialized.example shouldBe Some("5")
        serialized.exampleValues shouldBe empty
      }
    }

    it("does not split a single value on the style's delimiter") {
      val parameter = p[Double]("id", style = PathStyle.LabelExploded)
      captured(parameter, resolve(parameter, 1.5)).example shouldBe Some("1.5")
    }

    it("carries the style and the individual values for a collection") {
      val expected = Seq("a,b", "c d", "e;f")
      PathStyle.values.foreach { style =>
        val parameter  = p[Seq[String]]("id", style = style)
        val serialized = captured(parameter, resolve(parameter, expected))
        serialized.style shouldBe style
        serialized.exampleValues shouldBe expected
        serialized.example shouldBe Some("a,b,c d,e;f")
      }
    }

    it("round-trips the style through the stored call format") {
      val parameter = p[Seq[Int]]("id", style = PathStyle.MatrixExploded)
      val path      = resolve(parameter, Seq(3, 4))
      val call      = CallFixtures.callAt(template, path)
      val stored    = call.copy(request = call.request.copy(pathParametersSeq = Seq(captured(parameter, path))))

      val restored = readFromString[BaklavaSerializableCall](writeToString(stored)).request.pathParametersSeq.head
      restored.style shouldBe PathStyle.MatrixExploded
      restored.exampleValues shouldBe Seq("3", "4")
    }

    it("stores a Simple single value exactly as before styles existed") {
      val parameter = p[Int]("id")
      val call      = CallFixtures.callAt(template, "/users/5/posts")
      val stored    = writeToString(call.copy(request = call.request.copy(pathParametersSeq = Seq(captured(parameter, "/users/5/posts")))))

      stored should not include "style"
      stored should not include "exampleValues"
      readFromString[BaklavaSerializableCall](stored).request.pathParametersSeq.head.style shouldBe PathStyle.Simple
    }
  }
}
