package pl.iterators.baklava

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import pl.iterators.baklava.BaklavaSerialize.serializableCallCodec

class CollectionStyleSpec extends AnyFunSpec with Matchers with BaklavaQueryParams {

  private def resolve[P, V](parameters: P, provided: V, uri: String = "/items")(implicit provide: ProvideQueryParams[P, V]): String =
    provide(parameters, provided, uri)

  private def captured(parameter: QueryParam[?], path: String): BaklavaQueryParamSerializable =
    BaklavaRequestContextSerializable(
      CallFixtures.requestContextAt("/items", path, queryParameters = Seq(parameter)),
      ""
    ).queryParametersSeq.head

  describe("CollectionStyle") {

    it("maps every style to its OpenAPI style and explode pair") {
      CollectionStyle.values.map(s => (s, s.style, s.explode)) shouldBe Seq(
        (CollectionStyle.Exploded, "form", true),
        (CollectionStyle.CommaSeparated, "form", false),
        (CollectionStyle.PipeDelimited, "pipeDelimited", false),
        (CollectionStyle.SpaceDelimited, "spaceDelimited", false)
      )
    }

    it("defaults to Exploded") {
      q[Seq[Int]]("ids").collectionStyle shouldBe CollectionStyle.Exploded
      QueryParam[Seq[Int]]("ids", None).collectionStyle shouldBe CollectionStyle.Exploded
    }
  }

  describe("query string built for a collection parameter") {

    it("repeats the parameter for Exploded") {
      resolve(q[Seq[Int]]("ids"), Seq(1, 2, 3)) shouldBe "/items?ids=1&ids=2&ids=3"
    }

    it("joins the values with a literal comma for CommaSeparated") {
      resolve(q[Seq[Int]]("ids", collectionStyle = CollectionStyle.CommaSeparated), Seq(1, 2, 3)) shouldBe "/items?ids=1,2,3"
    }

    it("joins the values with a percent-encoded pipe for PipeDelimited") {
      resolve(q[Seq[Int]]("ids", collectionStyle = CollectionStyle.PipeDelimited), Seq(1, 2, 3)) shouldBe "/items?ids=1%7C2%7C3"
    }

    it("joins the values with a percent-encoded space for SpaceDelimited") {
      resolve(q[Seq[Int]]("ids", collectionStyle = CollectionStyle.SpaceDelimited), Seq(1, 2, 3)) shouldBe "/items?ids=1%202%203"
    }

    it("percent-encodes each value on its own so the delimiter stays distinguishable") {
      resolve(q[Seq[String]]("tags", collectionStyle = CollectionStyle.CommaSeparated), Seq("a,b", "c d")) shouldBe "/items?tags=a%2Cb,c+d"
      resolve(q[Seq[String]]("tags", collectionStyle = CollectionStyle.SpaceDelimited), Seq("a b", "c")) shouldBe "/items?tags=a+b%20c"
    }

    it("omits the parameter entirely for an empty collection, whatever the style") {
      CollectionStyle.values.foreach { style =>
        resolve(q[Seq[Int]]("ids", collectionStyle = style), Seq.empty[Int]) shouldBe "/items"
      }
    }

    it("supports List, Vector, Set and Array") {
      resolve(q[List[Int]]("ids", collectionStyle = CollectionStyle.CommaSeparated), List(1, 2)) shouldBe "/items?ids=1,2"
      resolve(q[Vector[Int]]("ids", collectionStyle = CollectionStyle.CommaSeparated), Vector(1, 2)) shouldBe "/items?ids=1,2"
      resolve(q[Set[Int]]("ids", collectionStyle = CollectionStyle.CommaSeparated), Set(3, 1, 2)) shouldBe "/items?ids=1,2,3"
      resolve(q[Array[Int]]("ids", collectionStyle = CollectionStyle.CommaSeparated), Array(1, 2)) shouldBe "/items?ids=1,2"
    }

    it("applies the style to an optional collection and omits it when absent") {
      val parameter = q[Option[Seq[Int]]]("ids", collectionStyle = CollectionStyle.PipeDelimited)
      resolve(parameter, Option(Seq(1, 2))) shouldBe "/items?ids=1%7C2"
      resolve(parameter, Option.empty[Seq[Int]]) shouldBe "/items"
    }

    it("keeps declaration order and each parameter's own style in a tuple") {
      val parameters = (
        q[Seq[Int]]("ids", collectionStyle = CollectionStyle.CommaSeparated),
        q[String]("sort"),
        q[Seq[String]]("tags"),
        q[Seq[String]]("fields", collectionStyle = CollectionStyle.PipeDelimited),
        q[Int]("page")
      )
      resolve(parameters, (Seq(1, 2), "name", Seq("a", "b"), Seq("x", "y"), 3)) shouldBe
      "/items?ids=1,2&sort=name&tags=a&tags=b&fields=x%7Cy&page=3"
    }

    it("appends to an existing query and keeps the fragment last") {
      resolve(q[Seq[Int]]("ids", collectionStyle = CollectionStyle.CommaSeparated), Seq(1, 2), "/items?key=k#top") shouldBe
      "/items?key=k&ids=1,2#top"
    }

    it("still explodes values passed to addQueryParametersToUri") {
      addQueryParametersToUri("/items", Map("ids" -> Seq("1", "2"))) shouldBe "/items?ids=1&ids=2"
    }
  }

  describe("QueryParam") {

    it("rejects a non-default style on a parameter that is not a collection") {
      val thrown = the[IllegalArgumentException] thrownBy q[Int]("page", collectionStyle = CollectionStyle.PipeDelimited)
      thrown.getMessage should include("'page'")
      thrown.getMessage should include("PipeDelimited")
    }
  }

  describe("captured query parameter") {

    it("carries the style and the individual values for every style") {
      val expected = Seq("a,b", "c d", "e")
      CollectionStyle.values.foreach { style =>
        val parameter  = q[Seq[String]]("tags", collectionStyle = style)
        val serialized = captured(parameter, resolve(parameter, expected))
        serialized.collectionStyle shouldBe style
        serialized.exampleValues shouldBe expected
        serialized.example shouldBe Some("a,b,c d,e")
      }
    }

    it("reads a pipe inside a PipeDelimited value as the delimiter, exactly as a server would") {
      val parameter = q[Seq[String]]("tags", collectionStyle = CollectionStyle.PipeDelimited)
      val path      = resolve(parameter, Seq("a|b", "c"))
      path shouldBe "/items?tags=a%7Cb%7Cc"
      captured(parameter, path).exampleValues shouldBe Seq("a", "b", "c")
    }

    it("keeps the single example and no item values for a parameter that is not a collection") {
      val parameter  = q[String]("sort")
      val serialized = captured(parameter, resolve(parameter, "a,b"))
      serialized.example shouldBe Some("a,b")
      serialized.exampleValues shouldBe empty
    }

    it("captures nothing when the collection was not sent") {
      val parameter  = q[Seq[Int]]("ids", collectionStyle = CollectionStyle.CommaSeparated)
      val serialized = captured(parameter, resolve(parameter, Seq.empty[Int]))
      serialized.example shouldBe None
      serialized.exampleValues shouldBe empty
    }

    it("round-trips the style through the stored call format") {
      val parameter = q[Seq[Int]]("ids", collectionStyle = CollectionStyle.PipeDelimited)
      val call      = CallFixtures.callAt("/items", "/items?ids=1%7C2")
      val stored    = call.copy(request = call.request.copy(queryParametersSeq = Seq(captured(parameter, "/items?ids=1%7C2"))))

      val restored = readFromString[BaklavaSerializableCall](writeToString(stored)).request.queryParametersSeq.head
      restored.collectionStyle shouldBe CollectionStyle.PipeDelimited
      restored.exampleValues shouldBe Seq("1", "2")
    }

    it("reads a call stored before styles existed as Exploded") {
      val parameter = q[Seq[Int]]("ids")
      val call      = CallFixtures.callAt("/items", "/items?ids=1&ids=2")
      val current   =
        writeToString(call.copy(request = call.request.copy(queryParametersSeq = Seq(captured(parameter, "/items?ids=1&ids=2")))))
      val legacy = current.replace(""","exampleValues":["1","2"]""", "")

      current should not include "collectionStyle"
      legacy should not equal current
      val restored = readFromString[BaklavaSerializableCall](legacy).request.queryParametersSeq.head
      restored.collectionStyle shouldBe CollectionStyle.Exploded
      restored.exampleValues shouldBe empty
      restored.example shouldBe Some("1,2")
    }
  }
}
