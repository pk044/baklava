package pl.iterators.baklava.openapi

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.parameters.Parameter

import scala.jdk.CollectionConverters.*

object CollectionStyleAssertions {

  private case class Documented(style: String, explode: Boolean, example: List[Long])

  private val expected: Map[String, Documented] = Map(
    "/query/exploded"             -> Documented("form", explode = true, List(1, 2, 3)),
    "/query/comma"                -> Documented("form", explode = false, List(1, 2, 3)),
    "/query/pipe"                 -> Documented("pipeDelimited", explode = false, List(1, 2, 3)),
    "/query/space"                -> Documented("spaceDelimited", explode = false, List(1, 2, 3)),
    "/path/simple/{ids}"          -> Documented("simple", explode = false, List(3, 4, 5)),
    "/path/label/{ids}"           -> Documented("label", explode = false, List(3, 4, 5)),
    "/path/label-exploded/{ids}"  -> Documented("label", explode = true, List(3, 4, 5)),
    "/path/matrix/{ids}"          -> Documented("matrix", explode = false, List(3, 4, 5)),
    "/path/matrix-exploded/{ids}" -> Documented("matrix", explode = true, List(3, 4, 5))
  )

  def assertDocumented(openAPI: OpenAPI): Unit = {
    val actual = expected.keys.map(path => path -> documented(parameter(openAPI, path))).toMap
    if (actual != expected)
      throw new AssertionError(
        (expected.keys.toList.sorted
          .filter(path => actual(path) != expected(path))
          .map(path => s"$path: expected ${expected(path)}, got ${actual(path)}"))
          .mkString("Collection parameters documented differently from the wire format:\n  ", "\n  ", "")
      )
  }

  private def parameter(openAPI: OpenAPI, path: String): Parameter =
    Option(openAPI.getPaths)
      .flatMap(paths => Option(paths.get(path)))
      .flatMap(item => Option(item.getGet))
      .flatMap(operation => Option(operation.getParameters))
      .flatMap(_.asScala.find(_.getName == "ids"))
      .getOrElse(throw new AssertionError(s"No `ids` parameter documented for GET $path"))

  private def documented(parameter: Parameter): Documented =
    Documented(
      style = String.valueOf(parameter.getStyle),
      explode = parameter.getExplode,
      example = parameter.getExample match {
        case items: java.util.List[?] => items.asScala.toList.collect { case item: java.lang.Long => item.longValue }
        case _                        => Nil
      }
    )
}
