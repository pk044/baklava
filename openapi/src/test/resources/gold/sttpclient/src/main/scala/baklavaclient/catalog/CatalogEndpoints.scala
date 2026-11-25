package baklavaclient.catalog

import sttp.client4._
import sttp.client4.circe._
import io.circe.generic.auto._
import sttp.model.Uri

object CatalogEndpoints {

  /** List categories — The category tree */
  def listCategories(
      baseUri: Uri
  ): Request[Either[ResponseException[String], Seq[Category]]] = {
    basicRequest
      .get(baseUri.addPath("categories"))
      .response(asJson[Seq[Category]])
  }
}
