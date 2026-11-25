package baklavaclient.catalog

final case class Category(children: Seq[Category], id: Long, name: String)

object Category {
  implicit lazy val codec: io.circe.Codec.AsObject[Category] = io.circe.generic.semiauto.deriveCodec[Category]
}
