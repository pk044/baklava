package pl.iterators.baklava

sealed trait SumEncoding

object SumEncoding {
  final case class Discriminator(propertyName: String) extends SumEncoding

  case object WrapperObject extends SumEncoding

  case object Untagged extends SumEncoding
}
