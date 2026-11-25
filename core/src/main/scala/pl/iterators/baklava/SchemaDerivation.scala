package pl.iterators.baklava

trait SchemaDerivation extends SchemaDerivationCompat {
  // override to match the codec's wire naming (e.g. SchemaNameTransform.snakeCase)
  def transformMemberName(name: String): String = name

  def transformConstructorName(name: String): String = name

  // called by the derivation macro
  def deriveProduct[T](className: String, fields: List[SchemaDerivation.Field]): Schema[T] = {
    val _className = className
    new Schema[T] {
      val className: String                       = _className
      val `type`: SchemaType                      = SchemaType.ObjectType
      val format: Option[String]                  = None
      lazy val properties: Map[String, Schema[?]] = fields.map { field =>
        field.default match {
          // certainly not perfect as it depends on JSON serialization
          case Some(default) => transformMemberName(field.label) -> field.schema.asInstanceOf[Schema[Any]].withDefault(default)
          case None          => transformMemberName(field.label) -> field.schema
        }
      }.toMap
      val items: Option[Schema[?]]      = None
      val `enum`: Option[Set[String]]   = None
      val required: Boolean             = true
      val additionalProperties: Boolean = false
      val default: Option[T]            = None
      val description: Option[String]   = None
    }
  }

  def deriveSum[T](className: String, encoding: SumEncoding, subtypes: List[SchemaDerivation.Subtype]): Schema[T] = {
    val tags = subtypes.map(subtype => transformConstructorName(subtype.name))
    encoding match {
      case SumEncoding.Discriminator(propertyName) =>
        Schema.oneOfSchema[T](className, subtypes.map(_.schema), Some(SchemaDiscriminator(propertyName, tags)))
      case SumEncoding.WrapperObject =>
        Schema.oneOfSchema[T](className, subtypes.zip(tags).map { case (subtype, tag) => Schema.wrapperObject(tag, subtype.schema) })
      case SumEncoding.Untagged =>
        Schema.oneOfSchema[T](className, subtypes.map(_.schema))
    }
  }
}

object SchemaDerivation {
  final class Field(val label: String, schemaThunk: => Schema[?], val default: Option[Any]) {
    lazy val schema: Schema[?] = schemaThunk
  }

  final class Subtype(val name: String, schemaThunk: => Schema[?]) {
    lazy val schema: Schema[?] = schemaThunk
  }
}
