package pl.iterators.baklava

sealed trait SchemaType
object SchemaType {
  case object NullType extends SchemaType

  case object StringType extends SchemaType

  case object BooleanType extends SchemaType

  case object IntegerType extends SchemaType

  case object NumberType extends SchemaType

  case object ArrayType extends SchemaType

  case object ObjectType extends SchemaType
}

final case class SchemaDiscriminator(propertyName: String, values: Seq[String])

@scala.annotation.implicitNotFound(
  "No Schema[${T}] found. Only case classes, case objects and value classes get one derived; for a sealed trait or enum use Schema.derivedSum, or define one."
)
trait Schema[T] {
  val className: String
  val `type`: SchemaType
  val format: Option[String]
  def properties: Map[String, Schema[?]]
  val items: Option[Schema[?]]
  val `enum`: Option[Set[String]]
  val required: Boolean
  val additionalProperties: Boolean
  val default: Option[T]
  val description: Option[String]

  // For map-like object schemas: the schema all values conform to (renders as a JSON Schema /
  // OpenAPI `additionalProperties: <schema>`). `None` for ordinary objects and primitives.
  def additionalPropertiesSchema: Option[Schema[?]] = None

  // may be explicitly null, not just absent — set by optionSchema (#131)
  def nullable: Boolean = false

  def oneOf: Seq[Schema[?]] = Nil

  def discriminator: Option[SchemaDiscriminator] = None

  def recursionKey: AnyRef = this

  def withDescription(_description: String): Schema[T] = new Schema.Delegating[T](this, required, nullable) {
    val default: Option[T]          = Schema.this.default
    val description: Option[String] = Some(_description)
  }

  def withDefault(_default: T): Schema[T] = new Schema.Delegating[T](this, required, nullable) {
    val default: Option[T]          = Some(_default)
    val description: Option[String] = Schema.this.description
  }

  override def toString: String = Schema.render(this, Nil)
}

case class PrimitiveSchema[T](
    className: String,
    `type`: SchemaType,
    format: Option[String]
) extends Schema[T] {
  val properties: Map[String, Schema[?]] = Map.empty
  val items: Option[Schema[?]]           = None
  val `enum`: Option[Set[String]]        = None
  val required: Boolean                  = true
  val additionalProperties: Boolean      = false
  val default: Option[T]                 = None
  val description: Option[String]        = None
}

case class FreeFormSchema[T](className: String) extends Schema[T] {
  val `type`: SchemaType                 = SchemaType.ObjectType
  val format: Option[String]             = None
  val properties: Map[String, Schema[?]] = Map.empty
  val items: Option[Schema[?]]           = None
  val `enum`: Option[Set[String]]        = None
  val required: Boolean                  = true
  val additionalProperties: Boolean      = true
  val default: Option[T]                 = None
  val description: Option[String]        = None
}

trait SchemaDefaults {
  implicit val emptyBodySchema: Schema[EmptyBody] = new Schema[EmptyBody] {
    val className: String                  = "EmptyBody"
    val `type`: SchemaType                 = SchemaType.NullType
    val format: Option[String]             = None
    val properties: Map[String, Schema[?]] = Map.empty
    val items: Option[Schema[?]]           = None
    val `enum`: Option[Set[String]]        = None
    val required: Boolean                  = false
    val additionalProperties: Boolean      = false
    val default: Option[EmptyBody]         = None
    val description: Option[String]        = None
  }
  implicit val intSchema: PrimitiveSchema[Int]               = PrimitiveSchema[Int]("Int", SchemaType.IntegerType, Some("int32"))
  implicit val longSchema: PrimitiveSchema[Long]             = PrimitiveSchema[Long]("Long", SchemaType.IntegerType, Some("int64"))
  implicit val floatSchema: PrimitiveSchema[Float]           = PrimitiveSchema[Float]("Float", SchemaType.NumberType, Some("float"))
  implicit val doubleSchema: PrimitiveSchema[Double]         = PrimitiveSchema[Double]("Double", SchemaType.NumberType, Some("double"))
  implicit val stringSchema: PrimitiveSchema[String]         = PrimitiveSchema[String]("String", SchemaType.StringType, None)
  implicit val byteSchema: PrimitiveSchema[Byte]             = PrimitiveSchema[Byte]("Byte", SchemaType.IntegerType, None)
  implicit val charSchema: PrimitiveSchema[Char]             = PrimitiveSchema[Char]("Char", SchemaType.StringType, None)
  implicit val shortSchema: PrimitiveSchema[Short]           = PrimitiveSchema[Short]("Short", SchemaType.IntegerType, None)
  implicit val booleanSchema: PrimitiveSchema[Boolean]       = PrimitiveSchema[Boolean]("Boolean", SchemaType.BooleanType, None)
  implicit val nullSchema: PrimitiveSchema[Null]             = PrimitiveSchema[Null]("Null", SchemaType.NullType, None)
  implicit val unitSchema: PrimitiveSchema[Unit]             = PrimitiveSchema[Unit]("Unit", SchemaType.NullType, None)
  implicit val bigDecimalSchema: PrimitiveSchema[BigDecimal] = PrimitiveSchema[BigDecimal]("BigDecimal", SchemaType.NumberType, None)
  implicit def optionSchema[T](implicit schema: Schema[T]): Schema[Option[T]] =
    new Schema.Delegating[Option[T]](schema, required = false, nullable = true) {
      val default: Option[Option[T]]  = Some(None)
      val description: Option[String] = schema.description
    }

  implicit def eitherSchema[L, R](implicit left: Schema[L], right: Schema[R]): Schema[Either[L, R]] =
    Schema.oneOfSchema[Either[L, R]](
      s"Either[${left.className}, ${right.className}]",
      Seq(left, right),
      `type` = if (left.`type` == right.`type`) left.`type` else SchemaType.ObjectType
    )

  def eitherWrappedSchema[L, R](leftKey: String = "Left", rightKey: String = "Right")(implicit
      left: Schema[L],
      right: Schema[R]
  ): Schema[Either[L, R]] =
    Schema.oneOfSchema[Either[L, R]](
      s"Either[${left.className}, ${right.className}]",
      Seq(Schema.wrapperObject(leftKey, left), Schema.wrapperObject(rightKey, right))
    )
  implicit def seqSchema[T](implicit schema: Schema[T]): Schema[Seq[T]] = new Schema[Seq[T]] {
    val className: String                  = "Seq[" + schema.className + "]"
    val `type`: SchemaType                 = SchemaType.ArrayType
    val format: Option[String]             = None
    val properties: Map[String, Schema[?]] = Map.empty
    val items: Option[Schema[?]]           = Some(schema)
    val `enum`: Option[Set[String]]        = None
    val required: Boolean                  = true
    val additionalProperties: Boolean      = false
    val default: Option[Seq[T]]            = None
    val description: Option[String]        = None
  }
  implicit def listSchema[T](implicit schema: Schema[T]): Schema[List[T]] = new Schema[List[T]] {
    val className: String                  = "List[" + schema.className + "]"
    val `type`: SchemaType                 = SchemaType.ArrayType
    val format: Option[String]             = None
    val properties: Map[String, Schema[?]] = Map.empty
    val items: Option[Schema[?]]           = Some(schema)
    val `enum`: Option[Set[String]]        = None
    val required: Boolean                  = true
    val additionalProperties: Boolean      = false
    val default: Option[List[T]]           = None
    val description: Option[String]        = None
  }
  // A JSON object whose keys are the (string-encoded) `K` values and whose values all conform to
  // `V` — renders as `type: object` + `additionalProperties: <schema for V>`. The key constraint
  // isn't expressible in OpenAPI 3.0, so it's dropped (JSON object keys are strings regardless).
  implicit def mapSchema[K, V](implicit keySchema: Schema[K], valueSchema: Schema[V]): Schema[Map[K, V]] = new Schema[Map[K, V]] {
    val className: String                                      = s"Map[${keySchema.className}, ${valueSchema.className}]"
    val `type`: SchemaType                                     = SchemaType.ObjectType
    val format: Option[String]                                 = None
    val properties: Map[String, Schema[?]]                     = Map.empty
    val items: Option[Schema[?]]                               = None
    val `enum`: Option[Set[String]]                            = None
    val required: Boolean                                      = true
    val additionalProperties: Boolean                          = true
    override val additionalPropertiesSchema: Option[Schema[?]] = Some(valueSchema)
    val default: Option[Map[K, V]]                             = None
    val description: Option[String]                            = None
  }
  implicit val uuidSchema: PrimitiveSchema[java.util.UUID] = PrimitiveSchema[java.util.UUID]("UUID", SchemaType.StringType, Some("uuid"))

  implicit val byteArraySchema: PrimitiveSchema[Array[Byte]] =
    PrimitiveSchema[Array[Byte]]("Array[Byte]", SchemaType.StringType, Some("binary"))

  implicit def setSchema[T](implicit schema: Schema[T]): Schema[Set[T]] = new Schema[Set[T]] {
    val className: String                  = "Set[" + schema.className + "]"
    val `type`: SchemaType                 = SchemaType.ArrayType
    val format: Option[String]             = None
    val properties: Map[String, Schema[?]] = Map.empty
    val items: Option[Schema[?]]           = Some(schema)
    val `enum`: Option[Set[String]]        = None
    val required: Boolean                  = true
    val additionalProperties: Boolean      = false
    val default: Option[Set[T]]            = None
    val description: Option[String]        = None
  }

  implicit def vectorSchema[T](implicit schema: Schema[T]): Schema[Vector[T]] = new Schema[Vector[T]] {
    val className: String                  = "Vector[" + schema.className + "]"
    val `type`: SchemaType                 = SchemaType.ArrayType
    val format: Option[String]             = None
    val properties: Map[String, Schema[?]] = Map.empty
    val items: Option[Schema[?]]           = Some(schema)
    val `enum`: Option[Set[String]]        = None
    val required: Boolean                  = true
    val additionalProperties: Boolean      = false
    val default: Option[Vector[T]]         = None
    val description: Option[String]        = None
  }

  implicit def arraySchema[T](implicit schema: Schema[T]): Schema[Array[T]] = new Schema[Array[T]] {
    val className: String                  = "Array[" + schema.className + "]"
    val `type`: SchemaType                 = SchemaType.ArrayType
    val format: Option[String]             = None
    val properties: Map[String, Schema[?]] = Map.empty
    val items: Option[Schema[?]]           = Some(schema)
    val `enum`: Option[Set[String]]        = None
    val required: Boolean                  = true
    val additionalProperties: Boolean      = false
    val default: Option[Array[T]]          = None
    val description: Option[String]        = None
  }

}

object Schema extends SchemaDerivation with SchemaDefaults {
  // every SchemaDefaults mixin carries its own emptyBodySchema instance, so identity checks miss
  def isEmptyBody(schema: Schema[?]): Boolean = schema.`type` == SchemaType.NullType && schema.className == "EmptyBody"

  abstract class Delegating[T](underlying: Schema[?], val required: Boolean, override val nullable: Boolean) extends Schema[T] {
    val className: String                                      = underlying.className
    val `type`: SchemaType                                     = underlying.`type`
    val format: Option[String]                                 = underlying.format
    def properties: Map[String, Schema[?]]                     = underlying.properties
    val items: Option[Schema[?]]                               = underlying.items
    val `enum`: Option[Set[String]]                            = underlying.`enum`
    val additionalProperties: Boolean                          = underlying.additionalProperties
    override def additionalPropertiesSchema: Option[Schema[?]] = underlying.additionalPropertiesSchema
    override def oneOf: Seq[Schema[?]]                         = underlying.oneOf
    override def discriminator: Option[SchemaDiscriminator]    = underlying.discriminator
    override def recursionKey: AnyRef                          = underlying.recursionKey
  }

  def oneOfSchema[T](
      className: String,
      alternatives: => Seq[Schema[?]],
      discriminator: Option[SchemaDiscriminator] = None,
      `type`: SchemaType = SchemaType.ObjectType
  ): Schema[T] = {
    val _className     = className
    val _discriminator = discriminator
    val _type          = `type`
    new Schema[T] {
      val className: String                                   = _className
      val `type`: SchemaType                                  = _type
      val format: Option[String]                              = None
      val properties: Map[String, Schema[?]]                  = Map.empty
      val items: Option[Schema[?]]                            = None
      val `enum`: Option[Set[String]]                         = None
      val required: Boolean                                   = true
      val additionalProperties: Boolean                       = false
      val default: Option[T]                                  = None
      val description: Option[String]                         = None
      override lazy val oneOf: Seq[Schema[?]]                 = alternatives
      override val discriminator: Option[SchemaDiscriminator] = _discriminator
    }
  }

  def wrapperObject(key: String, schema: => Schema[?]): Schema[Any] =
    new Schema[Any] {
      val className: String                       = ""
      val `type`: SchemaType                      = SchemaType.ObjectType
      val format: Option[String]                  = None
      lazy val properties: Map[String, Schema[?]] = Map(key -> schema)
      val items: Option[Schema[?]]                = None
      val `enum`: Option[Set[String]]             = None
      val required: Boolean                       = true
      val additionalProperties: Boolean           = false
      val default: Option[Any]                    = None
      val description: Option[String]             = None
    }

  private def render(schema: Schema[?], ancestors: List[AnyRef]): String =
    if (ancestors.exists(_ eq schema.recursionKey)) s"ref(${schema.className})"
    else {
      val inner = schema.recursionKey :: ancestors
      if (schema.oneOf.nonEmpty) s"oneOf(${schema.oneOf.map(render(_, inner)).mkString(", ")})"
      else
        schema.`type` match {
          case _ if schema.`enum`.isDefined => s"enum(${schema.`enum`.get.mkString(", ")})"
          case SchemaType.NullType          => "null"
          case SchemaType.StringType        => "string"
          case SchemaType.BooleanType       => "boolean"
          case SchemaType.IntegerType       => "integer"
          case SchemaType.NumberType        => "number"
          case SchemaType.ArrayType         => s"array(${schema.items.fold("unknown")(render(_, inner))})"
          case SchemaType.ObjectType        =>
            val props = schema.properties.map { case (k, v) => k + (if (v.required) "*" else "") + ": " + render(v, inner) }
            s"object(${props.mkString(", ")})"
        }
    }
}
