package pl.iterators.baklava.openapi

import io.swagger.v3.oas.models.media.{ComposedSchema, Discriminator, Schema}
import pl.iterators.baklava.*

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

final private[openapi] class OpenApiSchemaConverter(refTargets: Set[String], jsonToJavaObject: io.circe.Json => Object) {

  private val registered     = mutable.LinkedHashMap.empty[String, Schema[?]]
  private val definitionName = mutable.Map.empty[String, String]
  private val taggedName     = mutable.Map.empty[(BaklavaSchemaSerializable, String, String), String]

  def components: Seq[(String, Schema[?])] = registered.toSeq.sortBy(_._1)

  def convert(baklavaSchema: BaklavaSchemaSerializable): Schema[?] =
    baklavaSchema.ref match {
      case Some(target)                        => referenceTo(definitionNameOf(target), baklavaSchema)
      case None if isDefinition(baklavaSchema) =>
        val name = definitionNameOf(baklavaSchema.className)
        if (!registered.contains(name)) {
          registered(name) = new Schema[String]()
          registered(name) = inlined(asDefinition(baklavaSchema))
        }
        referenceTo(name, baklavaSchema)
      case None => inlined(baklavaSchema)
    }

  private def isDefinition(baklavaSchema: BaklavaSchemaSerializable): Boolean =
    refTargets.contains(baklavaSchema.className) && (baklavaSchema.properties.nonEmpty || baklavaSchema.oneOf.nonEmpty)

  private def asDefinition(baklavaSchema: BaklavaSchemaSerializable): BaklavaSchemaSerializable =
    baklavaSchema.copy(required = true, nullable = false, description = None, default = None)

  private def definitionNameOf(className: String): String =
    definitionName.getOrElseUpdate(className, uniqueName(className))

  private def uniqueName(base: String): String = {
    val taken = registered.keySet ++ definitionName.values ++ taggedName.values
    if (!taken.contains(base)) base
    else Iterator.from(2).map(i => s"$base$i").find(n => !taken.contains(n)).get
  }

  private def refPath(name: String): String = s"#/components/schemas/$name"

  private def referenceTo(name: String, site: BaklavaSchemaSerializable): Schema[?] = {
    val ref = new Schema[String]().$ref(refPath(name))
    if (!site.nullable && site.description.isEmpty) ref
    else {
      val wrapper = new ComposedSchema().allOf(List[Schema[?]](ref).asJava)
      site.description.foreach(wrapper.setDescription)
      if (site.nullable) wrapper.setNullable(true)
      wrapper
    }
  }

  private def inlined(baklavaSchema: BaklavaSchemaSerializable): Schema[?] =
    if (baklavaSchema.oneOf.nonEmpty) oneOfSchema(baklavaSchema) else plainSchema(baklavaSchema)

  private def oneOfSchema(baklavaSchema: BaklavaSchemaSerializable): Schema[?] = {
    val composed     = new ComposedSchema()
    val alternatives = baklavaSchema.discriminator match {
      case Some(discriminator) =>
        val names = baklavaSchema.oneOf.zip(discriminator.values).map { case (alternative, tag) =>
          tag -> taggedAlternative(alternative, discriminator.propertyName, tag)
        }
        composed.discriminator(
          new Discriminator()
            .propertyName(discriminator.propertyName)
            .mapping(names.map { case (tag, name) => tag -> refPath(name) }.toMap.asJava)
        )
        names.map { case (_, name) => new Schema[String]().$ref(refPath(name)): Schema[?] }
      case None => baklavaSchema.oneOf.map(convert)
    }
    composed.oneOf(alternatives.asJava)
    if (baklavaSchema.className.nonEmpty && !baklavaSchema.className.contains("["))
      composed.addExtension("x-class", baklavaSchema.className)
    baklavaSchema.description.foreach(composed.setDescription)
    if (baklavaSchema.nullable) composed.setNullable(true)
    composed
  }

  private def taggedAlternative(alternative: BaklavaSchemaSerializable, propertyName: String, tag: String): String =
    taggedName.get((alternative, propertyName, tag)) match {
      case Some(name) => name
      case None       =>
        val name = uniqueName(if (alternative.className.nonEmpty) alternative.className else tag)
        taggedName((alternative, propertyName, tag)) = name
        val tagSchema = new Schema[String]()
        tagSchema.setType("string")
        tagSchema.setEnum(List(tag).asJava)
        val tagged =
          if (alternative.ref.isEmpty && alternative.oneOf.isEmpty && alternative.`type` == SchemaType.ObjectType) {
            val schema = plainSchema(asDefinition(alternative))
            schema.addProperty(propertyName, tagSchema)
            schema.setRequired((propertyName +: Option(schema.getRequired).map(_.asScala.toList).getOrElse(Nil)).asJava)
            schema
          } else {
            val tagObject = new Schema[String]()
            tagObject.setType("object")
            tagObject.addProperty(propertyName, tagSchema)
            tagObject.setRequired(List(propertyName).asJava)
            new ComposedSchema().allOf(List[Schema[?]](convert(asDefinition(alternative)), tagObject).asJava)
          }
        registered(name) = tagged
        name
    }

  private def plainSchema(baklavaSchema: BaklavaSchemaSerializable): Schema[?] = {
    val schema = new Schema[String]()
    // A named object type carries an `x-class` extension so post-processors can lift it into
    // components/schemas; a *pure* map (no declared properties, just `additionalProperties: <schema>`)
    // is structural rather than a named class, so it's left untagged. An object that has both
    // declared properties and a catch-all is still a named DTO and keeps its `x-class`.
    val isPureMap = baklavaSchema.additionalPropertiesSchema.isDefined && baklavaSchema.properties.isEmpty
    if (baklavaSchema.`type` == SchemaType.ObjectType && !isPureMap && baklavaSchema.className.nonEmpty) {
      schema.extensions(
        Map
          .apply[String, AnyRef](
            "x-class" -> baklavaSchema.className
          )
          .asJava
      )
      ()
    }
    baklavaSchema.`type` match {
      case SchemaType.NullType    => schema.setType("null")
      case SchemaType.StringType  => schema.setType("string")
      case SchemaType.BooleanType => schema.setType("boolean")
      case SchemaType.IntegerType => schema.setType("integer")
      case SchemaType.NumberType  => schema.setType("number")
      case SchemaType.ArrayType   => schema.setType("array")
      case SchemaType.ObjectType  => schema.setType("object")
    }
    baklavaSchema.description.foreach(schema.setDescription)
    baklavaSchema.format.foreach(schema.setFormat)
    if (baklavaSchema.nullable) schema.setNullable(true)
    baklavaSchema.items.foreach(bs => schema.setItems(convert(bs)))
    baklavaSchema.properties.foreach { case (name, bs) =>
      schema.addProperty(name, convert(bs))
    }
    // OAS 3.0: null must also be listed among enum values to pass validation
    baklavaSchema.`enum`.foreach { e =>
      val values = if (baklavaSchema.nullable) e.toList :+ (null: String) else e.toList
      schema.setEnum(values.asJava)
    }
    baklavaSchema.default.map(jsonToJavaObject).foreach(schema.setDefault)
    schema.setRequired(baklavaSchema.properties.toList.filter(_._2.required).map(_._1).asJava)
    baklavaSchema.additionalPropertiesSchema match {
      case Some(bs) => schema.setAdditionalProperties(convert(bs))
      case None     => if (baklavaSchema.additionalProperties) schema.setAdditionalProperties(baklavaSchema.additionalProperties)
    }

    schema
  }
}

private[openapi] object OpenApiSchemaConverter {
  def forCalls(calls: Seq[BaklavaSerializableCall], jsonToJavaObject: io.circe.Json => Object): OpenApiSchemaConverter = {
    def refs(schema: BaklavaSchemaSerializable): Seq[String] =
      schema.ref.toSeq ++ (schema.properties.values ++ schema.items ++ schema.additionalPropertiesSchema ++ schema.oneOf).flatMap(refs)
    val schemas = calls.flatMap { call =>
      call.request.bodySchema.toSeq ++ call.response.bodySchema ++ call.request.headersSeq.map(_.schema) ++
      call.request.pathParametersSeq.map(_.schema) ++ call.request.queryParametersSeq.map(_.schema) ++
      call.request.responseHeaders.map(_.schema)
    }
    new OpenApiSchemaConverter(schemas.flatMap(refs).toSet, jsonToJavaObject)
  }
}
