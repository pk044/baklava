package pl.iterators.baklava

import java.net.{URLDecoder, URLEncoder}
import java.nio.charset.StandardCharsets
import java.util.regex.Pattern

trait ToPathParam[T] {
  def apply(t: T): String

  def values(t: T): Seq[String] = Seq(apply(t))
}

sealed abstract class PathStyle(val style: String, val explode: Boolean) extends Product with Serializable {

  def prefix(name: String): String

  def separator(name: String): String

  private[baklava] def serialize(name: String, values: Seq[String]): String = {
    val encodedName = PathStyle.encode(name)
    if (values.isEmpty) "" else prefix(encodedName) + values.map(PathStyle.encode).mkString(separator(encodedName))
  }

  private[baklava] def deserialize(name: String, segment: String, collection: Boolean): Seq[String] = {
    val encodedName = PathStyle.encode(name)
    val unprefixed  = segment.stripPrefix(prefix(encodedName))
    val raw         =
      if (!collection) Seq(unprefixed)
      else if (segment.isEmpty) Seq.empty
      else unprefixed.split(Pattern.quote(separator(encodedName)), -1).toSeq
    raw.map(PathStyle.decode)
  }
}

object PathStyle {
  case object Simple extends PathStyle("simple", explode = false) {
    def prefix(name: String): String    = ""
    def separator(name: String): String = ","
  }

  case object Label extends PathStyle("label", explode = false) {
    def prefix(name: String): String    = "."
    def separator(name: String): String = ","
  }

  case object LabelExploded extends PathStyle("label", explode = true) {
    def prefix(name: String): String    = "."
    def separator(name: String): String = "."
  }

  case object Matrix extends PathStyle("matrix", explode = false) {
    def prefix(name: String): String    = s";$name="
    def separator(name: String): String = ","
  }

  case object MatrixExploded extends PathStyle("matrix", explode = true) {
    def prefix(name: String): String    = s";$name="
    def separator(name: String): String = s";$name="
  }

  val values: Seq[PathStyle] = Seq(Simple, Label, LabelExploded, Matrix, MatrixExploded)

  private[baklava] def encode(value: String): String = URLEncoder.encode(value, StandardCharsets.UTF_8.toString)
  private[baklava] def decode(value: String): String = URLDecoder.decode(value, StandardCharsets.UTF_8.toString)
}

case class PathParam[T](name: String, description: Option[String], style: PathStyle = PathStyle.Simple)(implicit
    val tsm: ToPathParam[T],
    val schema: Schema[T]
) {
  type Underlying = T

  private[baklava] def resolve(uri: String, value: T): String = uri.replace(s"{$name}", style.serialize(name, tsm.values(value)))
}

trait ToPathParamSeq[T] {
  def apply(t: T): Seq[PathParam[?]]
}

trait ProvidePathParams[T, U] {
  def apply(
      pathParams: T,
      pathParamsProvided: U,
      uri: String
  ): String
}

trait BaklavaPathParams {
  def p[T](name: String, description: String = "", style: PathStyle = PathStyle.Simple)(implicit
      tsm: ToPathParam[T],
      schema: Schema[T]
  ): PathParam[T] =
    PathParam[T](name, if (description.trim.isEmpty) None else Some(description.trim), style)

  implicit val stringToPathParam: ToPathParam[String] = new ToPathParam[String] {
    override def apply(t: String): String = t
  }

  implicit val intToPathParam: ToPathParam[Int] = new ToPathParam[Int] {
    override def apply(t: Int): String = t.toString
  }

  implicit val longToPathParam: ToPathParam[Long] = new ToPathParam[Long] {
    override def apply(t: Long): String = t.toString
  }

  implicit val doubleToPathParam: ToPathParam[Double] = new ToPathParam[Double] {
    override def apply(t: Double): String = t.toString
  }

  implicit val floatToPathParam: ToPathParam[Float] = new ToPathParam[Float] {
    override def apply(t: Float): String = t.toString
  }

  implicit val booleanToPathParam: ToPathParam[Boolean] = new ToPathParam[Boolean] {
    override def apply(t: Boolean): String = t.toString
  }

  implicit val byteToPathParam: ToPathParam[Byte] = new ToPathParam[Byte] {
    override def apply(t: Byte): String = t.toString
  }

  implicit val shortToPathParam: ToPathParam[Short] = new ToPathParam[Short] {
    override def apply(t: Short): String = t.toString
  }

  implicit val charToPathParam: ToPathParam[Char] = new ToPathParam[Char] {
    override def apply(t: Char): String = t.toString
  }

  implicit val bigDecimalToPathParam: ToPathParam[BigDecimal] = new ToPathParam[BigDecimal] {
    override def apply(t: BigDecimal): String = t.toString
  }

  implicit val uuidToPathParam: ToPathParam[java.util.UUID] = new ToPathParam[java.util.UUID] {
    override def apply(t: java.util.UUID): String = t.toString
  }

  implicit def seqToPathParam[T](implicit tsm: ToPathParam[T]): ToPathParam[Seq[T]] = new ToPathParam[Seq[T]] {
    override def apply(t: Seq[T]): String       = values(t).mkString(",")
    override def values(t: Seq[T]): Seq[String] = t.flatMap(tsm.values)
  }

  implicit def listToPathParam[T](implicit tsm: ToPathParam[T]): ToPathParam[List[T]] = new ToPathParam[List[T]] {
    override def apply(t: List[T]): String       = values(t).mkString(",")
    override def values(t: List[T]): Seq[String] = t.flatMap(tsm.values)
  }

  implicit def vectorToPathParam[T](implicit tsm: ToPathParam[T]): ToPathParam[Vector[T]] = new ToPathParam[Vector[T]] {
    override def apply(t: Vector[T]): String       = values(t).mkString(",")
    override def values(t: Vector[T]): Seq[String] = t.flatMap(tsm.values)
  }

  implicit def setToPathParam[T](implicit tsm: ToPathParam[T]): ToPathParam[Set[T]] = new ToPathParam[Set[T]] {
    override def apply(t: Set[T]): String       = values(t).mkString(",")
    override def values(t: Set[T]): Seq[String] = t.toSeq.flatMap(tsm.values).sorted
  }

  implicit def arrayToPathParam[T](implicit tsm: ToPathParam[T]): ToPathParam[Array[T]] = new ToPathParam[Array[T]] {
    override def apply(t: Array[T]): String       = values(t).mkString(",")
    override def values(t: Array[T]): Seq[String] = t.toSeq.flatMap(tsm.values)
  }

  implicit val unitToPathParamSeq: ToPathParamSeq[Unit] = (_: Unit) => Seq.empty

  implicit def singleValueToPathParamSeq[T]: ToPathParamSeq[PathParam[T]] = (t: PathParam[T]) => Seq(t)

  implicit def tuple2ToPathParamSeq[T1, T2]: ToPathParamSeq[(PathParam[T1], PathParam[T2])] =
    (t: (PathParam[T1], PathParam[T2])) => Seq(t._1, t._2)

  implicit def tuple3ToPathParamSeq[T1, T2, T3]: ToPathParamSeq[(PathParam[T1], PathParam[T2], PathParam[T3])] =
    (t: (PathParam[T1], PathParam[T2], PathParam[T3])) => Seq(t._1, t._2, t._3)

  implicit def tuple4ToPathParamSeq[T1, T2, T3, T4]: ToPathParamSeq[(PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4])] =
    (t: (PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4])) => Seq(t._1, t._2, t._3, t._4)

  implicit def tuple5ToPathParamSeq[T1, T2, T3, T4, T5]
      : ToPathParamSeq[(PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5])] =
    t => Seq(t._1, t._2, t._3, t._4, t._5)

  implicit def tuple6ToPathParamSeq[T1, T2, T3, T4, T5, T6]
      : ToPathParamSeq[(PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5], PathParam[T6])] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6)

  implicit def tuple7ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7]
      : ToPathParamSeq[(PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5], PathParam[T6], PathParam[T7])] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7)

  implicit def tuple8ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8]: ToPathParamSeq[
    (PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5], PathParam[T6], PathParam[T7], PathParam[T8])
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)

  implicit def tuple9ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8, T9]: ToPathParamSeq[
    (PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5], PathParam[T6], PathParam[T7], PathParam[T8], PathParam[T9])
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)

  implicit def tuple10ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]: ToPathParamSeq[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)

  implicit def tuple11ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]: ToPathParamSeq[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)

  implicit def tuple12ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]: ToPathParamSeq[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)

  implicit def tuple13ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]: ToPathParamSeq[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)

  implicit def tuple14ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]: ToPathParamSeq[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)

  implicit def tuple15ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]: ToPathParamSeq[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14],
        PathParam[T15]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)

  implicit def tuple16ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]: ToPathParamSeq[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14],
        PathParam[T15],
        PathParam[T16]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)

  implicit def tuple17ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]: ToPathParamSeq[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14],
        PathParam[T15],
        PathParam[T16],
        PathParam[T17]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)

  implicit def tuple18ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]: ToPathParamSeq[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14],
        PathParam[T15],
        PathParam[T16],
        PathParam[T17],
        PathParam[T18]
    )
  ] = t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)

  implicit def tuple19ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]: ToPathParamSeq[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14],
        PathParam[T15],
        PathParam[T16],
        PathParam[T17],
        PathParam[T18],
        PathParam[T19]
    )
  ] = t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)

  implicit def tuple20ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
      : ToPathParamSeq[
        (
            PathParam[T1],
            PathParam[T2],
            PathParam[T3],
            PathParam[T4],
            PathParam[T5],
            PathParam[T6],
            PathParam[T7],
            PathParam[T8],
            PathParam[T9],
            PathParam[T10],
            PathParam[T11],
            PathParam[T12],
            PathParam[T13],
            PathParam[T14],
            PathParam[T15],
            PathParam[T16],
            PathParam[T17],
            PathParam[T18],
            PathParam[T19],
            PathParam[T20]
        )
      ] = t =>
    Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)

  implicit def tuple21ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]
      : ToPathParamSeq[
        (
            PathParam[T1],
            PathParam[T2],
            PathParam[T3],
            PathParam[T4],
            PathParam[T5],
            PathParam[T6],
            PathParam[T7],
            PathParam[T8],
            PathParam[T9],
            PathParam[T10],
            PathParam[T11],
            PathParam[T12],
            PathParam[T13],
            PathParam[T14],
            PathParam[T15],
            PathParam[T16],
            PathParam[T17],
            PathParam[T18],
            PathParam[T19],
            PathParam[T20],
            PathParam[T21]
        )
      ] = t =>
    Seq(
      t._1,
      t._2,
      t._3,
      t._4,
      t._5,
      t._6,
      t._7,
      t._8,
      t._9,
      t._10,
      t._11,
      t._12,
      t._13,
      t._14,
      t._15,
      t._16,
      t._17,
      t._18,
      t._19,
      t._20,
      t._21
    )

  implicit def tuple22ToPathParamSeq[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]
      : ToPathParamSeq[
        (
            PathParam[T1],
            PathParam[T2],
            PathParam[T3],
            PathParam[T4],
            PathParam[T5],
            PathParam[T6],
            PathParam[T7],
            PathParam[T8],
            PathParam[T9],
            PathParam[T10],
            PathParam[T11],
            PathParam[T12],
            PathParam[T13],
            PathParam[T14],
            PathParam[T15],
            PathParam[T16],
            PathParam[T17],
            PathParam[T18],
            PathParam[T19],
            PathParam[T20],
            PathParam[T21],
            PathParam[T22]
        )
      ] = t =>
    Seq(
      t._1,
      t._2,
      t._3,
      t._4,
      t._5,
      t._6,
      t._7,
      t._8,
      t._9,
      t._10,
      t._11,
      t._12,
      t._13,
      t._14,
      t._15,
      t._16,
      t._17,
      t._18,
      t._19,
      t._20,
      t._21,
      t._22
    )

  implicit val providePathParamsByUnit: ProvidePathParams[Unit, Unit] = new ProvidePathParams[Unit, Unit] {
    override def apply(
        pathParams: Unit,
        pathParamsProvided: Unit,
        uri: String
    ): String = uri
  }

  implicit def providePathParamsSingleValue[T, U <: T]: ProvidePathParams[PathParam[T], U] = new ProvidePathParams[PathParam[T], U] {
    override def apply(
        pathParams: PathParam[T],
        pathParamsProvided: U,
        uri: String
    ): String =
      pathParams.resolve(uri, pathParamsProvided)
  }

  implicit def providePathParams2[T1, T2, U1 <: T1, U2 <: T2]: ProvidePathParams[(PathParam[T1], PathParam[T2]), (U1, U2)] =
    new ProvidePathParams[(PathParam[T1], PathParam[T2]), (U1, U2)] {
      override def apply(
          pathParams: (PathParam[T1], PathParam[T2]),
          pathParamsProvided: (U1, U2),
          uri: String
      ): String = {
        val (param1, param2)       = pathParams
        val (provided1, provided2) = pathParamsProvided
        val uri1                   = param1.resolve(uri, provided1)
        param2.resolve(uri1, provided2)
      }
    }

  implicit def providePathParams3[T1, T2, T3, U1 <: T1, U2 <: T2, U3 <: T3]
      : ProvidePathParams[(PathParam[T1], PathParam[T2], PathParam[T3]), (U1, U2, U3)] =
    new ProvidePathParams[(PathParam[T1], PathParam[T2], PathParam[T3]), (U1, U2, U3)] {
      override def apply(
          pathParams: (PathParam[T1], PathParam[T2], PathParam[T3]),
          pathParamsProvided: (U1, U2, U3),
          uri: String
      ): String = {
        val (param1, param2, param3)          = pathParams
        val (provided1, provided2, provided3) = pathParamsProvided
        val uri1                              = param1.resolve(uri, provided1)
        val uri2                              = param2.resolve(uri1, provided2)
        param3.resolve(uri2, provided3)
      }
    }

  implicit def providePathParams4[T1, T2, T3, T4, U1 <: T1, U2 <: T2, U3 <: T3, U4 <: T4]
      : ProvidePathParams[(PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4]), (U1, U2, U3, U4)] =
    new ProvidePathParams[(PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4]), (U1, U2, U3, U4)] {
      override def apply(
          pathParams: (PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4]),
          pathParamsProvided: (U1, U2, U3, U4),
          uri: String
      ): String = {
        val (param1, param2, param3, param4)             = pathParams
        val (provided1, provided2, provided3, provided4) = pathParamsProvided
        val uri1                                         = param1.resolve(uri, provided1)
        val uri2                                         = param2.resolve(uri1, provided2)
        val uri3                                         = param3.resolve(uri2, provided3)
        param4.resolve(uri3, provided4)
      }
    }

  implicit def providePathParams5[T1, T2, T3, T4, T5, U1 <: T1, U2 <: T2, U3 <: T3, U4 <: T4, U5 <: T5]
      : ProvidePathParams[(PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5]), (U1, U2, U3, U4, U5)] =
    new ProvidePathParams[(PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5]), (U1, U2, U3, U4, U5)] {
      override def apply(
          pathParams: (PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5]),
          pathParamsProvided: (U1, U2, U3, U4, U5),
          uri: String
      ): String = {
        val (param1, param2, param3, param4, param5)                = pathParams
        val (provided1, provided2, provided3, provided4, provided5) = pathParamsProvided
        val uri1                                                    = param1.resolve(uri, provided1)
        val uri2                                                    = param2.resolve(uri1, provided2)
        val uri3                                                    = param3.resolve(uri2, provided3)
        val uri4                                                    = param4.resolve(uri3, provided4)
        param5.resolve(uri4, provided5)
      }
    }

  implicit def providePathParams6[T1, T2, T3, T4, T5, T6, U1 <: T1, U2 <: T2, U3 <: T3, U4 <: T4, U5 <: T5, U6 <: T6]: ProvidePathParams[
    (PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5], PathParam[T6]),
    (U1, U2, U3, U4, U5, U6)
  ] =
    new ProvidePathParams[
      (PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5], PathParam[T6]),
      (U1, U2, U3, U4, U5, U6)
    ] {
      override def apply(
          pathParams: (PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5], PathParam[T6]),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6),
          uri: String
      ): String = {
        val (param1, param2, param3, param4, param5, param6)                   = pathParams
        val (provided1, provided2, provided3, provided4, provided5, provided6) = pathParamsProvided
        val uri1                                                               = param1.resolve(uri, provided1)
        val uri2                                                               = param2.resolve(uri1, provided2)
        val uri3                                                               = param3.resolve(uri2, provided3)
        val uri4                                                               = param4.resolve(uri3, provided4)
        val uri5                                                               = param5.resolve(uri4, provided5)
        param6.resolve(uri5, provided6)
      }
    }

  implicit def providePathParams7[T1, T2, T3, T4, T5, T6, T7, U1 <: T1, U2 <: T2, U3 <: T3, U4 <: T4, U5 <: T5, U6 <: T6, U7 <: T7]
      : ProvidePathParams[
        (PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5], PathParam[T6], PathParam[T7]),
        (U1, U2, U3, U4, U5, U6, U7)
      ] =
    new ProvidePathParams[
      (PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5], PathParam[T6], PathParam[T7]),
      (U1, U2, U3, U4, U5, U6, U7)
    ] {
      override def apply(
          pathParams: (PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5], PathParam[T6], PathParam[T7]),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7),
          uri: String
      ): String = {
        val (param1, param2, param3, param4, param5, param6, param7)                      = pathParams
        val (provided1, provided2, provided3, provided4, provided5, provided6, provided7) = pathParamsProvided
        val uri1                                                                          = param1.resolve(uri, provided1)
        val uri2                                                                          = param2.resolve(uri1, provided2)
        val uri3                                                                          = param3.resolve(uri2, provided3)
        val uri4                                                                          = param4.resolve(uri3, provided4)
        val uri5                                                                          = param5.resolve(uri4, provided5)
        val uri6                                                                          = param6.resolve(uri5, provided6)
        param7.resolve(uri6, provided7)
      }
    }

  implicit def providePathParams8[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8
  ]: ProvidePathParams[
    (PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5], PathParam[T6], PathParam[T7], PathParam[T8]),
    (U1, U2, U3, U4, U5, U6, U7, U8)
  ] =
    new ProvidePathParams[
      (PathParam[T1], PathParam[T2], PathParam[T3], PathParam[T4], PathParam[T5], PathParam[T6], PathParam[T7], PathParam[T8]),
      (U1, U2, U3, U4, U5, U6, U7, U8)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8),
          uri: String
      ): String = {
        val (param1, param2, param3, param4, param5, param6, param7, param8)                         = pathParams
        val (provided1, provided2, provided3, provided4, provided5, provided6, provided7, provided8) = pathParamsProvided
        val uri1                                                                                     = param1.resolve(uri, provided1)
        val uri2                                                                                     = param2.resolve(uri1, provided2)
        val uri3                                                                                     = param3.resolve(uri2, provided3)
        val uri4                                                                                     = param4.resolve(uri3, provided4)
        val uri5                                                                                     = param5.resolve(uri4, provided5)
        val uri6                                                                                     = param6.resolve(uri5, provided6)
        val uri7                                                                                     = param7.resolve(uri6, provided7)
        param8.resolve(uri7, provided8)
      }
    }

  implicit def providePathParams9[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8,
      U9 <: T9
  ]: ProvidePathParams[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9)
  ] =
    new ProvidePathParams[
      (
          PathParam[T1],
          PathParam[T2],
          PathParam[T3],
          PathParam[T4],
          PathParam[T5],
          PathParam[T6],
          PathParam[T7],
          PathParam[T8],
          PathParam[T9]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8],
              PathParam[T9]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9),
          uri: String
      ): String = {
        val (param1, param2, param3, param4, param5, param6, param7, param8, param9)                            = pathParams
        val (provided1, provided2, provided3, provided4, provided5, provided6, provided7, provided8, provided9) = pathParamsProvided
        val uri1 = param1.resolve(uri, provided1)
        val uri2 = param2.resolve(uri1, provided2)
        val uri3 = param3.resolve(uri2, provided3)
        val uri4 = param4.resolve(uri3, provided4)
        val uri5 = param5.resolve(uri4, provided5)
        val uri6 = param6.resolve(uri5, provided6)
        val uri7 = param7.resolve(uri6, provided7)
        val uri8 = param8.resolve(uri7, provided8)
        param9.resolve(uri8, provided9)
      }
    }

  implicit def providePathParams10[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8,
      U9 <: T9,
      U10 <: T10
  ]: ProvidePathParams[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10)
  ] =
    new ProvidePathParams[
      (
          PathParam[T1],
          PathParam[T2],
          PathParam[T3],
          PathParam[T4],
          PathParam[T5],
          PathParam[T6],
          PathParam[T7],
          PathParam[T8],
          PathParam[T9],
          PathParam[T10]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8],
              PathParam[T9],
              PathParam[T10]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10),
          uri: String
      ): String = {
        val (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10)                               = pathParams
        val (provided1, provided2, provided3, provided4, provided5, provided6, provided7, provided8, provided9, provided10) =
          pathParamsProvided
        val uri1 = param1.resolve(uri, provided1)
        val uri2 = param2.resolve(uri1, provided2)
        val uri3 = param3.resolve(uri2, provided3)
        val uri4 = param4.resolve(uri3, provided4)
        val uri5 = param5.resolve(uri4, provided5)
        val uri6 = param6.resolve(uri5, provided6)
        val uri7 = param7.resolve(uri6, provided7)
        val uri8 = param8.resolve(uri7, provided8)
        val uri9 = param9.resolve(uri8, provided9)
        param10.resolve(uri9, provided10)
      }
    }

  implicit def providePathParams11[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8,
      U9 <: T9,
      U10 <: T10,
      U11 <: T11
  ]: ProvidePathParams[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11)
  ] =
    new ProvidePathParams[
      (
          PathParam[T1],
          PathParam[T2],
          PathParam[T3],
          PathParam[T4],
          PathParam[T5],
          PathParam[T6],
          PathParam[T7],
          PathParam[T8],
          PathParam[T9],
          PathParam[T10],
          PathParam[T11]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8],
              PathParam[T9],
              PathParam[T10],
              PathParam[T11]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11),
          uri: String
      ): String = {
        val (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) = pathParams
        val (provided1, provided2, provided3, provided4, provided5, provided6, provided7, provided8, provided9, provided10, provided11) =
          pathParamsProvided
        val uri1  = param1.resolve(uri, provided1)
        val uri2  = param2.resolve(uri1, provided2)
        val uri3  = param3.resolve(uri2, provided3)
        val uri4  = param4.resolve(uri3, provided4)
        val uri5  = param5.resolve(uri4, provided5)
        val uri6  = param6.resolve(uri5, provided6)
        val uri7  = param7.resolve(uri6, provided7)
        val uri8  = param8.resolve(uri7, provided8)
        val uri9  = param9.resolve(uri8, provided9)
        val uri10 = param10.resolve(uri9, provided10)
        param11.resolve(uri10, provided11)
      }
    }

  implicit def providePathParams12[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8,
      U9 <: T9,
      U10 <: T10,
      U11 <: T11,
      U12 <: T12
  ]: ProvidePathParams[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12)
  ] =
    new ProvidePathParams[
      (
          PathParam[T1],
          PathParam[T2],
          PathParam[T3],
          PathParam[T4],
          PathParam[T5],
          PathParam[T6],
          PathParam[T7],
          PathParam[T8],
          PathParam[T9],
          PathParam[T10],
          PathParam[T11],
          PathParam[T12]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8],
              PathParam[T9],
              PathParam[T10],
              PathParam[T11],
              PathParam[T12]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12),
          uri: String
      ): String = {
        val (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) = pathParams
        val (
          provided1,
          provided2,
          provided3,
          provided4,
          provided5,
          provided6,
          provided7,
          provided8,
          provided9,
          provided10,
          provided11,
          provided12
        )         = pathParamsProvided
        val uri1  = param1.resolve(uri, provided1)
        val uri2  = param2.resolve(uri1, provided2)
        val uri3  = param3.resolve(uri2, provided3)
        val uri4  = param4.resolve(uri3, provided4)
        val uri5  = param5.resolve(uri4, provided5)
        val uri6  = param6.resolve(uri5, provided6)
        val uri7  = param7.resolve(uri6, provided7)
        val uri8  = param8.resolve(uri7, provided8)
        val uri9  = param9.resolve(uri8, provided9)
        val uri10 = param10.resolve(uri9, provided10)
        val uri11 = param11.resolve(uri10, provided11)
        param12.resolve(uri11, provided12)
      }
    }

  implicit def providePathParams13[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8,
      U9 <: T9,
      U10 <: T10,
      U11 <: T11,
      U12 <: T12,
      U13 <: T13
  ]: ProvidePathParams[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13)
  ] =
    new ProvidePathParams[
      (
          PathParam[T1],
          PathParam[T2],
          PathParam[T3],
          PathParam[T4],
          PathParam[T5],
          PathParam[T6],
          PathParam[T7],
          PathParam[T8],
          PathParam[T9],
          PathParam[T10],
          PathParam[T11],
          PathParam[T12],
          PathParam[T13]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8],
              PathParam[T9],
              PathParam[T10],
              PathParam[T11],
              PathParam[T12],
              PathParam[T13]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13),
          uri: String
      ): String = {
        val (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) = pathParams
        val (
          provided1,
          provided2,
          provided3,
          provided4,
          provided5,
          provided6,
          provided7,
          provided8,
          provided9,
          provided10,
          provided11,
          provided12,
          provided13
        )         = pathParamsProvided
        val uri1  = param1.resolve(uri, provided1)
        val uri2  = param2.resolve(uri1, provided2)
        val uri3  = param3.resolve(uri2, provided3)
        val uri4  = param4.resolve(uri3, provided4)
        val uri5  = param5.resolve(uri4, provided5)
        val uri6  = param6.resolve(uri5, provided6)
        val uri7  = param7.resolve(uri6, provided7)
        val uri8  = param8.resolve(uri7, provided8)
        val uri9  = param9.resolve(uri8, provided9)
        val uri10 = param10.resolve(uri9, provided10)
        val uri11 = param11.resolve(uri10, provided11)
        val uri12 = param12.resolve(uri11, provided12)
        param13.resolve(uri12, provided13)
      }
    }

  implicit def providePathParams14[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8,
      U9 <: T9,
      U10 <: T10,
      U11 <: T11,
      U12 <: T12,
      U13 <: T13,
      U14 <: T14
  ]: ProvidePathParams[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14)
  ] =
    new ProvidePathParams[
      (
          PathParam[T1],
          PathParam[T2],
          PathParam[T3],
          PathParam[T4],
          PathParam[T5],
          PathParam[T6],
          PathParam[T7],
          PathParam[T8],
          PathParam[T9],
          PathParam[T10],
          PathParam[T11],
          PathParam[T12],
          PathParam[T13],
          PathParam[T14]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8],
              PathParam[T9],
              PathParam[T10],
              PathParam[T11],
              PathParam[T12],
              PathParam[T13],
              PathParam[T14]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14),
          uri: String
      ): String = {
        val (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) =
          pathParams
        val (
          provided1,
          provided2,
          provided3,
          provided4,
          provided5,
          provided6,
          provided7,
          provided8,
          provided9,
          provided10,
          provided11,
          provided12,
          provided13,
          provided14
        )         = pathParamsProvided
        val uri1  = param1.resolve(uri, provided1)
        val uri2  = param2.resolve(uri1, provided2)
        val uri3  = param3.resolve(uri2, provided3)
        val uri4  = param4.resolve(uri3, provided4)
        val uri5  = param5.resolve(uri4, provided5)
        val uri6  = param6.resolve(uri5, provided6)
        val uri7  = param7.resolve(uri6, provided7)
        val uri8  = param8.resolve(uri7, provided8)
        val uri9  = param9.resolve(uri8, provided9)
        val uri10 = param10.resolve(uri9, provided10)
        val uri11 = param11.resolve(uri10, provided11)
        val uri12 = param12.resolve(uri11, provided12)
        val uri13 = param13.resolve(uri12, provided13)
        param14.resolve(uri13, provided14)
      }
    }

  implicit def providePathParams15[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8,
      U9 <: T9,
      U10 <: T10,
      U11 <: T11,
      U12 <: T12,
      U13 <: T13,
      U14 <: T14,
      U15 <: T15
  ]: ProvidePathParams[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14],
        PathParam[T15]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15)
  ] =
    new ProvidePathParams[
      (
          PathParam[T1],
          PathParam[T2],
          PathParam[T3],
          PathParam[T4],
          PathParam[T5],
          PathParam[T6],
          PathParam[T7],
          PathParam[T8],
          PathParam[T9],
          PathParam[T10],
          PathParam[T11],
          PathParam[T12],
          PathParam[T13],
          PathParam[T14],
          PathParam[T15]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8],
              PathParam[T9],
              PathParam[T10],
              PathParam[T11],
              PathParam[T12],
              PathParam[T13],
              PathParam[T14],
              PathParam[T15]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15),
          uri: String
      ): String = {
        val (
          param1,
          param2,
          param3,
          param4,
          param5,
          param6,
          param7,
          param8,
          param9,
          param10,
          param11,
          param12,
          param13,
          param14,
          param15
        ) = pathParams
        val (
          provided1,
          provided2,
          provided3,
          provided4,
          provided5,
          provided6,
          provided7,
          provided8,
          provided9,
          provided10,
          provided11,
          provided12,
          provided13,
          provided14,
          provided15
        )         = pathParamsProvided
        val uri1  = param1.resolve(uri, provided1)
        val uri2  = param2.resolve(uri1, provided2)
        val uri3  = param3.resolve(uri2, provided3)
        val uri4  = param4.resolve(uri3, provided4)
        val uri5  = param5.resolve(uri4, provided5)
        val uri6  = param6.resolve(uri5, provided6)
        val uri7  = param7.resolve(uri6, provided7)
        val uri8  = param8.resolve(uri7, provided8)
        val uri9  = param9.resolve(uri8, provided9)
        val uri10 = param10.resolve(uri9, provided10)
        val uri11 = param11.resolve(uri10, provided11)
        val uri12 = param12.resolve(uri11, provided12)
        val uri13 = param13.resolve(uri12, provided13)
        val uri14 = param14.resolve(uri13, provided14)
        param15.resolve(uri14, provided15)
      }
    }

  implicit def providePathParams16[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8,
      U9 <: T9,
      U10 <: T10,
      U11 <: T11,
      U12 <: T12,
      U13 <: T13,
      U14 <: T14,
      U15 <: T15,
      U16 <: T16
  ]: ProvidePathParams[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14],
        PathParam[T15],
        PathParam[T16]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16)
  ] =
    new ProvidePathParams[
      (
          PathParam[T1],
          PathParam[T2],
          PathParam[T3],
          PathParam[T4],
          PathParam[T5],
          PathParam[T6],
          PathParam[T7],
          PathParam[T8],
          PathParam[T9],
          PathParam[T10],
          PathParam[T11],
          PathParam[T12],
          PathParam[T13],
          PathParam[T14],
          PathParam[T15],
          PathParam[T16]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8],
              PathParam[T9],
              PathParam[T10],
              PathParam[T11],
              PathParam[T12],
              PathParam[T13],
              PathParam[T14],
              PathParam[T15],
              PathParam[T16]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16),
          uri: String
      ): String = {
        val (
          param1,
          param2,
          param3,
          param4,
          param5,
          param6,
          param7,
          param8,
          param9,
          param10,
          param11,
          param12,
          param13,
          param14,
          param15,
          param16
        ) = pathParams
        val (
          provided1,
          provided2,
          provided3,
          provided4,
          provided5,
          provided6,
          provided7,
          provided8,
          provided9,
          provided10,
          provided11,
          provided12,
          provided13,
          provided14,
          provided15,
          provided16
        )         = pathParamsProvided
        val uri1  = param1.resolve(uri, provided1)
        val uri2  = param2.resolve(uri1, provided2)
        val uri3  = param3.resolve(uri2, provided3)
        val uri4  = param4.resolve(uri3, provided4)
        val uri5  = param5.resolve(uri4, provided5)
        val uri6  = param6.resolve(uri5, provided6)
        val uri7  = param7.resolve(uri6, provided7)
        val uri8  = param8.resolve(uri7, provided8)
        val uri9  = param9.resolve(uri8, provided9)
        val uri10 = param10.resolve(uri9, provided10)
        val uri11 = param11.resolve(uri10, provided11)
        val uri12 = param12.resolve(uri11, provided12)
        val uri13 = param13.resolve(uri12, provided13)
        val uri14 = param14.resolve(uri13, provided14)
        val uri15 = param15.resolve(uri14, provided15)
        param16.resolve(uri15, provided16)
      }
    }

  implicit def providePathParams17[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8,
      U9 <: T9,
      U10 <: T10,
      U11 <: T11,
      U12 <: T12,
      U13 <: T13,
      U14 <: T14,
      U15 <: T15,
      U16 <: T16,
      U17 <: T17
  ]: ProvidePathParams[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14],
        PathParam[T15],
        PathParam[T16],
        PathParam[T17]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17)
  ] =
    new ProvidePathParams[
      (
          PathParam[T1],
          PathParam[T2],
          PathParam[T3],
          PathParam[T4],
          PathParam[T5],
          PathParam[T6],
          PathParam[T7],
          PathParam[T8],
          PathParam[T9],
          PathParam[T10],
          PathParam[T11],
          PathParam[T12],
          PathParam[T13],
          PathParam[T14],
          PathParam[T15],
          PathParam[T16],
          PathParam[T17]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8],
              PathParam[T9],
              PathParam[T10],
              PathParam[T11],
              PathParam[T12],
              PathParam[T13],
              PathParam[T14],
              PathParam[T15],
              PathParam[T16],
              PathParam[T17]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17),
          uri: String
      ): String = {
        val (
          param1,
          param2,
          param3,
          param4,
          param5,
          param6,
          param7,
          param8,
          param9,
          param10,
          param11,
          param12,
          param13,
          param14,
          param15,
          param16,
          param17
        ) = pathParams
        val (
          provided1,
          provided2,
          provided3,
          provided4,
          provided5,
          provided6,
          provided7,
          provided8,
          provided9,
          provided10,
          provided11,
          provided12,
          provided13,
          provided14,
          provided15,
          provided16,
          provided17
        )         = pathParamsProvided
        val uri1  = param1.resolve(uri, provided1)
        val uri2  = param2.resolve(uri1, provided2)
        val uri3  = param3.resolve(uri2, provided3)
        val uri4  = param4.resolve(uri3, provided4)
        val uri5  = param5.resolve(uri4, provided5)
        val uri6  = param6.resolve(uri5, provided6)
        val uri7  = param7.resolve(uri6, provided7)
        val uri8  = param8.resolve(uri7, provided8)
        val uri9  = param9.resolve(uri8, provided9)
        val uri10 = param10.resolve(uri9, provided10)
        val uri11 = param11.resolve(uri10, provided11)
        val uri12 = param12.resolve(uri11, provided12)
        val uri13 = param13.resolve(uri12, provided13)
        val uri14 = param14.resolve(uri13, provided14)
        val uri15 = param15.resolve(uri14, provided15)
        val uri16 = param16.resolve(uri15, provided16)
        param17.resolve(uri16, provided17)
      }
    }

  implicit def providePathParams18[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8,
      U9 <: T9,
      U10 <: T10,
      U11 <: T11,
      U12 <: T12,
      U13 <: T13,
      U14 <: T14,
      U15 <: T15,
      U16 <: T16,
      U17 <: T17,
      U18 <: T18
  ]: ProvidePathParams[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14],
        PathParam[T15],
        PathParam[T16],
        PathParam[T17],
        PathParam[T18]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18)
  ] =
    new ProvidePathParams[
      (
          PathParam[T1],
          PathParam[T2],
          PathParam[T3],
          PathParam[T4],
          PathParam[T5],
          PathParam[T6],
          PathParam[T7],
          PathParam[T8],
          PathParam[T9],
          PathParam[T10],
          PathParam[T11],
          PathParam[T12],
          PathParam[T13],
          PathParam[T14],
          PathParam[T15],
          PathParam[T16],
          PathParam[T17],
          PathParam[T18]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8],
              PathParam[T9],
              PathParam[T10],
              PathParam[T11],
              PathParam[T12],
              PathParam[T13],
              PathParam[T14],
              PathParam[T15],
              PathParam[T16],
              PathParam[T17],
              PathParam[T18]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18),
          uri: String
      ): String = {
        val (
          param1,
          param2,
          param3,
          param4,
          param5,
          param6,
          param7,
          param8,
          param9,
          param10,
          param11,
          param12,
          param13,
          param14,
          param15,
          param16,
          param17,
          param18
        ) = pathParams
        val (
          provided1,
          provided2,
          provided3,
          provided4,
          provided5,
          provided6,
          provided7,
          provided8,
          provided9,
          provided10,
          provided11,
          provided12,
          provided13,
          provided14,
          provided15,
          provided16,
          provided17,
          provided18
        )         = pathParamsProvided
        val uri1  = param1.resolve(uri, provided1)
        val uri2  = param2.resolve(uri1, provided2)
        val uri3  = param3.resolve(uri2, provided3)
        val uri4  = param4.resolve(uri3, provided4)
        val uri5  = param5.resolve(uri4, provided5)
        val uri6  = param6.resolve(uri5, provided6)
        val uri7  = param7.resolve(uri6, provided7)
        val uri8  = param8.resolve(uri7, provided8)
        val uri9  = param9.resolve(uri8, provided9)
        val uri10 = param10.resolve(uri9, provided10)
        val uri11 = param11.resolve(uri10, provided11)
        val uri12 = param12.resolve(uri11, provided12)
        val uri13 = param13.resolve(uri12, provided13)
        val uri14 = param14.resolve(uri13, provided14)
        val uri15 = param15.resolve(uri14, provided15)
        val uri16 = param16.resolve(uri15, provided16)
        val uri17 = param17.resolve(uri16, provided17)
        param18.resolve(uri17, provided18)
      }
    }

  implicit def providePathParams19[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8,
      U9 <: T9,
      U10 <: T10,
      U11 <: T11,
      U12 <: T12,
      U13 <: T13,
      U14 <: T14,
      U15 <: T15,
      U16 <: T16,
      U17 <: T17,
      U18 <: T18,
      U19 <: T19
  ]: ProvidePathParams[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14],
        PathParam[T15],
        PathParam[T16],
        PathParam[T17],
        PathParam[T18],
        PathParam[T19]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19)
  ] =
    new ProvidePathParams[
      (
          PathParam[T1],
          PathParam[T2],
          PathParam[T3],
          PathParam[T4],
          PathParam[T5],
          PathParam[T6],
          PathParam[T7],
          PathParam[T8],
          PathParam[T9],
          PathParam[T10],
          PathParam[T11],
          PathParam[T12],
          PathParam[T13],
          PathParam[T14],
          PathParam[T15],
          PathParam[T16],
          PathParam[T17],
          PathParam[T18],
          PathParam[T19]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8],
              PathParam[T9],
              PathParam[T10],
              PathParam[T11],
              PathParam[T12],
              PathParam[T13],
              PathParam[T14],
              PathParam[T15],
              PathParam[T16],
              PathParam[T17],
              PathParam[T18],
              PathParam[T19]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19),
          uri: String
      ): String = {
        val (
          param1,
          param2,
          param3,
          param4,
          param5,
          param6,
          param7,
          param8,
          param9,
          param10,
          param11,
          param12,
          param13,
          param14,
          param15,
          param16,
          param17,
          param18,
          param19
        ) = pathParams
        val (
          provided1,
          provided2,
          provided3,
          provided4,
          provided5,
          provided6,
          provided7,
          provided8,
          provided9,
          provided10,
          provided11,
          provided12,
          provided13,
          provided14,
          provided15,
          provided16,
          provided17,
          provided18,
          provided19
        )         = pathParamsProvided
        val uri1  = param1.resolve(uri, provided1)
        val uri2  = param2.resolve(uri1, provided2)
        val uri3  = param3.resolve(uri2, provided3)
        val uri4  = param4.resolve(uri3, provided4)
        val uri5  = param5.resolve(uri4, provided5)
        val uri6  = param6.resolve(uri5, provided6)
        val uri7  = param7.resolve(uri6, provided7)
        val uri8  = param8.resolve(uri7, provided8)
        val uri9  = param9.resolve(uri8, provided9)
        val uri10 = param10.resolve(uri9, provided10)
        val uri11 = param11.resolve(uri10, provided11)
        val uri12 = param12.resolve(uri11, provided12)
        val uri13 = param13.resolve(uri12, provided13)
        val uri14 = param14.resolve(uri13, provided14)
        val uri15 = param15.resolve(uri14, provided15)
        val uri16 = param16.resolve(uri15, provided16)
        val uri17 = param17.resolve(uri16, provided17)
        val uri18 = param18.resolve(uri17, provided18)
        param19.resolve(uri18, provided19)
      }
    }

  implicit def providePathParams20[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8,
      U9 <: T9,
      U10 <: T10,
      U11 <: T11,
      U12 <: T12,
      U13 <: T13,
      U14 <: T14,
      U15 <: T15,
      U16 <: T16,
      U17 <: T17,
      U18 <: T18,
      U19 <: T19,
      U20 <: T20
  ]: ProvidePathParams[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14],
        PathParam[T15],
        PathParam[T16],
        PathParam[T17],
        PathParam[T18],
        PathParam[T19],
        PathParam[T20]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20)
  ] =
    new ProvidePathParams[
      (
          PathParam[T1],
          PathParam[T2],
          PathParam[T3],
          PathParam[T4],
          PathParam[T5],
          PathParam[T6],
          PathParam[T7],
          PathParam[T8],
          PathParam[T9],
          PathParam[T10],
          PathParam[T11],
          PathParam[T12],
          PathParam[T13],
          PathParam[T14],
          PathParam[T15],
          PathParam[T16],
          PathParam[T17],
          PathParam[T18],
          PathParam[T19],
          PathParam[T20]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8],
              PathParam[T9],
              PathParam[T10],
              PathParam[T11],
              PathParam[T12],
              PathParam[T13],
              PathParam[T14],
              PathParam[T15],
              PathParam[T16],
              PathParam[T17],
              PathParam[T18],
              PathParam[T19],
              PathParam[T20]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20),
          uri: String
      ): String = {
        val (
          param1,
          param2,
          param3,
          param4,
          param5,
          param6,
          param7,
          param8,
          param9,
          param10,
          param11,
          param12,
          param13,
          param14,
          param15,
          param16,
          param17,
          param18,
          param19,
          param20
        ) = pathParams
        val (
          provided1,
          provided2,
          provided3,
          provided4,
          provided5,
          provided6,
          provided7,
          provided8,
          provided9,
          provided10,
          provided11,
          provided12,
          provided13,
          provided14,
          provided15,
          provided16,
          provided17,
          provided18,
          provided19,
          provided20
        )         = pathParamsProvided
        val uri1  = param1.resolve(uri, provided1)
        val uri2  = param2.resolve(uri1, provided2)
        val uri3  = param3.resolve(uri2, provided3)
        val uri4  = param4.resolve(uri3, provided4)
        val uri5  = param5.resolve(uri4, provided5)
        val uri6  = param6.resolve(uri5, provided6)
        val uri7  = param7.resolve(uri6, provided7)
        val uri8  = param8.resolve(uri7, provided8)
        val uri9  = param9.resolve(uri8, provided9)
        val uri10 = param10.resolve(uri9, provided10)
        val uri11 = param11.resolve(uri10, provided11)
        val uri12 = param12.resolve(uri11, provided12)
        val uri13 = param13.resolve(uri12, provided13)
        val uri14 = param14.resolve(uri13, provided14)
        val uri15 = param15.resolve(uri14, provided15)
        val uri16 = param16.resolve(uri15, provided16)
        val uri17 = param17.resolve(uri16, provided17)
        val uri18 = param18.resolve(uri17, provided18)
        val uri19 = param19.resolve(uri18, provided19)
        param20.resolve(uri19, provided20)
      }
    }

  implicit def providePathParams21[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20,
      T21,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8,
      U9 <: T9,
      U10 <: T10,
      U11 <: T11,
      U12 <: T12,
      U13 <: T13,
      U14 <: T14,
      U15 <: T15,
      U16 <: T16,
      U17 <: T17,
      U18 <: T18,
      U19 <: T19,
      U20 <: T20,
      U21 <: T21
  ]: ProvidePathParams[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14],
        PathParam[T15],
        PathParam[T16],
        PathParam[T17],
        PathParam[T18],
        PathParam[T19],
        PathParam[T20],
        PathParam[T21]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20, U21)
  ] =
    new ProvidePathParams[
      (
          PathParam[T1],
          PathParam[T2],
          PathParam[T3],
          PathParam[T4],
          PathParam[T5],
          PathParam[T6],
          PathParam[T7],
          PathParam[T8],
          PathParam[T9],
          PathParam[T10],
          PathParam[T11],
          PathParam[T12],
          PathParam[T13],
          PathParam[T14],
          PathParam[T15],
          PathParam[T16],
          PathParam[T17],
          PathParam[T18],
          PathParam[T19],
          PathParam[T20],
          PathParam[T21]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20, U21)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8],
              PathParam[T9],
              PathParam[T10],
              PathParam[T11],
              PathParam[T12],
              PathParam[T13],
              PathParam[T14],
              PathParam[T15],
              PathParam[T16],
              PathParam[T17],
              PathParam[T18],
              PathParam[T19],
              PathParam[T20],
              PathParam[T21]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20, U21),
          uri: String
      ): String = {
        val (
          param1,
          param2,
          param3,
          param4,
          param5,
          param6,
          param7,
          param8,
          param9,
          param10,
          param11,
          param12,
          param13,
          param14,
          param15,
          param16,
          param17,
          param18,
          param19,
          param20,
          param21
        ) = pathParams
        val (
          provided1,
          provided2,
          provided3,
          provided4,
          provided5,
          provided6,
          provided7,
          provided8,
          provided9,
          provided10,
          provided11,
          provided12,
          provided13,
          provided14,
          provided15,
          provided16,
          provided17,
          provided18,
          provided19,
          provided20,
          provided21
        )         = pathParamsProvided
        val uri1  = param1.resolve(uri, provided1)
        val uri2  = param2.resolve(uri1, provided2)
        val uri3  = param3.resolve(uri2, provided3)
        val uri4  = param4.resolve(uri3, provided4)
        val uri5  = param5.resolve(uri4, provided5)
        val uri6  = param6.resolve(uri5, provided6)
        val uri7  = param7.resolve(uri6, provided7)
        val uri8  = param8.resolve(uri7, provided8)
        val uri9  = param9.resolve(uri8, provided9)
        val uri10 = param10.resolve(uri9, provided10)
        val uri11 = param11.resolve(uri10, provided11)
        val uri12 = param12.resolve(uri11, provided12)
        val uri13 = param13.resolve(uri12, provided13)
        val uri14 = param14.resolve(uri13, provided14)
        val uri15 = param15.resolve(uri14, provided15)
        val uri16 = param16.resolve(uri15, provided16)
        val uri17 = param17.resolve(uri16, provided17)
        val uri18 = param18.resolve(uri17, provided18)
        val uri19 = param19.resolve(uri18, provided19)
        val uri20 = param20.resolve(uri19, provided20)
        param21.resolve(uri20, provided21)
      }
    }

  implicit def providePathParams22[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20,
      T21,
      T22,
      U1 <: T1,
      U2 <: T2,
      U3 <: T3,
      U4 <: T4,
      U5 <: T5,
      U6 <: T6,
      U7 <: T7,
      U8 <: T8,
      U9 <: T9,
      U10 <: T10,
      U11 <: T11,
      U12 <: T12,
      U13 <: T13,
      U14 <: T14,
      U15 <: T15,
      U16 <: T16,
      U17 <: T17,
      U18 <: T18,
      U19 <: T19,
      U20 <: T20,
      U21 <: T21,
      U22 <: T22
  ]: ProvidePathParams[
    (
        PathParam[T1],
        PathParam[T2],
        PathParam[T3],
        PathParam[T4],
        PathParam[T5],
        PathParam[T6],
        PathParam[T7],
        PathParam[T8],
        PathParam[T9],
        PathParam[T10],
        PathParam[T11],
        PathParam[T12],
        PathParam[T13],
        PathParam[T14],
        PathParam[T15],
        PathParam[T16],
        PathParam[T17],
        PathParam[T18],
        PathParam[T19],
        PathParam[T20],
        PathParam[T21],
        PathParam[T22]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20, U21, U22)
  ] =
    new ProvidePathParams[
      (
          PathParam[T1],
          PathParam[T2],
          PathParam[T3],
          PathParam[T4],
          PathParam[T5],
          PathParam[T6],
          PathParam[T7],
          PathParam[T8],
          PathParam[T9],
          PathParam[T10],
          PathParam[T11],
          PathParam[T12],
          PathParam[T13],
          PathParam[T14],
          PathParam[T15],
          PathParam[T16],
          PathParam[T17],
          PathParam[T18],
          PathParam[T19],
          PathParam[T20],
          PathParam[T21],
          PathParam[T22]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20, U21, U22)
    ] {
      override def apply(
          pathParams: (
              PathParam[T1],
              PathParam[T2],
              PathParam[T3],
              PathParam[T4],
              PathParam[T5],
              PathParam[T6],
              PathParam[T7],
              PathParam[T8],
              PathParam[T9],
              PathParam[T10],
              PathParam[T11],
              PathParam[T12],
              PathParam[T13],
              PathParam[T14],
              PathParam[T15],
              PathParam[T16],
              PathParam[T17],
              PathParam[T18],
              PathParam[T19],
              PathParam[T20],
              PathParam[T21],
              PathParam[T22]
          ),
          pathParamsProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20, U21, U22),
          uri: String
      ): String = {
        val (
          param1,
          param2,
          param3,
          param4,
          param5,
          param6,
          param7,
          param8,
          param9,
          param10,
          param11,
          param12,
          param13,
          param14,
          param15,
          param16,
          param17,
          param18,
          param19,
          param20,
          param21,
          param22
        ) = pathParams
        val (
          provided1,
          provided2,
          provided3,
          provided4,
          provided5,
          provided6,
          provided7,
          provided8,
          provided9,
          provided10,
          provided11,
          provided12,
          provided13,
          provided14,
          provided15,
          provided16,
          provided17,
          provided18,
          provided19,
          provided20,
          provided21,
          provided22
        )         = pathParamsProvided
        val uri1  = param1.resolve(uri, provided1)
        val uri2  = param2.resolve(uri1, provided2)
        val uri3  = param3.resolve(uri2, provided3)
        val uri4  = param4.resolve(uri3, provided4)
        val uri5  = param5.resolve(uri4, provided5)
        val uri6  = param6.resolve(uri5, provided6)
        val uri7  = param7.resolve(uri6, provided7)
        val uri8  = param8.resolve(uri7, provided8)
        val uri9  = param9.resolve(uri8, provided9)
        val uri10 = param10.resolve(uri9, provided10)
        val uri11 = param11.resolve(uri10, provided11)
        val uri12 = param12.resolve(uri11, provided12)
        val uri13 = param13.resolve(uri12, provided13)
        val uri14 = param14.resolve(uri13, provided14)
        val uri15 = param15.resolve(uri14, provided15)
        val uri16 = param16.resolve(uri15, provided16)
        val uri17 = param17.resolve(uri16, provided17)
        val uri18 = param18.resolve(uri17, provided18)
        val uri19 = param19.resolve(uri18, provided19)
        val uri20 = param20.resolve(uri19, provided20)
        val uri21 = param21.resolve(uri20, provided21)
        param22.resolve(uri21, provided22)
      }
    }
}
