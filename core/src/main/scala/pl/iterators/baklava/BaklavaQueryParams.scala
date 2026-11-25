package pl.iterators.baklava

trait ToQueryParam[T] {
  def apply(t: T): Seq[String]
}

sealed abstract class CollectionStyle(
    val style: String,
    val explode: Boolean,
    val delimiter: Option[String],
    wireDelimiter: Option[String]
) extends Product
    with Serializable {

  private[baklava] def serialize(name: String, values: Seq[String]): Seq[String] = {
    val key     = CollectionStyle.encode(name)
    val encoded = values.map(CollectionStyle.encode)
    wireDelimiter match {
      case None                       => encoded.map(value => s"$key=$value")
      case Some(_) if encoded.isEmpty => Seq.empty
      case Some(separator)            => Seq(s"$key=${encoded.mkString(separator)}")
    }
  }

  private[baklava] def deserialize(rawValues: Seq[String]): Seq[String] =
    wireDelimiter
      .fold(rawValues)(separator => rawValues.flatMap(_.split(java.util.regex.Pattern.quote(separator), -1).toSeq))
      .map(CollectionStyle.decode)
}

object CollectionStyle {
  case object Exploded       extends CollectionStyle("form", explode = true, delimiter = None, wireDelimiter = None)
  case object CommaSeparated extends CollectionStyle("form", explode = false, delimiter = Some(","), wireDelimiter = Some(","))
  case object PipeDelimited  extends CollectionStyle("pipeDelimited", explode = false, delimiter = Some("|"), wireDelimiter = Some("%7C"))
  case object SpaceDelimited extends CollectionStyle("spaceDelimited", explode = false, delimiter = Some(" "), wireDelimiter = Some("%20"))

  val values: Seq[CollectionStyle] = Seq(Exploded, CommaSeparated, PipeDelimited, SpaceDelimited)

  private[baklava] def encode(value: String): String = java.net.URLEncoder.encode(value, "UTF-8")
  private[baklava] def decode(value: String): String = java.net.URLDecoder.decode(value, "UTF-8")
}

case class QueryParam[T](name: String, description: Option[String], collectionStyle: CollectionStyle = CollectionStyle.Exploded)(implicit
    val tsm: ToQueryParam[T],
    val schema: Schema[T]
) {
  type Underlying = T

  require(
    collectionStyle == CollectionStyle.Exploded || schema.`type` == SchemaType.ArrayType,
    s"Query parameter '$name' declares collectionStyle $collectionStyle, which applies only to collection parameters (Seq, List, Vector, Set, Array)"
  )

  private[baklava] def serialize(value: T): Seq[String] = collectionStyle.serialize(name, tsm(value))
}

trait ToQueryParamSeq[T] {
  def apply(t: T): Seq[QueryParam[?]]
}

trait ProvideQueryParams[T, U] {
  def apply(
      queryParameters: T,
      queryParametersProvided: U,
      uri: String
  ): String
}

trait BaklavaQueryParams {
  def q[T](name: String, description: String = "", collectionStyle: CollectionStyle = CollectionStyle.Exploded)(implicit
      tsm: ToQueryParam[T],
      schema: Schema[T]
  ): QueryParam[T] =
    QueryParam[T](name, if (description.trim.isEmpty) None else Some(description.trim), collectionStyle)

  implicit val stringToQueryParam: ToQueryParam[String] = new ToQueryParam[String] {
    override def apply(t: String): Seq[String] = Seq(t)
  }

  implicit val intToQueryParam: ToQueryParam[Int] = new ToQueryParam[Int] {
    override def apply(t: Int): Seq[String] = Seq(t.toString)
  }

  implicit val longToQueryParam: ToQueryParam[Long] = new ToQueryParam[Long] {
    override def apply(t: Long): Seq[String] = Seq(t.toString)
  }

  implicit val doubleToQueryParam: ToQueryParam[Double] = new ToQueryParam[Double] {
    override def apply(t: Double): Seq[String] = Seq(t.toString)
  }

  implicit val floatToQueryParam: ToQueryParam[Float] = new ToQueryParam[Float] {
    override def apply(t: Float): Seq[String] = Seq(t.toString)
  }

  implicit val booleanToQueryParam: ToQueryParam[Boolean] = new ToQueryParam[Boolean] {
    override def apply(t: Boolean): Seq[String] = Seq(t.toString)
  }

  implicit val byteToQueryParam: ToQueryParam[Byte] = new ToQueryParam[Byte] {
    override def apply(t: Byte): Seq[String] = Seq(t.toString)
  }

  implicit val shortToQueryParam: ToQueryParam[Short] = new ToQueryParam[Short] {
    override def apply(t: Short): Seq[String] = Seq(t.toString)
  }

  implicit val charToQueryParam: ToQueryParam[Char] = new ToQueryParam[Char] {
    override def apply(t: Char): Seq[String] = Seq(t.toString)
  }

  implicit val bigDecimalToQueryParam: ToQueryParam[BigDecimal] = new ToQueryParam[BigDecimal] {
    override def apply(t: BigDecimal): Seq[String] = Seq(t.toString)
  }

  implicit val uuidToQueryParam: ToQueryParam[java.util.UUID] = new ToQueryParam[java.util.UUID] {
    override def apply(t: java.util.UUID): Seq[String] = Seq(t.toString)
  }

  implicit def seqToQueryParam[T](implicit tsm: ToQueryParam[T]): ToQueryParam[Seq[T]] = new ToQueryParam[Seq[T]] {
    override def apply(t: Seq[T]): Seq[String] = t.flatMap(tsm.apply)
  }

  implicit def listToQueryParam[T](implicit tsm: ToQueryParam[T]): ToQueryParam[List[T]] = new ToQueryParam[List[T]] {
    override def apply(t: List[T]): Seq[String] = t.flatMap(tsm.apply)
  }

  implicit def vectorToQueryParam[T](implicit tsm: ToQueryParam[T]): ToQueryParam[Vector[T]] = new ToQueryParam[Vector[T]] {
    override def apply(t: Vector[T]): Seq[String] = t.flatMap(tsm.apply)
  }

  implicit def setToQueryParam[T](implicit tsm: ToQueryParam[T]): ToQueryParam[Set[T]] = new ToQueryParam[Set[T]] {
    // Set iteration order is undefined; sort the rendered string values so generated URLs are deterministic.
    override def apply(t: Set[T]): Seq[String] = t.toSeq.flatMap(tsm.apply).sorted
  }

  implicit def arrayToQueryParam[T](implicit tsm: ToQueryParam[T]): ToQueryParam[Array[T]] = new ToQueryParam[Array[T]] {
    override def apply(t: Array[T]): Seq[String] = t.toSeq.flatMap(tsm.apply)
  }

  implicit def optionToQueryParam[T](implicit tsm: ToQueryParam[T]): ToQueryParam[Option[T]] = new ToQueryParam[Option[T]] {
    override def apply(t: Option[T]): Seq[String] = t.map(tsm.apply).getOrElse(Seq.empty)
  }

  implicit val unitToQueryParamSeq: ToQueryParamSeq[Unit] = (_: Unit) => Seq.empty

  implicit def singleValueToQueryParamSeq[T]: ToQueryParamSeq[QueryParam[T]] = (t: QueryParam[T]) => Seq(t)

  implicit def tuple2ToQueryParamSeq[A, B]: ToQueryParamSeq[(QueryParam[A], QueryParam[B])] =
    (t: (QueryParam[A], QueryParam[B])) => Seq(t._1, t._2)

  implicit def tuple3ToQueryParamSeq[A, B, C]: ToQueryParamSeq[(QueryParam[A], QueryParam[B], QueryParam[C])] =
    (t: (QueryParam[A], QueryParam[B], QueryParam[C])) => Seq(t._1, t._2, t._3)

  implicit def tuple4ToQueryParamSeq[A, B, C, D]: ToQueryParamSeq[(QueryParam[A], QueryParam[B], QueryParam[C], QueryParam[D])] =
    (t: (QueryParam[A], QueryParam[B], QueryParam[C], QueryParam[D])) => Seq(t._1, t._2, t._3, t._4)

  implicit def tuple5ToQueryParamSeq[A, B, C, D, E]
      : ToQueryParamSeq[(QueryParam[A], QueryParam[B], QueryParam[C], QueryParam[D], QueryParam[E])] =
    t => Seq(t._1, t._2, t._3, t._4, t._5)

  implicit def tuple6ToQueryParamSeq[A, B, C, D, E, F]
      : ToQueryParamSeq[(QueryParam[A], QueryParam[B], QueryParam[C], QueryParam[D], QueryParam[E], QueryParam[F])] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6)

  implicit def tuple7ToQueryParamSeq[A, B, C, D, E, F, G]
      : ToQueryParamSeq[(QueryParam[A], QueryParam[B], QueryParam[C], QueryParam[D], QueryParam[E], QueryParam[F], QueryParam[G])] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7)

  implicit def tuple8ToQueryParamSeq[A, B, C, D, E, F, G, H]: ToQueryParamSeq[
    (QueryParam[A], QueryParam[B], QueryParam[C], QueryParam[D], QueryParam[E], QueryParam[F], QueryParam[G], QueryParam[H])
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)

  implicit def tuple9ToQueryParamSeq[A, B, C, D, E, F, G, H, I]: ToQueryParamSeq[
    (QueryParam[A], QueryParam[B], QueryParam[C], QueryParam[D], QueryParam[E], QueryParam[F], QueryParam[G], QueryParam[H], QueryParam[I])
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)

  implicit def tuple10ToQueryParamSeq[A, B, C, D, E, F, G, H, I, J]: ToQueryParamSeq[
    (
        QueryParam[A],
        QueryParam[B],
        QueryParam[C],
        QueryParam[D],
        QueryParam[E],
        QueryParam[F],
        QueryParam[G],
        QueryParam[H],
        QueryParam[I],
        QueryParam[J]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)

  implicit def tuple11ToQueryParamSeq[A, B, C, D, E, F, G, H, I, J, K]: ToQueryParamSeq[
    (
        QueryParam[A],
        QueryParam[B],
        QueryParam[C],
        QueryParam[D],
        QueryParam[E],
        QueryParam[F],
        QueryParam[G],
        QueryParam[H],
        QueryParam[I],
        QueryParam[J],
        QueryParam[K]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)

  implicit def tuple12ToQueryParamSeq[A, B, C, D, E, F, G, H, I, J, K, L]: ToQueryParamSeq[
    (
        QueryParam[A],
        QueryParam[B],
        QueryParam[C],
        QueryParam[D],
        QueryParam[E],
        QueryParam[F],
        QueryParam[G],
        QueryParam[H],
        QueryParam[I],
        QueryParam[J],
        QueryParam[K],
        QueryParam[L]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)

  implicit def tuple13ToQueryParamSeq[A, B, C, D, E, F, G, H, I, J, K, L, M]: ToQueryParamSeq[
    (
        QueryParam[A],
        QueryParam[B],
        QueryParam[C],
        QueryParam[D],
        QueryParam[E],
        QueryParam[F],
        QueryParam[G],
        QueryParam[H],
        QueryParam[I],
        QueryParam[J],
        QueryParam[K],
        QueryParam[L],
        QueryParam[M]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)

  implicit def tuple14ToQueryParamSeq[A, B, C, D, E, F, G, H, I, J, K, L, M, N]: ToQueryParamSeq[
    (
        QueryParam[A],
        QueryParam[B],
        QueryParam[C],
        QueryParam[D],
        QueryParam[E],
        QueryParam[F],
        QueryParam[G],
        QueryParam[H],
        QueryParam[I],
        QueryParam[J],
        QueryParam[K],
        QueryParam[L],
        QueryParam[M],
        QueryParam[N]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)

  implicit def tuple15ToQueryParamSeq[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]: ToQueryParamSeq[
    (
        QueryParam[A],
        QueryParam[B],
        QueryParam[C],
        QueryParam[D],
        QueryParam[E],
        QueryParam[F],
        QueryParam[G],
        QueryParam[H],
        QueryParam[I],
        QueryParam[J],
        QueryParam[K],
        QueryParam[L],
        QueryParam[M],
        QueryParam[N],
        QueryParam[O]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)

  implicit def tuple16ToQueryParamSeq[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]: ToQueryParamSeq[
    (
        QueryParam[A],
        QueryParam[B],
        QueryParam[C],
        QueryParam[D],
        QueryParam[E],
        QueryParam[F],
        QueryParam[G],
        QueryParam[H],
        QueryParam[I],
        QueryParam[J],
        QueryParam[K],
        QueryParam[L],
        QueryParam[M],
        QueryParam[N],
        QueryParam[O],
        QueryParam[P]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)

  implicit def tuple17ToQueryParamSeq[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]: ToQueryParamSeq[
    (
        QueryParam[A],
        QueryParam[B],
        QueryParam[C],
        QueryParam[D],
        QueryParam[E],
        QueryParam[F],
        QueryParam[G],
        QueryParam[H],
        QueryParam[I],
        QueryParam[J],
        QueryParam[K],
        QueryParam[L],
        QueryParam[M],
        QueryParam[N],
        QueryParam[O],
        QueryParam[P],
        QueryParam[Q]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)

  implicit def tuple18ToQueryParamSeq[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]: ToQueryParamSeq[
    (
        QueryParam[A],
        QueryParam[B],
        QueryParam[C],
        QueryParam[D],
        QueryParam[E],
        QueryParam[F],
        QueryParam[G],
        QueryParam[H],
        QueryParam[I],
        QueryParam[J],
        QueryParam[K],
        QueryParam[L],
        QueryParam[M],
        QueryParam[N],
        QueryParam[O],
        QueryParam[P],
        QueryParam[Q],
        QueryParam[R]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)

  implicit def tuple19ToQueryParamSeq[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]: ToQueryParamSeq[
    (
        QueryParam[A],
        QueryParam[B],
        QueryParam[C],
        QueryParam[D],
        QueryParam[E],
        QueryParam[F],
        QueryParam[G],
        QueryParam[H],
        QueryParam[I],
        QueryParam[J],
        QueryParam[K],
        QueryParam[L],
        QueryParam[M],
        QueryParam[N],
        QueryParam[O],
        QueryParam[P],
        QueryParam[Q],
        QueryParam[R],
        QueryParam[S]
    )
  ] =
    t => Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)

  implicit def tuple20ToQueryParamSeq[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]: ToQueryParamSeq[
    (
        QueryParam[A],
        QueryParam[B],
        QueryParam[C],
        QueryParam[D],
        QueryParam[E],
        QueryParam[F],
        QueryParam[G],
        QueryParam[H],
        QueryParam[I],
        QueryParam[J],
        QueryParam[K],
        QueryParam[L],
        QueryParam[M],
        QueryParam[N],
        QueryParam[O],
        QueryParam[P],
        QueryParam[Q],
        QueryParam[R],
        QueryParam[S],
        QueryParam[T]
    )
  ] =
    t =>
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
        t._20
      )

  implicit def tuple21ToQueryParamSeq[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]: ToQueryParamSeq[
    (
        QueryParam[A],
        QueryParam[B],
        QueryParam[C],
        QueryParam[D],
        QueryParam[E],
        QueryParam[F],
        QueryParam[G],
        QueryParam[H],
        QueryParam[I],
        QueryParam[J],
        QueryParam[K],
        QueryParam[L],
        QueryParam[M],
        QueryParam[N],
        QueryParam[O],
        QueryParam[P],
        QueryParam[Q],
        QueryParam[R],
        QueryParam[S],
        QueryParam[T],
        QueryParam[U]
    )
  ] =
    t =>
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

  implicit def tuple22ToQueryParamSeq[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]: ToQueryParamSeq[
    (
        QueryParam[A],
        QueryParam[B],
        QueryParam[C],
        QueryParam[D],
        QueryParam[E],
        QueryParam[F],
        QueryParam[G],
        QueryParam[H],
        QueryParam[I],
        QueryParam[J],
        QueryParam[K],
        QueryParam[L],
        QueryParam[M],
        QueryParam[N],
        QueryParam[O],
        QueryParam[P],
        QueryParam[Q],
        QueryParam[R],
        QueryParam[S],
        QueryParam[T],
        QueryParam[U],
        QueryParam[V]
    )
  ] =
    t =>
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

  implicit def provideQueryParamsByUnit[T]: ProvideQueryParams[T, Unit] = new ProvideQueryParams[T, Unit] {
    override def apply(
        queryParameters: T,
        queryParametersProvided: Unit,
        uri: String
    ): String = uri
  }

  implicit def provideQueryParamsSingleValue[T, U <: T]: ProvideQueryParams[QueryParam[T], U] = new ProvideQueryParams[QueryParam[T], U] {
    override def apply(
        queryParameters: QueryParam[T],
        queryParametersProvided: U,
        uri: String
    ): String = addSerializedQueryParametersToUri(uri, Seq(queryParameters.serialize(queryParametersProvided)))
  }

  implicit def provideQueryParams2[T1, T2, U1 <: T1, U2 <: T2]: ProvideQueryParams[(QueryParam[T1], QueryParam[T2]), (U1, U2)] =
    new ProvideQueryParams[(QueryParam[T1], QueryParam[T2]), (U1, U2)] {
      override def apply(
          queryParameters: (QueryParam[T1], QueryParam[T2]),
          queryParametersProvided: (U1, U2),
          uri: String
      ): String = {
        val (a, b) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b)
          )
        )
      }
    }

  implicit def provideQueryParams3[T1, T2, T3, U1 <: T1, U2 <: T2, U3 <: T3]
      : ProvideQueryParams[(QueryParam[T1], QueryParam[T2], QueryParam[T3]), (U1, U2, U3)] =
    new ProvideQueryParams[(QueryParam[T1], QueryParam[T2], QueryParam[T3]), (U1, U2, U3)] {
      override def apply(
          queryParameters: (QueryParam[T1], QueryParam[T2], QueryParam[T3]),
          queryParametersProvided: (U1, U2, U3),
          uri: String
      ): String = {
        val (a, b, c) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c)
          )
        )
      }
    }

  implicit def provideQueryParams4[T1, T2, T3, T4, U1 <: T1, U2 <: T2, U3 <: T3, U4 <: T4]
      : ProvideQueryParams[(QueryParam[T1], QueryParam[T2], QueryParam[T3], QueryParam[T4]), (U1, U2, U3, U4)] =
    new ProvideQueryParams[(QueryParam[T1], QueryParam[T2], QueryParam[T3], QueryParam[T4]), (U1, U2, U3, U4)] {
      override def apply(
          queryParameters: (QueryParam[T1], QueryParam[T2], QueryParam[T3], QueryParam[T4]),
          queryParametersProvided: (U1, U2, U3, U4),
          uri: String
      ): String = {
        val (a, b, c, d) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d)
          )
        )
      }
    }

  implicit def provideQueryParams5[T1, T2, T3, T4, T5, U1 <: T1, U2 <: T2, U3 <: T3, U4 <: T4, U5 <: T5]
      : ProvideQueryParams[(QueryParam[T1], QueryParam[T2], QueryParam[T3], QueryParam[T4], QueryParam[T5]), (U1, U2, U3, U4, U5)] =
    new ProvideQueryParams[(QueryParam[T1], QueryParam[T2], QueryParam[T3], QueryParam[T4], QueryParam[T5]), (U1, U2, U3, U4, U5)] {
      override def apply(
          queryParameters: (QueryParam[T1], QueryParam[T2], QueryParam[T3], QueryParam[T4], QueryParam[T5]),
          queryParametersProvided: (U1, U2, U3, U4, U5),
          uri: String
      ): String = {
        val (a, b, c, d, e) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e)
          )
        )
      }
    }

  implicit def provideQueryParams6[T1, T2, T3, T4, T5, T6, U1 <: T1, U2 <: T2, U3 <: T3, U4 <: T4, U5 <: T5, U6 <: T6]: ProvideQueryParams[
    (QueryParam[T1], QueryParam[T2], QueryParam[T3], QueryParam[T4], QueryParam[T5], QueryParam[T6]),
    (U1, U2, U3, U4, U5, U6)
  ] =
    new ProvideQueryParams[
      (QueryParam[T1], QueryParam[T2], QueryParam[T3], QueryParam[T4], QueryParam[T5], QueryParam[T6]),
      (U1, U2, U3, U4, U5, U6)
    ] {
      override def apply(
          queryParameters: (QueryParam[T1], QueryParam[T2], QueryParam[T3], QueryParam[T4], QueryParam[T5], QueryParam[T6]),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6),
          uri: String
      ): String = {
        val (a, b, c, d, e, f) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f)
          )
        )
      }
    }

  implicit def provideQueryParams7[T1, T2, T3, T4, T5, T6, T7, U1 <: T1, U2 <: T2, U3 <: T3, U4 <: T4, U5 <: T5, U6 <: T6, U7 <: T7]
      : ProvideQueryParams[
        (QueryParam[T1], QueryParam[T2], QueryParam[T3], QueryParam[T4], QueryParam[T5], QueryParam[T6], QueryParam[T7]),
        (U1, U2, U3, U4, U5, U6, U7)
      ] =
    new ProvideQueryParams[
      (QueryParam[T1], QueryParam[T2], QueryParam[T3], QueryParam[T4], QueryParam[T5], QueryParam[T6], QueryParam[T7]),
      (U1, U2, U3, U4, U5, U6, U7)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g)
          )
        )
      }
    }

  implicit def provideQueryParams8[
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
  ]: ProvideQueryParams[
    (QueryParam[T1], QueryParam[T2], QueryParam[T3], QueryParam[T4], QueryParam[T5], QueryParam[T6], QueryParam[T7], QueryParam[T8]),
    (U1, U2, U3, U4, U5, U6, U7, U8)
  ] =
    new ProvideQueryParams[
      (QueryParam[T1], QueryParam[T2], QueryParam[T3], QueryParam[T4], QueryParam[T5], QueryParam[T6], QueryParam[T7], QueryParam[T8]),
      (U1, U2, U3, U4, U5, U6, U7, U8)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h)
          )
        )
      }
    }

  implicit def provideQueryParams9[
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
  ]: ProvideQueryParams[
    (
        QueryParam[T1],
        QueryParam[T2],
        QueryParam[T3],
        QueryParam[T4],
        QueryParam[T5],
        QueryParam[T6],
        QueryParam[T7],
        QueryParam[T8],
        QueryParam[T9]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9)
  ] =
    new ProvideQueryParams[
      (
          QueryParam[T1],
          QueryParam[T2],
          QueryParam[T3],
          QueryParam[T4],
          QueryParam[T5],
          QueryParam[T6],
          QueryParam[T7],
          QueryParam[T8],
          QueryParam[T9]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8],
              QueryParam[T9]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h, i) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h),
            queryParameters._9.serialize(i)
          )
        )
      }
    }

  implicit def provideQueryParams10[
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
  ]: ProvideQueryParams[
    (
        QueryParam[T1],
        QueryParam[T2],
        QueryParam[T3],
        QueryParam[T4],
        QueryParam[T5],
        QueryParam[T6],
        QueryParam[T7],
        QueryParam[T8],
        QueryParam[T9],
        QueryParam[T10]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10)
  ] =
    new ProvideQueryParams[
      (
          QueryParam[T1],
          QueryParam[T2],
          QueryParam[T3],
          QueryParam[T4],
          QueryParam[T5],
          QueryParam[T6],
          QueryParam[T7],
          QueryParam[T8],
          QueryParam[T9],
          QueryParam[T10]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8],
              QueryParam[T9],
              QueryParam[T10]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h, i, j) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h),
            queryParameters._9.serialize(i),
            queryParameters._10.serialize(j)
          )
        )
      }
    }

  implicit def provideQueryParams11[
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
  ]: ProvideQueryParams[
    (
        QueryParam[T1],
        QueryParam[T2],
        QueryParam[T3],
        QueryParam[T4],
        QueryParam[T5],
        QueryParam[T6],
        QueryParam[T7],
        QueryParam[T8],
        QueryParam[T9],
        QueryParam[T10],
        QueryParam[T11]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11)
  ] =
    new ProvideQueryParams[
      (
          QueryParam[T1],
          QueryParam[T2],
          QueryParam[T3],
          QueryParam[T4],
          QueryParam[T5],
          QueryParam[T6],
          QueryParam[T7],
          QueryParam[T8],
          QueryParam[T9],
          QueryParam[T10],
          QueryParam[T11]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8],
              QueryParam[T9],
              QueryParam[T10],
              QueryParam[T11]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h, i, j, k) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h),
            queryParameters._9.serialize(i),
            queryParameters._10.serialize(j),
            queryParameters._11.serialize(k)
          )
        )
      }
    }

  implicit def provideQueryParams12[
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
  ]: ProvideQueryParams[
    (
        QueryParam[T1],
        QueryParam[T2],
        QueryParam[T3],
        QueryParam[T4],
        QueryParam[T5],
        QueryParam[T6],
        QueryParam[T7],
        QueryParam[T8],
        QueryParam[T9],
        QueryParam[T10],
        QueryParam[T11],
        QueryParam[T12]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12)
  ] =
    new ProvideQueryParams[
      (
          QueryParam[T1],
          QueryParam[T2],
          QueryParam[T3],
          QueryParam[T4],
          QueryParam[T5],
          QueryParam[T6],
          QueryParam[T7],
          QueryParam[T8],
          QueryParam[T9],
          QueryParam[T10],
          QueryParam[T11],
          QueryParam[T12]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8],
              QueryParam[T9],
              QueryParam[T10],
              QueryParam[T11],
              QueryParam[T12]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h, i, j, k, l) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h),
            queryParameters._9.serialize(i),
            queryParameters._10.serialize(j),
            queryParameters._11.serialize(k),
            queryParameters._12.serialize(l)
          )
        )
      }
    }

  implicit def provideQueryParams13[
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
  ]: ProvideQueryParams[
    (
        QueryParam[T1],
        QueryParam[T2],
        QueryParam[T3],
        QueryParam[T4],
        QueryParam[T5],
        QueryParam[T6],
        QueryParam[T7],
        QueryParam[T8],
        QueryParam[T9],
        QueryParam[T10],
        QueryParam[T11],
        QueryParam[T12],
        QueryParam[T13]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13)
  ] =
    new ProvideQueryParams[
      (
          QueryParam[T1],
          QueryParam[T2],
          QueryParam[T3],
          QueryParam[T4],
          QueryParam[T5],
          QueryParam[T6],
          QueryParam[T7],
          QueryParam[T8],
          QueryParam[T9],
          QueryParam[T10],
          QueryParam[T11],
          QueryParam[T12],
          QueryParam[T13]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8],
              QueryParam[T9],
              QueryParam[T10],
              QueryParam[T11],
              QueryParam[T12],
              QueryParam[T13]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h, i, j, k, l, m) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h),
            queryParameters._9.serialize(i),
            queryParameters._10.serialize(j),
            queryParameters._11.serialize(k),
            queryParameters._12.serialize(l),
            queryParameters._13.serialize(m)
          )
        )
      }
    }

  implicit def provideQueryParams14[
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
  ]: ProvideQueryParams[
    (
        QueryParam[T1],
        QueryParam[T2],
        QueryParam[T3],
        QueryParam[T4],
        QueryParam[T5],
        QueryParam[T6],
        QueryParam[T7],
        QueryParam[T8],
        QueryParam[T9],
        QueryParam[T10],
        QueryParam[T11],
        QueryParam[T12],
        QueryParam[T13],
        QueryParam[T14]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14)
  ] =
    new ProvideQueryParams[
      (
          QueryParam[T1],
          QueryParam[T2],
          QueryParam[T3],
          QueryParam[T4],
          QueryParam[T5],
          QueryParam[T6],
          QueryParam[T7],
          QueryParam[T8],
          QueryParam[T9],
          QueryParam[T10],
          QueryParam[T11],
          QueryParam[T12],
          QueryParam[T13],
          QueryParam[T14]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8],
              QueryParam[T9],
              QueryParam[T10],
              QueryParam[T11],
              QueryParam[T12],
              QueryParam[T13],
              QueryParam[T14]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h),
            queryParameters._9.serialize(i),
            queryParameters._10.serialize(j),
            queryParameters._11.serialize(k),
            queryParameters._12.serialize(l),
            queryParameters._13.serialize(m),
            queryParameters._14.serialize(n)
          )
        )
      }
    }

  implicit def provideQueryParams15[
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
  ]: ProvideQueryParams[
    (
        QueryParam[T1],
        QueryParam[T2],
        QueryParam[T3],
        QueryParam[T4],
        QueryParam[T5],
        QueryParam[T6],
        QueryParam[T7],
        QueryParam[T8],
        QueryParam[T9],
        QueryParam[T10],
        QueryParam[T11],
        QueryParam[T12],
        QueryParam[T13],
        QueryParam[T14],
        QueryParam[T15]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15)
  ] =
    new ProvideQueryParams[
      (
          QueryParam[T1],
          QueryParam[T2],
          QueryParam[T3],
          QueryParam[T4],
          QueryParam[T5],
          QueryParam[T6],
          QueryParam[T7],
          QueryParam[T8],
          QueryParam[T9],
          QueryParam[T10],
          QueryParam[T11],
          QueryParam[T12],
          QueryParam[T13],
          QueryParam[T14],
          QueryParam[T15]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8],
              QueryParam[T9],
              QueryParam[T10],
              QueryParam[T11],
              QueryParam[T12],
              QueryParam[T13],
              QueryParam[T14],
              QueryParam[T15]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h),
            queryParameters._9.serialize(i),
            queryParameters._10.serialize(j),
            queryParameters._11.serialize(k),
            queryParameters._12.serialize(l),
            queryParameters._13.serialize(m),
            queryParameters._14.serialize(n),
            queryParameters._15.serialize(o)
          )
        )
      }
    }

  implicit def provideQueryParams16[
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
  ]: ProvideQueryParams[
    (
        QueryParam[T1],
        QueryParam[T2],
        QueryParam[T3],
        QueryParam[T4],
        QueryParam[T5],
        QueryParam[T6],
        QueryParam[T7],
        QueryParam[T8],
        QueryParam[T9],
        QueryParam[T10],
        QueryParam[T11],
        QueryParam[T12],
        QueryParam[T13],
        QueryParam[T14],
        QueryParam[T15],
        QueryParam[T16]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16)
  ] =
    new ProvideQueryParams[
      (
          QueryParam[T1],
          QueryParam[T2],
          QueryParam[T3],
          QueryParam[T4],
          QueryParam[T5],
          QueryParam[T6],
          QueryParam[T7],
          QueryParam[T8],
          QueryParam[T9],
          QueryParam[T10],
          QueryParam[T11],
          QueryParam[T12],
          QueryParam[T13],
          QueryParam[T14],
          QueryParam[T15],
          QueryParam[T16]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8],
              QueryParam[T9],
              QueryParam[T10],
              QueryParam[T11],
              QueryParam[T12],
              QueryParam[T13],
              QueryParam[T14],
              QueryParam[T15],
              QueryParam[T16]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h),
            queryParameters._9.serialize(i),
            queryParameters._10.serialize(j),
            queryParameters._11.serialize(k),
            queryParameters._12.serialize(l),
            queryParameters._13.serialize(m),
            queryParameters._14.serialize(n),
            queryParameters._15.serialize(o),
            queryParameters._16.serialize(p)
          )
        )
      }
    }

  implicit def provideQueryParams17[
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
  ]: ProvideQueryParams[
    (
        QueryParam[T1],
        QueryParam[T2],
        QueryParam[T3],
        QueryParam[T4],
        QueryParam[T5],
        QueryParam[T6],
        QueryParam[T7],
        QueryParam[T8],
        QueryParam[T9],
        QueryParam[T10],
        QueryParam[T11],
        QueryParam[T12],
        QueryParam[T13],
        QueryParam[T14],
        QueryParam[T15],
        QueryParam[T16],
        QueryParam[T17]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17)
  ] =
    new ProvideQueryParams[
      (
          QueryParam[T1],
          QueryParam[T2],
          QueryParam[T3],
          QueryParam[T4],
          QueryParam[T5],
          QueryParam[T6],
          QueryParam[T7],
          QueryParam[T8],
          QueryParam[T9],
          QueryParam[T10],
          QueryParam[T11],
          QueryParam[T12],
          QueryParam[T13],
          QueryParam[T14],
          QueryParam[T15],
          QueryParam[T16],
          QueryParam[T17]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8],
              QueryParam[T9],
              QueryParam[T10],
              QueryParam[T11],
              QueryParam[T12],
              QueryParam[T13],
              QueryParam[T14],
              QueryParam[T15],
              QueryParam[T16],
              QueryParam[T17]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h),
            queryParameters._9.serialize(i),
            queryParameters._10.serialize(j),
            queryParameters._11.serialize(k),
            queryParameters._12.serialize(l),
            queryParameters._13.serialize(m),
            queryParameters._14.serialize(n),
            queryParameters._15.serialize(o),
            queryParameters._16.serialize(p),
            queryParameters._17.serialize(q)
          )
        )
      }
    }

  implicit def provideQueryParams18[
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
  ]: ProvideQueryParams[
    (
        QueryParam[T1],
        QueryParam[T2],
        QueryParam[T3],
        QueryParam[T4],
        QueryParam[T5],
        QueryParam[T6],
        QueryParam[T7],
        QueryParam[T8],
        QueryParam[T9],
        QueryParam[T10],
        QueryParam[T11],
        QueryParam[T12],
        QueryParam[T13],
        QueryParam[T14],
        QueryParam[T15],
        QueryParam[T16],
        QueryParam[T17],
        QueryParam[T18]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18)
  ] =
    new ProvideQueryParams[
      (
          QueryParam[T1],
          QueryParam[T2],
          QueryParam[T3],
          QueryParam[T4],
          QueryParam[T5],
          QueryParam[T6],
          QueryParam[T7],
          QueryParam[T8],
          QueryParam[T9],
          QueryParam[T10],
          QueryParam[T11],
          QueryParam[T12],
          QueryParam[T13],
          QueryParam[T14],
          QueryParam[T15],
          QueryParam[T16],
          QueryParam[T17],
          QueryParam[T18]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8],
              QueryParam[T9],
              QueryParam[T10],
              QueryParam[T11],
              QueryParam[T12],
              QueryParam[T13],
              QueryParam[T14],
              QueryParam[T15],
              QueryParam[T16],
              QueryParam[T17],
              QueryParam[T18]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h),
            queryParameters._9.serialize(i),
            queryParameters._10.serialize(j),
            queryParameters._11.serialize(k),
            queryParameters._12.serialize(l),
            queryParameters._13.serialize(m),
            queryParameters._14.serialize(n),
            queryParameters._15.serialize(o),
            queryParameters._16.serialize(p),
            queryParameters._17.serialize(q),
            queryParameters._18.serialize(r)
          )
        )
      }
    }

  implicit def provideQueryParams19[
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
  ]: ProvideQueryParams[
    (
        QueryParam[T1],
        QueryParam[T2],
        QueryParam[T3],
        QueryParam[T4],
        QueryParam[T5],
        QueryParam[T6],
        QueryParam[T7],
        QueryParam[T8],
        QueryParam[T9],
        QueryParam[T10],
        QueryParam[T11],
        QueryParam[T12],
        QueryParam[T13],
        QueryParam[T14],
        QueryParam[T15],
        QueryParam[T16],
        QueryParam[T17],
        QueryParam[T18],
        QueryParam[T19]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19)
  ] =
    new ProvideQueryParams[
      (
          QueryParam[T1],
          QueryParam[T2],
          QueryParam[T3],
          QueryParam[T4],
          QueryParam[T5],
          QueryParam[T6],
          QueryParam[T7],
          QueryParam[T8],
          QueryParam[T9],
          QueryParam[T10],
          QueryParam[T11],
          QueryParam[T12],
          QueryParam[T13],
          QueryParam[T14],
          QueryParam[T15],
          QueryParam[T16],
          QueryParam[T17],
          QueryParam[T18],
          QueryParam[T19]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8],
              QueryParam[T9],
              QueryParam[T10],
              QueryParam[T11],
              QueryParam[T12],
              QueryParam[T13],
              QueryParam[T14],
              QueryParam[T15],
              QueryParam[T16],
              QueryParam[T17],
              QueryParam[T18],
              QueryParam[T19]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h),
            queryParameters._9.serialize(i),
            queryParameters._10.serialize(j),
            queryParameters._11.serialize(k),
            queryParameters._12.serialize(l),
            queryParameters._13.serialize(m),
            queryParameters._14.serialize(n),
            queryParameters._15.serialize(o),
            queryParameters._16.serialize(p),
            queryParameters._17.serialize(q),
            queryParameters._18.serialize(r),
            queryParameters._19.serialize(s)
          )
        )
      }
    }

  implicit def provideQueryParams20[
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
  ]: ProvideQueryParams[
    (
        QueryParam[T1],
        QueryParam[T2],
        QueryParam[T3],
        QueryParam[T4],
        QueryParam[T5],
        QueryParam[T6],
        QueryParam[T7],
        QueryParam[T8],
        QueryParam[T9],
        QueryParam[T10],
        QueryParam[T11],
        QueryParam[T12],
        QueryParam[T13],
        QueryParam[T14],
        QueryParam[T15],
        QueryParam[T16],
        QueryParam[T17],
        QueryParam[T18],
        QueryParam[T19],
        QueryParam[T20]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20)
  ] =
    new ProvideQueryParams[
      (
          QueryParam[T1],
          QueryParam[T2],
          QueryParam[T3],
          QueryParam[T4],
          QueryParam[T5],
          QueryParam[T6],
          QueryParam[T7],
          QueryParam[T8],
          QueryParam[T9],
          QueryParam[T10],
          QueryParam[T11],
          QueryParam[T12],
          QueryParam[T13],
          QueryParam[T14],
          QueryParam[T15],
          QueryParam[T16],
          QueryParam[T17],
          QueryParam[T18],
          QueryParam[T19],
          QueryParam[T20]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8],
              QueryParam[T9],
              QueryParam[T10],
              QueryParam[T11],
              QueryParam[T12],
              QueryParam[T13],
              QueryParam[T14],
              QueryParam[T15],
              QueryParam[T16],
              QueryParam[T17],
              QueryParam[T18],
              QueryParam[T19],
              QueryParam[T20]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h),
            queryParameters._9.serialize(i),
            queryParameters._10.serialize(j),
            queryParameters._11.serialize(k),
            queryParameters._12.serialize(l),
            queryParameters._13.serialize(m),
            queryParameters._14.serialize(n),
            queryParameters._15.serialize(o),
            queryParameters._16.serialize(p),
            queryParameters._17.serialize(q),
            queryParameters._18.serialize(r),
            queryParameters._19.serialize(s),
            queryParameters._20.serialize(t)
          )
        )
      }
    }

  implicit def provideQueryParams21[
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
  ]: ProvideQueryParams[
    (
        QueryParam[T1],
        QueryParam[T2],
        QueryParam[T3],
        QueryParam[T4],
        QueryParam[T5],
        QueryParam[T6],
        QueryParam[T7],
        QueryParam[T8],
        QueryParam[T9],
        QueryParam[T10],
        QueryParam[T11],
        QueryParam[T12],
        QueryParam[T13],
        QueryParam[T14],
        QueryParam[T15],
        QueryParam[T16],
        QueryParam[T17],
        QueryParam[T18],
        QueryParam[T19],
        QueryParam[T20],
        QueryParam[T21]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20, U21)
  ] =
    new ProvideQueryParams[
      (
          QueryParam[T1],
          QueryParam[T2],
          QueryParam[T3],
          QueryParam[T4],
          QueryParam[T5],
          QueryParam[T6],
          QueryParam[T7],
          QueryParam[T8],
          QueryParam[T9],
          QueryParam[T10],
          QueryParam[T11],
          QueryParam[T12],
          QueryParam[T13],
          QueryParam[T14],
          QueryParam[T15],
          QueryParam[T16],
          QueryParam[T17],
          QueryParam[T18],
          QueryParam[T19],
          QueryParam[T20],
          QueryParam[T21]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20, U21)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8],
              QueryParam[T9],
              QueryParam[T10],
              QueryParam[T11],
              QueryParam[T12],
              QueryParam[T13],
              QueryParam[T14],
              QueryParam[T15],
              QueryParam[T16],
              QueryParam[T17],
              QueryParam[T18],
              QueryParam[T19],
              QueryParam[T20],
              QueryParam[T21]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20, U21),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h),
            queryParameters._9.serialize(i),
            queryParameters._10.serialize(j),
            queryParameters._11.serialize(k),
            queryParameters._12.serialize(l),
            queryParameters._13.serialize(m),
            queryParameters._14.serialize(n),
            queryParameters._15.serialize(o),
            queryParameters._16.serialize(p),
            queryParameters._17.serialize(q),
            queryParameters._18.serialize(r),
            queryParameters._19.serialize(s),
            queryParameters._20.serialize(t),
            queryParameters._21.serialize(u)
          )
        )
      }
    }

  implicit def provideQueryParams22[
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
  ]: ProvideQueryParams[
    (
        QueryParam[T1],
        QueryParam[T2],
        QueryParam[T3],
        QueryParam[T4],
        QueryParam[T5],
        QueryParam[T6],
        QueryParam[T7],
        QueryParam[T8],
        QueryParam[T9],
        QueryParam[T10],
        QueryParam[T11],
        QueryParam[T12],
        QueryParam[T13],
        QueryParam[T14],
        QueryParam[T15],
        QueryParam[T16],
        QueryParam[T17],
        QueryParam[T18],
        QueryParam[T19],
        QueryParam[T20],
        QueryParam[T21],
        QueryParam[T22]
    ),
    (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20, U21, U22)
  ] =
    new ProvideQueryParams[
      (
          QueryParam[T1],
          QueryParam[T2],
          QueryParam[T3],
          QueryParam[T4],
          QueryParam[T5],
          QueryParam[T6],
          QueryParam[T7],
          QueryParam[T8],
          QueryParam[T9],
          QueryParam[T10],
          QueryParam[T11],
          QueryParam[T12],
          QueryParam[T13],
          QueryParam[T14],
          QueryParam[T15],
          QueryParam[T16],
          QueryParam[T17],
          QueryParam[T18],
          QueryParam[T19],
          QueryParam[T20],
          QueryParam[T21],
          QueryParam[T22]
      ),
      (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20, U21, U22)
    ] {
      override def apply(
          queryParameters: (
              QueryParam[T1],
              QueryParam[T2],
              QueryParam[T3],
              QueryParam[T4],
              QueryParam[T5],
              QueryParam[T6],
              QueryParam[T7],
              QueryParam[T8],
              QueryParam[T9],
              QueryParam[T10],
              QueryParam[T11],
              QueryParam[T12],
              QueryParam[T13],
              QueryParam[T14],
              QueryParam[T15],
              QueryParam[T16],
              QueryParam[T17],
              QueryParam[T18],
              QueryParam[T19],
              QueryParam[T20],
              QueryParam[T21],
              QueryParam[T22]
          ),
          queryParametersProvided: (U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13, U14, U15, U16, U17, U18, U19, U20, U21, U22),
          uri: String
      ): String = {
        val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = queryParametersProvided
        addSerializedQueryParametersToUri(
          uri,
          Seq(
            queryParameters._1.serialize(a),
            queryParameters._2.serialize(b),
            queryParameters._3.serialize(c),
            queryParameters._4.serialize(d),
            queryParameters._5.serialize(e),
            queryParameters._6.serialize(f),
            queryParameters._7.serialize(g),
            queryParameters._8.serialize(h),
            queryParameters._9.serialize(i),
            queryParameters._10.serialize(j),
            queryParameters._11.serialize(k),
            queryParameters._12.serialize(l),
            queryParameters._13.serialize(m),
            queryParameters._14.serialize(n),
            queryParameters._15.serialize(o),
            queryParameters._16.serialize(p),
            queryParameters._17.serialize(q),
            queryParameters._18.serialize(r),
            queryParameters._19.serialize(s),
            queryParameters._20.serialize(t),
            queryParameters._21.serialize(u),
            queryParameters._22.serialize(v)
          )
        )
      }
    }

  def addQueryParametersToUri(uri: String, queryParameters: Map[String, Seq[String]]): String =
    addSerializedQueryParametersToUri(
      uri,
      queryParameters.toSeq.map { case (name, values) => CollectionStyle.Exploded.serialize(name, values) }
    )

  private def addSerializedQueryParametersToUri(uri: String, serializedParameters: Seq[Seq[String]]): String = {
    val pairs = serializedParameters.flatten
    if (pairs.isEmpty) {
      uri
    } else {
      val (baseUri, fragment) = uri.splitAt(uri.indexOf("#") match {
        case -1  => uri.length
        case idx => idx
      })

      val baseWithQuery = if (baseUri.contains("?")) s"$baseUri&" else s"$baseUri?"

      baseWithQuery + pairs.mkString("&") + fragment
    }
  }
}
