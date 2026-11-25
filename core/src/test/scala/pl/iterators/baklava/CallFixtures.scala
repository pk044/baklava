package pl.iterators.baklava

import sttp.model.StatusCode

object CallFixtures {

  def requestContextAt(
      symbolicPath: String,
      path: String,
      pathParameters: Seq[PathParam[?]] = Nil,
      queryParameters: Seq[QueryParam[?]] = Nil
  ): BaklavaRequestContext[Unit, Unit, Unit, Unit, Unit, Unit, Unit] =
    BaklavaRequestContext[Unit, Unit, Unit, Unit, Unit, Unit, Unit](
      symbolicPath = symbolicPath,
      path = path,
      pathDescription = None,
      pathSummary = None,
      method = None,
      operationDescription = None,
      operationSummary = None,
      operationId = None,
      operationTags = Seq.empty,
      securitySchemes = Seq.empty,
      body = None,
      bodySchema = None,
      headers = Seq.empty,
      headersDefinition = (),
      headersProvided = (),
      headersSeq = Seq.empty,
      security = AppliedSecurity(NoopSecurity, Map.empty),
      pathParameters = (),
      pathParametersProvided = (),
      pathParametersSeq = pathParameters,
      queryParameters = (),
      queryParametersProvided = (),
      queryParametersSeq = queryParameters,
      responseDescription = None,
      responseHeaders = Seq.empty
    )

  def callAt(symbolicPath: String, path: String, tags: Seq[String] = Nil): BaklavaSerializableCall = BaklavaSerializableCall(
    BaklavaRequestContextSerializable(
      symbolicPath = symbolicPath,
      path = path,
      pathDescription = None,
      pathSummary = None,
      method = None,
      operationDescription = None,
      operationSummary = None,
      operationId = None,
      operationTags = tags,
      securitySchemes = Seq.empty,
      bodySchema = None,
      headersSeq = Seq.empty,
      pathParametersSeq = Seq.empty,
      queryParametersSeq = Seq.empty,
      responseDescription = None,
      responseHeaders = Seq.empty
    ),
    BaklavaResponseContextSerializable(
      protocol = BaklavaHttpProtocol("HTTP/1.1"),
      status = StatusCode.Ok,
      headers = Seq.empty,
      requestContentType = None,
      responseContentType = None,
      bodySchema = None
    )
  )
}
