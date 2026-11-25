package pl.iterators.baklava

trait SchemaDerivationCompat { this: SchemaDerivation =>
  def derived[T]: Schema[T] = macro internal.SchemaMacros.derivedImpl[T]

  def derivedSum[T](encoding: SumEncoding): Schema[T] = macro internal.SchemaMacros.derivedSumImpl[T]

  implicit def gen[T]: Schema[T] = macro internal.SchemaMacros.genImpl[T]
}
