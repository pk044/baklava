package pl.iterators.baklava

import scala.deriving.Mirror

trait SchemaDerivationCompat { this: SchemaDerivation =>
  inline def derived[T]: Schema[T] = ${ internal.SchemaMacros.derived[T]('{ this }) }

  inline def derivedSum[T](inline encoding: SumEncoding): Schema[T] = ${ internal.SchemaMacros.derivedSum[T]('{ this }, 'encoding) }

  inline given autoDerived[T](using Mirror.ProductOf[T]): Schema[T] = ${ internal.SchemaMacros.autoDerived[T]('{ this }) }

  inline given autoDerivedValueClass[T <: AnyVal]: Schema[T] = ${ internal.SchemaMacros.autoDerived[T]('{ this }) }
}
