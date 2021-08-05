package advxml.generic.decoder

import advxml.generic.Configuration
import magnolia.{CaseClass, Magnolia}

import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag

object auto {

  type Typeclass[T] = MagnoliaXmlDecoder.Typeclass[T]

  def combine[T: ClassTag: TypeTag](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
    MagnoliaXmlDecoder.combine(caseClass)(Configuration.default)

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}
