package advxml.generic.show

import magnolia._

object ShowDerivation {
  type Typeclass[T] = Show[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Show[T] = (t: T) => {
    val paramShows = caseClass.parameters.map { p =>
      p.label + "=" + p.typeclass.show(p.dereference(t))
    }
    caseClass.typeName.toString + paramShows.mkString("(", ",", ")")
  }

  def dispatch[T](sealedTrait: SealedTrait[Show, T]): Show[T] = (t: T) =>
    sealedTrait.dispatch(t) { subtype =>
      subtype.typeclass.show(subtype.cast(t))
    }

  implicit def gen[T]: Show[T] = macro Magnolia.gen[T]
}
