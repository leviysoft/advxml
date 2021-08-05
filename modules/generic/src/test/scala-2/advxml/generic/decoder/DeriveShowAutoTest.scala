package advxml.generic.decoder

import advxml.generic.show.{Show, ShowDerivation}
import org.scalatest.funsuite.AnyFunSuite

class DeriveShowAutoTest extends AnyFunSuite {

  case class Person(name: String)
  case class Car(typeOf: String, isSuperCar: Boolean, name: String)

  implicit val showForString: Show[String] = (t: String) => t
  implicit val showForBoolean: Show[Boolean] = (t: Boolean) => t.toString

  test("Show") {

    val test2 = Car("super", true, "very coolcar")
    implicit val show: Show[Car] = ShowDerivation.gen[Car]

    Console.print(implicitly[Show[Car]].show(test2))
  }
}
