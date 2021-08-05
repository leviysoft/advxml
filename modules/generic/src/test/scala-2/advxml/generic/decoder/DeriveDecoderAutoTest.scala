package advxml.generic.decoder

import advxml.core.data.{ValidatedNelThrow, XmlDecoder}
import cats.data.Validated.Valid
import org.scalatest.funsuite.AnyFunSuite

import scala.xml.NodeSeq

class DeriveDecoderAutoTest extends AnyFunSuite {

  import advxml.generic.decoder.auto._
  import advxml.implicits._

  test("Simple case") {

//    val a = implicitly[Converter[Text, String]]
    implicit val genDecoder: XmlDecoder[Foo] = gen[Foo]
    case class Foo(bar: String, age: String)
    val xml: NodeSeq = <foo bar="TEST" age="10"/>
    val result: ValidatedNelThrow[Foo] = xml.decode[Foo]

    assert(result == Valid(Foo("TEST", age = "10")))
  }
//
//  test("Simple case with monadic values - Option") {
//    case class Foo(bar: String, age: Option[String])
//    val xml: NodeSeq = <foo bar="TEST"/>
//    val result: ValidatedNelThrow[Foo] = xml.decode[Foo]
//
//    assert(result == Valid(Foo("TEST", age = None)))
//  }
//
//  test("Simple case with monadic values - List") {
//    case class Foo(bar: String, age: List[String])
//    val xml: NodeSeq = <foo bar="TEST"/>
//    val result: ValidatedNelThrow[Foo] = xml.decode[Foo]
//
//    assert(result == Valid(Foo("TEST", age = Nil)))
//  }
//
//  test("Nested case with monadic values - Option") {
//    case class Foo(bar: String, age: Option[String], buzz: Buzz)
//    case class Buzz(buzz: String, wow: String)
//    val xml: NodeSeq = <foo bar="TEST"><buzz buzz="BUZZ" wow="23"></buzz></foo>
//    val result: ValidatedNelThrow[Foo] = xml.decode[Foo]
//
//    assert(result == Valid(Foo("TEST", age = None, buzz = Buzz("BUZZ", "23"))))
//  }
//
//  test("Nested case with duplicates with monadic values - Option") {
//    case class Foo(bar: String, age: Option[String], @node buzz: Buzz)
//    case class Buzz(buzz: String, wow: String)
//
//    val xml: NodeSeq =
//      <foo bar="TEST" buzz="TEST">
//        <buzz buzz="BUZZ" wow="23"></buzz>
//      </foo>
//    val result: ValidatedNelThrow[Foo] = xml.decode[Foo]
//
//    assert(result == Valid(Foo("TEST", age = None, buzz = Buzz("BUZZ", "23"))))
//  }
}
