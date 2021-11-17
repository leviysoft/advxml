package advxml.experimental.codec

import advxml.experimental.*
import advxml.experimental.codec.DecodingFailure.NoTextAvailable
import advxml.experimental.cursor.{CursorResult, CursorWithDecoder, NodeCursor}
import advxml.experimental.cursor.NodeCursor.Root
import cats.data.{NonEmptyList, ValidatedNel}
import cats.MonadError
import cats.data.Validated.{Invalid, Valid}

import scala.util.Try

trait Decoder[T] {

  import cats.implicits.*

  def decode(xml: Xml): Decoder.Result[T]

  def map[U](f: T => U): Decoder[U] =
    flatMap(f.andThen(Decoder.pure(_)))

  def emap[U](f: T => Either[DecodingFailure, U]): Decoder[U] =
    flatMap(t => Decoder.const(f(t).toValidatedNel[DecodingFailure]))

  def emap[E, U](f: T => Either[E, U])(implicit ctx: E <:< Throwable): Decoder[U] =
    emap(f.andThen(_.leftMap(e => DecodingFailure(DecodingFailure.Error(e)))))

  def emapTry[U](f: T => Try[U]): Decoder[U] =
    emap(f.andThen(_.toEither))

  def flatMap[U](f: T => Decoder[U]): Decoder[U] =
    Decoder.of(ns => decode(ns).andThen(t => f(t).decode(ns)))
}

object Decoder extends DecoderInstances {

  import cats.implicits.*
  type Result[T]     = ValidatedNel[DecodingFailure, T]
  type InvalidResult = Invalid[NonEmptyList[DecodingFailure]]
  object Result {
    def success[T](t: T): Result[T]                     = t.validNel
    def failed[T](f: DecodingFailure): Result[T]        = f.invalidNel
    def failed[T](f: DecodingFailure.Reason): Result[T] = failed(DecodingFailure(f))
  }

  def id: Decoder[Xml] = fromF(identity)

  def apply[T: Decoder]: Decoder[T] = implicitly[Decoder[T]]

  def of[T](f: Xml => Decoder.Result[T]): Decoder[T] = (xml: Xml) => f(xml)

  def pure[T](t: => T): Decoder[T] =
    Decoder.fromF(_ => t)

  def const[T](r: => Decoder.Result[T]): Decoder[T] =
    Decoder.of(_ => r)

  def fromCursor[T](f: NodeCursor => CursorWithDecoder[? <: Xml, T]): Decoder[T] =
    of {
      case tree: XmlTree =>
        f(Root).run(tree) match {
          case CursorResult.Focused(value) => Result.success(value)
          case failed: CursorResult.Failed => Result.failed(DecodingFailure.CursorFailure(failed))
        }
      case _ =>
        Result.failed(
          DecodingFailure.CursorFailure(CursorResult.WrongTarget("", "XmlTree"))
        )
    }

  def fromF[T](f: Xml => T): Decoder[T] =
    of(f.andThen(Valid(_)))

  def fromEither[T](f: Xml => Either[DecodingFailure, T]): Decoder[T] =
    id.emap(f)

  def fromEither[E, T](f: Xml => Either[E, T])(implicit ctx: E <:< Throwable): Decoder[T] =
    id.emap(f)

  def fromTry[T](f: Xml => Try[T]): Decoder[T] =
    id.emapTry(f)
}

private[advxml] trait DecoderInstances {

  import cats.implicits.*

  implicit val decodeXml: Decoder[Xml]   = Decoder.id
  implicit val decodeUnit: Decoder[Unit] = Decoder.pure[Unit](())
  implicit val decodeString: Decoder[String] = Decoder.of {
    case XmlAttribute(_, value) => decodeString.decode(value)
    case data: XmlData =>
      data match {
        case XmlString(value) => value.validNel
        case XmlNumber(value) => value.toString.validNel
        case XmlByte(value)   => value.toString.validNel
      }
    case sbj => DecodingFailure(NoTextAvailable(sbj)).invalidNel
  }
  implicit val decodeCharArray: Decoder[Array[Char]] = decodeString.map(_.toCharArray)
  implicit val decodeInt: Decoder[Int]               = decodeString.emapTry(s => Try(s.toInt))
  implicit val decodeLong: Decoder[Long]             = decodeString.emapTry(s => Try(s.toLong))
  implicit val decodeFloat: Decoder[Float]           = decodeString.emapTry(s => Try(s.toFloat))
  implicit val decodeDouble: Decoder[Double]         = decodeString.emapTry(s => Try(s.toDouble))
  implicit val decodeBigDecimal: Decoder[BigDecimal] =
    decodeString.emapTry(s => Try(BigDecimal(s)))

  implicit def catsMonadErrorInstanceForDecoder[T]
    : MonadError[Decoder, NonEmptyList[DecodingFailure]] =
    new MonadError[Decoder, NonEmptyList[DecodingFailure]] {

      override def raiseError[A](e: NonEmptyList[DecodingFailure]): Decoder[A] =
        Decoder.const(e.invalid)

      override def handleErrorWith[A](fa: Decoder[A])(
        f: NonEmptyList[DecodingFailure] => Decoder[A]
      ): Decoder[A] =
        Decoder.id.flatMap(ns => fa.decode(ns).fold(f, pure))

      override def pure[A](x: A): Decoder[A] =
        Decoder.pure(x)

      override def flatMap[A, B](fa: Decoder[A])(f: A => Decoder[B]): Decoder[B] =
        fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => Decoder[Either[A, B]]): Decoder[B] =
        f(a).flatMap {
          case Left(aa)  => tailRecM(aa)(f)
          case Right(bb) => Decoder.pure(bb)
        }
    }
}
