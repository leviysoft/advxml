package advxml.experimental.codec

import advxml.experimental.{Xml, XmlString}
import cats.Contravariant

// T => XML
trait Encoder[-T] {

  def encode(t: T): Xml

  def contramap[U](f: U => T): Encoder[U] =
    Encoder.of(f.andThen(encode))
}

object Encoder extends EncoderInstances {

  val id: Encoder[Xml] = Encoder.of(identity)

  def apply[T: Encoder]: Encoder[T] = implicitly[Encoder[T]]

  def of[T](f: T => Xml): Encoder[T] = (t: T) => f(t)

  def pure[T](ns: => Xml): Encoder[T] = Encoder(_ => ns)
}

private[advxml] trait EncoderInstances {

  implicit val encoderXml: Encoder[Xml]               = Encoder.id
  implicit val encoderString: Encoder[String]         = Encoder.of(XmlString(_))
  implicit val encoderChar: Encoder[Char]             = encoderString.contramap(_.toString)
  implicit val encoderInt: Encoder[Int]               = encoderString.contramap(_.toString)
  implicit val encoderLong: Encoder[Long]             = encoderString.contramap(_.toString)
  implicit val encoderFloat: Encoder[Float]           = encoderString.contramap(_.toString)
  implicit val encoderDouble: Encoder[Double]         = encoderString.contramap(_.toString)
  implicit val encoderBigDecimal: Encoder[BigDecimal] = encoderString.contramap(_.toString)

  implicit val catsContravariantInstanceForEncoder: Contravariant[Encoder] =
    new Contravariant[Encoder] {
      override def contramap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] = fa.contramap(f)
    }
}
