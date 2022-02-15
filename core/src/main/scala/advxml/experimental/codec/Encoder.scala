package advxml.experimental.codec

import advxml.experimental.{Xml, XmlData, XmlString}
import cats.Contravariant

import scala.xml.NodeSeq

// T => XML
trait Encoder[-T] {

  def encode(t: T): Xml

  def contramap[U](f: U => T): Encoder[U] =
    Encoder.of(f.andThen(encode))
}
object Encoder extends EncoderInstances {

  // lazy due circular dependencies with instances
  lazy val id: Encoder[Xml] = Encoder.of(identity)

  def apply[T: Encoder]: Encoder[T] = implicitly[Encoder[T]]

  def of[T](f: T => Xml): Encoder[T] = (t: T) => f(t)

  def fromNodeSeq[T](f: T => NodeSeq): Encoder[T] =
    Encoder.of(t => Xml.fromNodeSeq(f(t)))

  def pure[T](ns: => Xml): Encoder[T] = Encoder(_ => ns)
}

private[advxml] trait EncoderInstances extends EncoderPrimitivesInstances {

  implicit val catsContravariantInstanceForEncoder: Contravariant[Encoder] =
    new Contravariant[Encoder] {
      override def contramap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] = fa.contramap(f)
    }
}
private[advxml] trait EncoderPrimitivesInstances {
  implicit val encoderXml: Encoder[Xml] = Encoder.id
}

// data encoder
trait DataEncoder[-T] extends Encoder[T] {
  override def encode(t: T): XmlData
  override def contramap[U](f: U => T): DataEncoder[U] =
    DataEncoder.of(f.andThen(encode))
}
object DataEncoder extends DataEncoderPrimitivesInstances {
  def apply[T: DataEncoder]: DataEncoder[T] = implicitly[DataEncoder[T]]

  def of[T](f: T => XmlData): DataEncoder[T] = (t: T) => f(t)
}

private[advxml] trait DataEncoderPrimitivesInstances {
  implicit val encoderString: DataEncoder[String]         = DataEncoder.of(XmlString(_))
  implicit val encoderChar: DataEncoder[Char]             = encoderString.contramap(_.toString)
  implicit val encoderInt: DataEncoder[Int]               = encoderString.contramap(_.toString)
  implicit val encoderLong: DataEncoder[Long]             = encoderString.contramap(_.toString)
  implicit val encoderFloat: DataEncoder[Float]           = encoderString.contramap(_.toString)
  implicit val encoderDouble: DataEncoder[Double]         = encoderString.contramap(_.toString)
  implicit val encoderBigDecimal: DataEncoder[BigDecimal] = encoderString.contramap(_.toString)
}
