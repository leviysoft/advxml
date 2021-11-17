package advxml.experimental.cursor

import advxml.experimental.codec.{Decoder, DecodingFailure}
import advxml.experimental.{Xml, XmlTree}
import cats.data.{NonEmptyList, Validated}

class CursorWithDecoder[X <: Xml, T: Decoder](cursor: Cursor[X]) { $this =>

  def run[XIN <: XmlTree](xml: XIN): CursorResult[T] =
    cursor
      .focus(xml)
      .flatMap((focus: X) =>
        Decoder[T].decode(focus) match {
          case Validated.Valid(t) =>
            CursorResult.Focused(t)
          case e: Validated.Invalid[NonEmptyList[DecodingFailure]] =>
            CursorResult.DecodingFailure(cursor.path, e)
        }
      )

  def flatMap[U](f: T => CursorWithDecoder[X, U]): CursorWithDecoder[X, U] =
    new CursorWithDecoder[X, U](cursor)(
      Decoder.const[U](
        Decoder.Result.failed(DecodingFailure.Error(new RuntimeException("")))
      )
    ) {
      override def run[XIN <: XmlTree](xml: XIN): CursorResult[U] = {
        $this.run(xml) match {
          case CursorResult.Focused(value) => f(value).run(xml)
          case failed: CursorResult.Failed => failed
        }
      }
    }

  def map[U](f: T => U): CursorWithDecoder[X, U] =
    new CursorWithDecoder[X, U](cursor)(Decoder[T].map(f))
}
