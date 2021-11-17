package advxml.experimental.codec

import advxml.experimental.Xml
import advxml.experimental.cursor.CursorResult

trait DecodingResult

case class DecodingFailure(reason: DecodingFailure.Reason)
object DecodingFailure {
  sealed trait Reason
  case class Error(ex: Throwable) extends Reason
  case class NoTextAvailable(subject: Xml) extends Reason
  case class CursorFailure(failed: CursorResult.Failed) extends Reason
  case class Custom(message: String) extends Reason
}
