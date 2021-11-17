package advxml.experimental.cursor

import advxml.experimental.codec.Decoder
import advxml.experimental.cursor.CursorResult.{
  Failed,
  Focused,
  LeftBoundLimitAttr,
  MissingAttrAtIndex,
  MissingAttrByKey,
  MissingNode,
  RightBoundLimitAttr
}
import cats.Show

sealed trait CursorResult[+T] {

  def map[U](f: T => U): CursorResult[U] =
    flatMap(f.andThen(Focused(_)))

  def flatMap[U](f: T => CursorResult[U]): CursorResult[U] =
    fold(f)(identity)

  def fold[U](ifFocused: T => U)(ifFailed: Failed => U): U = this match {
    case Focused(target) => ifFocused(target)
    case failed: Failed  => ifFailed(failed)
  }

  def recover[U >: T](f: Failed => U): CursorResult[U] =
    recoverWith(failed => Focused(f(failed)))

  def recoverWith[U >: T](f: Failed => CursorResult[U]): CursorResult[U] =
    this match {
      case Focused(target) => Focused[U](target)
      case failed: Failed  => f(failed)
    }

  override def toString: String = Show[CursorResult[Any]].show(this)
}

object CursorResult extends CursorResultInstances {

  case class Focused[T](value: T) extends CursorResult[T]
  trait Failed extends CursorResult[Nothing] {
    val path: String
  }
  trait Missing extends Failed {
    val path: String
  }

  // decode
  case class DecodingFailure(path: String, error: Decoder.InvalidResult) extends Failed

  // node
  trait FailedNode extends Failed
  case class WrongTarget(path: String, expectedType: String) extends FailedNode
  case class MissingNode(path: String, nodeName: String) extends FailedNode with Missing

  // text
  case class MissingText(path: String) extends FailedNode with Missing

  // attrs
  sealed trait FailedAttribute extends Failed
  case class MissingAttrByKey(path: String, key: String) extends FailedAttribute with Missing
  case class MissingAttrAtIndex(path: String, index: Long) extends FailedAttribute with Missing
  case class MissingAttrHead(path: String) extends FailedAttribute with Missing
  case class MissingAttrLast(path: String) extends FailedAttribute with Missing
  case class LeftBoundLimitAttr(path: String, lastKey: String) extends FailedAttribute with Missing
  case class RightBoundLimitAttr(path: String, lastKey: String) extends FailedAttribute with Missing

  // ops
  def fromOption[T](opt: Option[T])(ifEmpty: => CursorResult[T]): CursorResult[T] = {
    opt.fold(ifEmpty)(Focused(_))
  }
}
private[advxml] sealed trait CursorResultInstances {

  implicit val showInstanceForCursorResult: Show[CursorResult[Any]] = {
    case MissingNode(path, nodeName)        => s"($path, '$nodeName')"
    case MissingAttrByKey(path, key)        => s"($path, '$key')"
    case MissingAttrAtIndex(path, index)    => s"($path, '$index')"
    case LeftBoundLimitAttr(path, lastKey)  => s"($path, '$lastKey')"
    case RightBoundLimitAttr(path, lastKey) => s"($path, '$lastKey')"
    case r                                  => r.toString
  }
}
