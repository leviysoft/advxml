package advxml.experimental.cursor

import advxml.experimental.codec.Decoder
import advxml.experimental.{Xml, XmlTree}
import cats.Show

import scala.language.dynamics

sealed trait Cursor[X <: Xml] extends Serializable {

  def path: String

//  final def modify(modifier: Endo[X]): Modifier[X] =
//    Modifier.endo(this, modifier)

  def as[T: Decoder]: CursorWithDecoder[X, T] =
    new CursorWithDecoder[X, T](this)

  private[advxml] def focus(x: XmlTree): CursorResult[X]

  private[advxml] def focusOpt(x: XmlTree): CursorResult[Option[X]] =
    focus(x).fold[CursorResult[Option[X]]](x => CursorResult.Focused(Some(x))) {
      case _: CursorResult.Missing => CursorResult.Focused(None)
      case error                   => error
    }
}

object Cursor {

  trait CursorOp
  object CursorOp {

    implicit val showInstanceForCursorOp: Show[CursorOp] = Show.show {
      case op: AttrCursor.Op => Show[AttrCursor.Op].show(op)
      case op: NodeCursor.Op => Show[NodeCursor.Op].show(op)
      case op                => op.toString
    }

    def buildOpsPath(ops: List[CursorOp]): String = {
      ops.map(Show[CursorOp].show(_)).mkString("")
    }
  }
}

trait VCursor[HFocus <: Xml, +VC <: VCursor[?, VC]] extends Dynamic with Cursor[HFocus] {

  type Focus = HFocus

  protected val lastCursor: VC

  final def up: VC = lastCursor
}

trait HCursor[HFocus <: Xml, +VC <: VCursor[?, VC], +HC <: HCursor[?, ?, HC]]
    extends Cursor[HFocus] {

  type Focus = HFocus

  protected val vCursor: VC

  final def up: VC = vCursor

  def head: HC

  def last: HC

  def left: HC

  def right: HC
}
