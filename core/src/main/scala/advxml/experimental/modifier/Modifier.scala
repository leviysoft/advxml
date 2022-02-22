package advxml.experimental.modifier

import advxml.experimental.XmlNode
import advxml.experimental.cursor.{CursorResult, NodeCursor}
import cats.{Endo, Monoid}

/** Create a modified copy of input [[XmlNode]]
  */
sealed trait Modifier {
  def modify(node: XmlNode): ModifierResult[XmlNode]
}

object Modifier extends ModifierInstances {

  def apply(
    f: XmlNode => ModifierResult[XmlNode]
  ): Modifier = {
    new Modifier {
      override def modify(node: XmlNode): ModifierResult[XmlNode] = f(node)
    }
  }

  def apply(
    cursor: NodeCursor,
    modifier: Endo[XmlNode]
  ): Modifier =
    Modifier(node => {
      val nodeClone = node.copy()
      cursor.focus(nodeClone) match {
        case failed: CursorResult.Failed => ModifierResult.CursorFailed(failed)
        case CursorResult.Focused(focus) =>
          focus.mute(modifier)
          ModifierResult.Modified(focus)
      }
    })

  val id: Modifier = Modifier(ModifierResult.pure)

  def const(
    result: => ModifierResult[XmlNode]
  ): Modifier =
    Modifier(_ => result)

  def fail(
    result: => ModifierResult.ModifierFailed
  ): Modifier =
    const(result)
}

sealed trait ModifierInstances {

  implicit val monoidForModifier: Monoid[Modifier] = new Monoid[Modifier] {
    override def empty: Modifier = Modifier.id
    override def combine(x: Modifier, y: Modifier): Modifier = Modifier(node => {
      x.modify(node) match {
        case ModifierResult.Modified(value)        => y.modify(value)
        case failed: ModifierResult.ModifierFailed => failed
      }
    })
  }
}
