package advxml.experimental.cursor

import advxml.experimental.{XmlData, XmlTree}

/** Vertical cursor for node Text
  */
class TextCursor(protected val lastCursor: NodeCursor) extends VCursor[XmlData, NodeCursor] {

  override lazy val path: String = s"${lastCursor.path} | text"

//  // modifier
//  def clean: Modifier[XmlString] =
//    set(XmlString.empty)
//
//  def set(v: => XmlString): Modifier[XmlString] =
//    modify(_ => v)

  // focus
  private[advxml] override def focus(node: XmlTree): CursorResult[XmlData] =
    focusOpt(node).flatMap {
      case Some(value: XmlData) => CursorResult.Focused(value)
      case None                 => CursorResult.MissingText(lastCursor.path)
    }
}
