package advxml.experimental

import advxml.experimental.codec.DataEncoder
import cats.data.NonEmptyList

/** Coproduct to define XML node content
  */
sealed trait NodeContent {

  def drain(): Option[NodeContent] = None

  def text: Option[XmlData] =
    this match {
      case NodeContent.Text(data)  => Some(data)
      case NodeContent.Children(_) => None
    }

  def children: List[XmlTree] = this match {
    case NodeContent.Text(_)            => Nil
    case NodeContent.Children(children) => children.toList
  }
}
object NodeContent {

  def text[T: DataEncoder](data: T): NodeContent =
    Text(DataEncoder[T].encode(data))

  def childrenSeq(childrenLs: Seq[XmlTree]): Option[NodeContent] =
    NonEmptyList.fromList(childrenLs.toList).map(children)

  def children(node: XmlTree, nodes: XmlTree*): NodeContent =
    Children(NonEmptyList.of(node, nodes*))

  def children(childrenNel: NonEmptyList[XmlTree]): NodeContent =
    Children(childrenNel)

  case class Text(data: XmlData) extends NodeContent
  case class Children(childrenNel: NonEmptyList[XmlTree]) extends NodeContent
}
