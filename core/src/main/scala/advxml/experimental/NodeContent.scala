package advxml.experimental

import advxml.experimental.codec.DataEncoder
import cats.Traverse
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

  def children: List[XmlNode] = this match {
    case NodeContent.Text(_)            => Nil
    case NodeContent.Children(children) => children.toList
  }

  // text ops
  def setText(text: XmlData): Option[NodeContent] =
    updateText(_ => Some(text))

  def updateText(f: XmlData => Option[XmlData]): Option[NodeContent]

  // children ops
  def appendChild(node: XmlNode): Option[NodeContent] =
    updateChildren(_.append(node))

  def prependChild(node: XmlNode): Option[NodeContent] =
    updateChildren(_.prepend(node))

  def findChild(label: String): Option[XmlNode] =
    findChildBy(_.label == label)

  def findChildBy(p: XmlNode => Boolean): Option[XmlNode] =
    children.find(p)

  def toChildren(ls: NonEmptyList[XmlNode]): NodeContent = this match {
    case NodeContent.Text(_)               => NodeContent.Children(ls)
    case NodeContent.Children(childrenNel) => NodeContent.Children(childrenNel.concatNel(ls))
  }

  def updateChildren[F[_]: Traverse](
    f: NonEmptyList[XmlNode] => F[XmlNode]
  ): Option[NodeContent]
}
object NodeContent {

  import cats.implicits.*

  def text[T: DataEncoder](data: T): NodeContent =
    Text(DataEncoder[T].encode(data))

  def children(node: XmlNode, nodes: XmlNode*): NodeContent =
    Children(NonEmptyList.of(node, nodes*))

  def children(childrenNel: NonEmptyList[XmlNode]): NodeContent =
    Children(childrenNel)

  case class Text(data: XmlData) extends NodeContent {
    override def updateText(f: XmlData => Option[XmlData]): Option[NodeContent] =
      f(data).map(Text)

    override def updateChildren[F[_]: Traverse](
      f: NonEmptyList[XmlNode] => F[XmlNode]
    ): Option[NodeContent] = Some(this)
  }

  case class Children(childrenNel: NonEmptyList[XmlNode]) extends NodeContent {

    override def updateText(f: XmlData => Option[XmlData]): Option[NodeContent] =
      Some(this)

    override def updateChildren[F[_]: Traverse](
      f: NonEmptyList[XmlNode] => F[XmlNode]
    ): Option[NodeContent] =
      NonEmptyList.fromList(f(childrenNel).toList) match {
        case Some(ls) => Some(Children(ls))
        case None     => None
      }
  }
}
