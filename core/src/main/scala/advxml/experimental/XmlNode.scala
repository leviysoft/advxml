package advxml.experimental

import advxml.experimental.codec.DataEncoder
import cats.{Endo, Show}

case class XmlNode(
  label: String,
  attributes: Seq[XmlAttribute] = Nil,
  content: NodeContent          = NodeContent.empty
) extends Xml
    with LabelOps
    with AttrsOps
    with ContentOps {

  // update ops
  override def updateLabel(f: Endo[String]): XmlNode =
    copy(label = f(label))

  override def updateAttrs(f: Endo[Seq[XmlAttribute]]): XmlNode =
    copy(attributes = f(attributes))

  private[advxml] override def updateContent(f: Endo[NodeContent]): XmlNode =
    copy(content = f(content))

  override final def toString: String =
    Show[XmlNode].show(this)
}

object XmlNode extends XmlTreeInstances

private[advxml] sealed trait XmlTreeInstances {
  implicit val showXmlTree: Show[XmlNode] = XmlPrinter.prettyString(_)
}

// ######################## OPS ########################
private[advxml] sealed trait LabelOps { $this: XmlNode =>

  def updateLabel(newLabel: String): XmlNode =
    updateLabel(_ => newLabel)

  def updateLabel(f: Endo[String]): XmlNode
}

private[advxml] sealed trait AttrsOps { $this: XmlNode =>

  // ---- GET ----
  def findAttr(key: String): Option[XmlAttribute] =
    attributes.find(_.key == key)

  // ---- UPDATE ----
  def withAttributes(attrs: Seq[XmlAttribute]): XmlNode =
    updateAttrs(_ => attrs)

  def withAttributes(attr: XmlAttribute, attrs: XmlAttribute*): XmlNode =
    updateAttrs(_ => attr +: attrs)

  def withAttributesMap(values: Map[String, String]): XmlNode =
    updateAttrs(_ => XmlAttribute.fromMap(values))

  def prependAttr(newAttr: XmlAttribute): XmlNode =
    updateAttrs(ls => newAttr +: ls)

  def appendAttr(newAttr: XmlAttribute): XmlNode =
    updateAttrs(ls => ls :+ newAttr)

  def removeAttr(key: String): XmlNode =
    updateAttrs(_.filterNot(_.key == key))

  def updateAttrs(f: Endo[Seq[XmlAttribute]]): XmlNode
}

private[advxml] sealed trait ContentOps { $this: XmlNode =>

  // ------ PROPS ------
  val hasChildren: Boolean = children.nonEmpty

  val hasText: Boolean = text.nonEmpty

  val isEmpty: Boolean = content.isEmpty

  // ------ UPDATE ------
  // text
  def withText[T: DataEncoder](data: T): XmlNode =
    withContent(NodeContent.text(data))

  // child
  def withChild(child: XmlNode, children: XmlNode*): XmlNode =
    withChildren(child +: children)

  def withChildren(children: Seq[XmlNode]): XmlNode =
    withContent(NodeContent.childrenSeq(children).getOrElse(NodeContent.empty))

  // generic
  def drain: XmlNode =
    withContent(NodeContent.empty)

  private[advxml] def withContent(newContent: NodeContent): XmlNode =
    updateContent(_ => newContent)

  private[advxml] def updateContent(f: Endo[NodeContent]): XmlNode

  // ------  GET ------
  def text: Option[XmlData] =
    content.text

  def children: Seq[XmlNode] =
    content.children

  def findChild(thatLabel: String): Option[XmlNode] =
    findChildBy(_.label == thatLabel)

  def findChildBy(p: XmlNode => Boolean): Option[XmlNode] =
    children.find(p)

  def findDeepChild(thatLabel: String): Option[XmlNode] =
    deepSubNodes.find(_.label == thatLabel)

  def deepSubNodes: List[XmlNode] =
    content match {
      case NodeContent.Children(childrenNel) => childrenNel.toList.flatMap(_.deepSubNodes)
      case _                                 => List(this)
    }
}
