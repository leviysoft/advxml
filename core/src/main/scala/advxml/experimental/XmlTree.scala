package advxml.experimental

import advxml.experimental.codec.DataEncoder
import cats.{Endo, Show}

sealed trait XmlTree extends Xml with LabelOps with AttrsOps with ContentOps {

  lazy val id: NodeId = NodeId.newRandom

  val label: String

  def attributes: Seq[XmlAttribute]

  def content: Option[NodeContent]

  lazy val parentIdOptional: Option[NodeId] = this match {
    case XmlRoot(_, _, _)           => None
    case XmlNode(parentId, _, _, _) => Some(parentId)
  }

  val isRoot: Boolean = this match {
    case XmlRoot(_, _, _) => true
    case _                => false
  }

  def toRoot: XmlTree =
    this match {
      case t: XmlRoot                             => t
      case XmlNode(_, label, attributes, content) => XmlRoot(label, attributes, content)
    }

  def withParent(parentId: => NodeId): XmlTree =
    this match {
      case XmlRoot(label, attributes, content) =>
        XmlNode(parentId, label, attributes, content)
      case t: XmlNode =>
        t.copy(parentId = parentId)
    }

  def newCopy: XmlTree

  override final def hashCode(): Int =
    id.value.hashCode()

  override final def toString: String =
    Show[XmlTree].show(this)
}
object XmlTree extends XmlTreeInstances {

  def apply(
    label: String,
    attributes: Seq[XmlAttribute] = Nil,
    content: Option[NodeContent]  = None,
    parentIdOpt: Option[NodeId]   = None
  ): XmlTree =
    parentIdOpt match {
      case Some(parentId) =>
        XmlNode(
          parentId,
          label,
          attributes,
          content
        )
      case None =>
        XmlRoot(
          label,
          attributes,
          content
        )
    }
}

private[advxml] sealed trait XmlTreeInstances {
  implicit val showXmlTree: Show[XmlTree] = XmlPrinter.prettyString(_)
}

case class XmlRoot(
  label: String,
  attributes: Seq[XmlAttribute]             = Nil,
  override val content: Option[NodeContent] = None
) extends XmlTree {

  override def newCopy: XmlTree = copy()

  // update ops
  override def updateLabel(f: Endo[String]): XmlTree =
    copy(label = f(label))

  override def updateAttrs(f: Endo[Seq[XmlAttribute]]): XmlTree =
    copy(attributes = f(attributes))

  override def withUpdatedContent(f: Endo[Option[NodeContent]]): XmlTree =
    copy(content = f(content))
}

case class XmlNode(
  parentId: NodeId,
  label: String,
  attributes: Seq[XmlAttribute]             = Nil,
  override val content: Option[NodeContent] = None
) extends XmlTree {

  override def newCopy: XmlTree = copy()

  // update ops
  override def updateLabel(f: Endo[String]): XmlTree =
    copy(label = f(label))

  override def updateAttrs(f: Endo[Seq[XmlAttribute]]): XmlTree =
    copy(attributes = f(attributes))

  private[advxml] override def withUpdatedContent(f: Endo[Option[NodeContent]]): XmlTree =
    copy(content = f(content))
}

// ######################## OPS ########################
private[advxml] sealed trait LabelOps { $this: XmlTree =>

  def updateLabel(newLabel: String): XmlTree =
    updateLabel(_ => newLabel)

  def updateLabel(f: Endo[String]): XmlTree
}

private[advxml] sealed trait AttrsOps { $this: XmlTree =>

  // ---- GET ----
  def findAttr(key: String): Option[XmlAttribute] =
    attributes.find(_.key == key)

  // ---- UPDATE ----
  def withAttributes(attrs: Seq[XmlAttribute]): XmlTree =
    updateAttrs(_ => attrs)

  def withAttributes(attr: XmlAttribute, attrs: XmlAttribute*): XmlTree =
    updateAttrs(_ => attr +: attrs)

  def withAttributesMap(values: Map[String, String]): XmlTree =
    updateAttrs(_ => XmlAttribute.fromMap(values))

  def prependAttr(newAttr: XmlAttribute): XmlTree =
    updateAttrs(ls => newAttr +: ls)

  def appendAttr(newAttr: XmlAttribute): XmlTree =
    updateAttrs(ls => ls :+ newAttr)

  def removeAttr(key: String): XmlTree =
    updateAttrs(_.filterNot(_.key == key))

  def updateAttrs(f: Endo[Seq[XmlAttribute]]): XmlTree
}

private[advxml] sealed trait ContentOps { $this: XmlTree =>

  // ------ PROPS ------
  val hasChildren: Boolean = children.nonEmpty

  val hasText: Boolean = text.nonEmpty

  val isEmpty: Boolean = content.isEmpty

  // ------ UPDATE ------
  // text
  def withText[T: DataEncoder](data: T): XmlTree =
    withContent(Some(NodeContent.text(data)))

  // child
  def withChild(child: XmlTree, children: XmlTree*): XmlTree =
    withChildren(child +: children)

  def withChildren(children: Seq[XmlTree]): XmlTree =
    withContent(NodeContent.childrenSeq(children))

  // generic
  def drain: XmlTree =
    withContent(None)

  def withUpdateIfPresent(f: NodeContent => Option[NodeContent]): XmlTree =
    withUpdatedContent(a => a.flatMap(f))

  private[advxml] def withContent(newContent: Option[NodeContent]): XmlTree =
    withUpdatedContent(_ => newContent)

  private[advxml] def withUpdatedContent(f: Endo[Option[NodeContent]]): XmlTree

  // ------  GET ------
  def text: Option[XmlData] =
    this.content.flatMap(_.text)

  def children: Seq[XmlTree] =
    this.content.map(_.children).getOrElse(Nil)

  def findChild(thatLabel: String): Option[XmlTree] =
    findChildBy(_.label == thatLabel)

  def findChildBy(p: XmlTree => Boolean): Option[XmlTree] =
    children.find(p)

  def findDeepChild(thatLabel: String): Option[XmlTree] =
    deepSubNodes.find(_.label == thatLabel)

  def deepSubNodes: List[XmlTree] =
    content match {
      case Some(NodeContent.Children(childrenNel)) => childrenNel.toList.flatMap(_.deepSubNodes)
      case _                                       => List(this)
    }
}
