package advxml.experimental

import cats.{Endo, Show}
import cats.data.NonEmptyList

import scala.annotation.tailrec
import scala.xml.*

sealed trait XmlTree extends Xml with LabelOps with AttrsOps with ContentOps {

  type SelfType <: XmlTree

  val label: String

  val attributes: List[XmlAttribute]

  val content: Option[NodeContent] = None

  val parentOptional: Option[XmlTree] = this match {
    case XmlRootNode(_, _, _)     => None
    case XmlNode(parent, _, _, _) => Some(parent)
  }

  val isRoot: Boolean = parentOptional.isEmpty

  final def toRoot: XmlRootNode =
    this match {
      case t: XmlRootNode                         => t
      case XmlNode(_, label, attributes, content) => XmlRootNode(label, attributes, content)
    }

  def toNode(parent: XmlTree): XmlNode = {
    this match {
      case XmlRootNode(label, attributes, content) => XmlNode(parent, label, attributes, content)
      case t: XmlNode                              => t.copy(parent = parent)
    }
  }

  override final def toString: String =
    Show[XmlTree].show(this)
}
object XmlTree extends XmlTreeInstances {

  @tailrec
  def fromNodeSeq[F[_]](ns: NodeSeq): XmlTree = {

    def buildEmptyParent(n: Node): XmlRootNode =
      XmlRootNode(n.label, XmlAttribute.fromMetaData(n.attributes))

    // indirection needed to let rec be tail recursive
    def recall(parent: XmlTree, node: Elem): XmlTree =
      rec(
        parent = buildEmptyParent(node).toNode(parent),
        leftNs = node.nonEmptyChildren.toList
      )

    @tailrec
    def rec(
      parent: XmlTree,
      leftNs: List[Node],
      processed: List[XmlTree] = Nil
    ): XmlTree =
      leftNs match {
        case Nil =>
          parent.updateIfPresent { _ =>
            processed match {
              case Nil => parent.content
              case ls =>
                NonEmptyList
                  .fromList(ls)
                  .map(_.map(_.asInstanceOf[XmlNode]))
                  .map(NodeContent.children)
            }
          }
        case x :: xs =>
          x match {

            // PCDATA
            case atomNode: Atom[String] =>
              processed.lastOption match {
                case Some(last) =>
                  rec(
                    parent    = parent,
                    leftNs    = xs,
                    processed = processed.dropRight(1) :+ last.set(NodeContent.text(atomNode.data))
                  )
                case None =>
                  rec(
                    parent    = parent.set(NodeContent.text(atomNode.data)),
                    leftNs    = xs,
                    processed = Nil
                  )
              }

            // TEXT
            case txtNode: Text =>
              rec(
                parent = parent,
                leftNs = xs,
                processed = processed :+ XmlNode(
                  parent,
                  txtNode.label,
                  XmlAttribute.fromMetaData(txtNode.attributes),
                  Some(NodeContent.Text(XmlString.fromScalaText(txtNode)))
                )
              )

            // NODES
            case node: Elem =>
              rec(
                parent    = parent,
                leftNs    = xs,
                processed = processed :+ recall(parent, node)
              )
          }
      }

    ns match {
      case node: Document =>
        XmlTree.fromNodeSeq(node.docElem)
      case node: Node =>
        rec(
          buildEmptyParent(node),
          node.nonEmptyChildren.toList
        )
    }
  }
}

private[advxml] sealed trait XmlTreeInstances {

  // TODO
  implicit val showXmlTree: Show[XmlTree] =
    new Show[XmlTree] {

      private def build(xt: XmlTree, content: Option[String]): String =
        (content match {
          case None =>
            s"<${xt.label} ${xt.attributes.map(XmlAttribute.stringify).mkString(" ")}/>"
          case Some(cont) =>
            s"<${xt.label} ${xt.attributes.map(XmlAttribute.stringify).mkString(" ")}>$cont</${xt.label}>"
        }).replace(s"${xt.label} >", s"${xt.label}>")

      override def show(t: XmlTree): String = {

        def rec(t: XmlTree, stringBuilder: StringBuilder): String =
          t.content match {
            case None =>
              stringBuilder.append(build(t, None)).toString()
            case Some(NodeContent.Text(data)) =>
              stringBuilder.append(build(t, Some(data.toString))).toString()
            case Some(NodeContent.Children(childrenNel)) =>
              build(
                t,
                Some(
                  childrenNel
                    .map(n => {
                      rec(n, new StringBuilder)
                    })
                    .toList
                    .mkString("\t\n")
                )
              )
          }

        rec(t, new StringBuilder())
      }
    }
}

case class XmlRootNode(
  label: String,
  attributes: List[XmlAttribute]            = Nil,
  override val content: Option[NodeContent] = None
) extends XmlTree {

  override type SelfType = XmlRootNode

  // update ops
  override def updateLabel(f: Endo[String]): XmlRootNode =
    copy(label = f(label))

  override def updateAttrs(f: Endo[List[XmlAttribute]]): XmlRootNode =
    copy(attributes = f(attributes))

  override def update(f: Endo[Option[NodeContent]]): XmlRootNode =
    copy(content = f(content))
}

case class XmlNode(
  parent: XmlTree,
  label: String,
  attributes: List[XmlAttribute]            = Nil,
  override val content: Option[NodeContent] = None
) extends XmlTree {

  override type SelfType = XmlNode

  // update ops
  override def updateLabel(f: Endo[String]): XmlNode =
    copy(label = f(label))

  override def updateAttrs(f: Endo[List[XmlAttribute]]): XmlNode =
    copy(attributes = f(attributes))

  override def update(f: Endo[Option[NodeContent]]): XmlNode =
    copy(content = f(content))
}

// ######################## OPS ########################
private[advxml] sealed trait LabelOps { $this: XmlTree =>

  def updateLabel(newLabel: String): SelfType =
    updateLabel(_ => newLabel)

  def updateLabel(f: Endo[String]): SelfType
}

private[advxml] sealed trait AttrsOps { $this: XmlTree =>

  def prependAttr(key: String, value: XmlData): SelfType =
    prependAttr(XmlAttribute(key, value))

  def prependAttr(newAttr: XmlAttribute): SelfType =
    updateAttrs(ls => newAttr +: ls)

  def appendAttr(key: String, value: XmlData): SelfType =
    appendAttr(XmlAttribute(key, value))

  def appendAttr(newAttr: XmlAttribute): SelfType =
    updateAttrs(ls => ls :+ newAttr)

  def removeAttr(key: String): SelfType =
    updateAttrs(_.filterNot(_.key == key))

  def updateAttr(key: String, f: XmlData => XmlData): SelfType =
    updateAttrs(_.map(a => if (a.key == key) a.copy(value = f(a.value)) else a))

  def updateAttrs(f: Endo[List[XmlAttribute]]): SelfType

  def findAttr(key: String): Option[XmlAttribute] =
    attributes.find(_.key == key)
}

private[advxml] sealed trait ContentOps { $this: XmlTree =>

  def update(f: Endo[Option[NodeContent]]): SelfType

  def set(content: Option[NodeContent]): SelfType =
    update(_ => content)

  def set(content: NodeContent): SelfType =
    set(Some(content))

  def drain: SelfType =
    set(None)

  def updateIfPresent(f: NodeContent => Option[NodeContent]): SelfType =
    update(a => a.flatMap(f))

  def text: Option[XmlData] =
    this.content.flatMap(_.text)

  def children: List[XmlNode] =
    this.content.map(_.children).getOrElse(Nil)

  val hasChildren: Boolean = children.nonEmpty

  val hasText: Boolean = text.nonEmpty

  val isEmpty: Boolean = content.isEmpty
}
