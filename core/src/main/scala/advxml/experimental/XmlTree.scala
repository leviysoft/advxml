package advxml.experimental

import cats.{Endo, MonadThrow, Show}

import scala.annotation.tailrec
import scala.xml.*

sealed trait XmlTree extends Xml with LabelOps with AttrsOps with ChildrenFindOps {

  type SelfType <: XmlTree

  val label: String

  val attributes: List[XmlAttribute]

  private[advxml] val childrenOptional: Option[List[XmlNodeWithParent]] =
    this match {
      case node: XmlRootNode => Some(node.children)
      case node: XmlNode     => Some(node.children)
      case _: XmlTextNode    => None
    }

  private[advxml] val textOptional: Option[XmlData] = this match {
    case node: XmlTextNode => node.text
    case _                 => None
  }

  private[advxml] val parentOptional: Option[XmlNodeWithChildren] = this match {
    case node: XmlNodeWithParent => Some(node.parent)
    case _                       => None
  }

  val isRoot: Boolean = this match {
    case _: XmlRootNode => true
    case _              => false
  }

  override final def toString: String =
    Show[XmlTree].show(this)
}
object XmlTree extends XmlTreeSyntax with XmlTreeInstances {

  import cats.implicits.*

  @tailrec
  def fromNodeSeq[F[_]](ns: NodeSeq)(implicit F: MonadThrow[F]): F[XmlTree] = {

    def buildEmptyParent(n: Node): XmlRootNode =
      XmlRootNode(n.label, XmlAttribute.fromMetaData(n.attributes))

    def rec(
      parent: XmlNodeWithChildren,
      leftNs: List[Node],
      processed: List[XmlNodeWithParent] = Nil
    ): F[XmlTree] =
      leftNs.filterNot(_.label.startsWith("#PCDATA")) match {
        case Nil => F.pure(parent.updateChildren(_ ++ processed))
        case x :: xs =>
          x match {
            // NODES
            case node: Elem =>
              rec(
                parent = buildEmptyParent(node).toNode(parent),
                leftNs = node.nonEmptyChildren.toList
              ).flatMap(subNode => {
                rec(
                  parent    = parent,
                  leftNs    = xs,
                  processed = processed :+ subNode.asInstanceOf[XmlNodeWithParent]
                )
              })

            // TEXT
            case node: Text =>
              rec(
                parent = parent,
                leftNs = xs,
                processed = processed :+ XmlTextNode(
                  parent,
                  node.label,
                  XmlAttribute.fromMetaData(node.attributes),
                  Some(XmlString.fromScalaText(node))
                )
              )
            case _ => F.raiseError(new UnsupportedOperationException("Unsupported yet."))
          }
      }

    ns match {
      case node: Document =>
        XmlTree.fromNodeSeq[F](node.docElem)
      case node: Node =>
        rec(
          buildEmptyParent(node),
          node.nonEmptyChildren.toList
        )
      case _ =>
        F.raiseError(
          new UnsupportedOperationException("Unsupported yet.")
        )
    }
  }
}
private[advxml] sealed trait XmlTreeSyntax {
  implicit class XmlTreeSyntax(tree: XmlTree) {

    val children: List[XmlNodeWithParent] =
      tree.childrenOptional.getOrElse(Nil)

    val text: Option[XmlData] =
      tree.textOptional

    val parent: Option[XmlNodeWithChildren] =
      tree.parentOptional
  }
}
private[advxml] sealed trait XmlTreeInstances {

  implicit val showXmlTree: Show[XmlTree] =
    new Show[XmlTree] {

      private def build(xt: XmlTree)(content: Option[String]): String =
        (content match {
          case None =>
            s"<${xt.label} ${xt.attributes.map(XmlAttribute.stringify).mkString(" ")}/>"
          case Some(cont) =>
            s"<${xt.label} ${xt.attributes.map(XmlAttribute.stringify).mkString(" ")}>$cont</${xt.label}>"
        }).replace(s"${xt.label} >", s"${xt.label}>")

      override def show(t: XmlTree): String =
        t match {
          case node: XmlRootNode =>
            build(node)(node.children match {
              case Nil => None
              case ls  => Some(ls.mkString("\n"))
            })
          case node: XmlNode =>
            build(node)(node.children match {
              case Nil => None
              case ls  => Some(ls.mkString("\t\n"))
            })
          case node: XmlTextNode =>
            build(node)(node.text match {
              case None       => None
              case Some(text) => Some(text.toString)
            })
        }
    }
}

sealed trait XmlNodeWithChildren extends XmlTree with ChildrenOps {
  type SelfType <: XmlNodeWithChildren
  def children: List[XmlNodeWithParent]
}

sealed trait XmlNodeWithParent extends XmlTree with ParentOps {
  override type SelfType <: XmlNodeWithParent
  def parent: XmlNodeWithChildren
}

case class XmlRootNode(
  label: String,
  attributes: List[XmlAttribute]    = Nil,
  children: List[XmlNodeWithParent] = Nil
) extends XmlNodeWithChildren {

  override type SelfType = XmlRootNode

  def toNode(parent: XmlNodeWithChildren): XmlNode =
    XmlNode(parent, label, attributes, children)

  // update ops
  override def updateChildren(f: Endo[List[XmlNodeWithParent]]): XmlRootNode =
    copy(children = f(children))

  override def updateLabel(f: Endo[String]): XmlRootNode =
    copy(label = f(label))

  override def updateAttrs(f: Endo[List[XmlAttribute]]): XmlRootNode =
    copy(attributes = f(attributes))
}

case class XmlNode(
  private val _parent: XmlNodeWithChildren,
  label: String,
  attributes: List[XmlAttribute]    = Nil,
  children: List[XmlNodeWithParent] = Nil
) extends XmlNodeWithChildren
    with XmlNodeWithParent {

  override type SelfType = XmlNode

  // parent ops
  override def parent: XmlNodeWithChildren = _parent

  // update ops
  override def updateChildren(f: Endo[List[XmlNodeWithParent]]): XmlNode =
    copy(children = f(children))

  override def updateLabel(f: Endo[String]): XmlNode =
    copy(label = f(label))

  override def updateAttrs(f: Endo[List[XmlAttribute]]): XmlNode =
    copy(attributes = f(attributes))
}

case class XmlTextNode(
  private val _parent: XmlNodeWithChildren,
  label: String,
  attributes: List[XmlAttribute] = Nil,
  text: Option[XmlData]          = None
) extends XmlTree
    with XmlNodeWithParent
    with TextOps {

  override type SelfType = XmlTextNode

  // parent ops
  override def parent: XmlNodeWithChildren = _parent

  // update ops
  override def updateLabel(f: Endo[String]): XmlTextNode =
    copy(label = f(label))

  override def updateAttrs(f: Endo[List[XmlAttribute]]): XmlTextNode =
    copy(attributes = f(attributes))

  override def updateText(f: Endo[Option[XmlData]]): XmlTextNode =
    copy(text = f(text))
}

// ######################## OPS ########################
private[advxml] sealed trait ParentOps { $this: XmlTree =>

  final def toRoot: XmlRootNode =
    XmlRootNode(label, attributes, childrenOptional.getOrElse(Nil))
}

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

private[advxml] sealed trait ChildrenOps extends ChildrenFindOps { $this: XmlTree =>

  def appendChild(node: XmlNode): SelfType =
    updateChildren(ls => ls :+ node)

  def prependChild(node: XmlNode): SelfType =
    updateChildren(ls => node +: ls)

  def updateChildren(f: Endo[List[XmlNodeWithParent]]): SelfType
}
private[advxml] sealed trait ChildrenFindOps { $this: XmlTree =>

  def findChild(label: String): Option[XmlNodeWithParent] =
    findChildBy(_.label == label)

  def findChildBy(p: XmlNodeWithParent => Boolean): Option[XmlNodeWithParent] =
    childrenOptional.getOrElse(Nil).find(p)
}

private[advxml] sealed trait TextOps { $this: XmlTextNode =>

  // text ops
  def textOrEmpty: XmlData =
    textOptional.getOrElse(XmlString.empty)

  def setText(text: XmlData): XmlTextNode =
    updateText(_ => Some(text))

  def removeText(): XmlTextNode =
    updateText(_ => None)

  def updateTextIfPresent(f: Endo[XmlData]): XmlTextNode =
    updateText(_.map(f))

  def updateText(f: Endo[Option[XmlData]]): XmlTextNode
}
