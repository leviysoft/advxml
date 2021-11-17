package advxml.utils

import advxml.experimental.XmlAttribute

import scala.xml.{Elem, Node, NodeSeq, Text}

//TODO: Create a syntax for these methods ?
object XmlUtils {

  val prettyPrinter = new scala.xml.PrettyPrinter(80, 4)

  def prettyPrint(xml: NodeSeq): String =
    prettyPrinter.formatNodes(xml)

  def nodeToElem(n: Node): Elem =
    Elem(null, n.label, n.attributes, n.scope, false, n.child*)

  def flatMapChildren(e: Elem, f: Node => NodeSeq): Elem = {
    val updatedChildren: Seq[Node] = e.child.filterNot(emptyText).flatMap(f)

    e.copy(child = updatedChildren)
  }

  // TODO: To RENAME
  def emptyText(n: Node): Boolean = n match {
    case t: Text => t.text.trim.isEmpty
    case _       => false
  }

  // TODO: TO TEST
  def isEmpty(ns: NodeSeq): Boolean = ns match {
    case x if x.isEmpty          => true
    case x: Text if emptyText(x) => true
    case _                       => false
  }

  // TODO: TO TEST
  def optionEmpty[T <: NodeSeq](ns: T): Option[T] =
    if (isEmpty(ns))
      None
    else
      Some(ns)

  // TODO: TO TEST
  def optionText(ns: NodeSeq): Option[String] =
    ns.text match {
      case x if x.isEmpty => None
      case x              => Some(x)
    }

  // TODO: TO TEST
  def findAttr(ns: NodeSeq, k: String): Option[XmlAttribute] =
    ns.headOption
      .flatMap(_.attributes.find(_.key.equals(k)))
      .flatMap(XmlAttribute.fromMetaData(_).headOption)

  // TODO: TO TEST
  def attrByIndex(ns: NodeSeq, index: Int): Option[XmlAttribute] =
    ns.headOption
      .flatMap(_.attributes.take(index).lastOption)
      .flatMap(XmlAttribute.fromMetaData(_).headOption)

  // TODO: TO TEST
  def headAttr(ns: NodeSeq): Option[XmlAttribute] =
    ns.headOption
      .flatMap(_.attributes.headOption)
      .flatMap(XmlAttribute.fromMetaData(_).headOption)

  // TODO: TO TEST
  def lastAttr(ns: NodeSeq): Option[XmlAttribute] =
    ns.headOption
      .flatMap(_.attributes.lastOption)
      .flatMap(XmlAttribute.fromMetaData(_).headOption)
}
