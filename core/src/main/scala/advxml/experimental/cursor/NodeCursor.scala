package advxml.experimental.cursor

import advxml.experimental.XmlTree
import advxml.experimental.cursor.Cursor.CursorOp
import advxml.experimental.cursor.CursorResult.{Failed, Focused}
import cats.Show

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.language.dynamics

/** Vertical cursor for nodes
  */
sealed trait NodeCursor extends Dynamic with VCursor[XmlTree, NodeCursor] {

  def history: List[NodeCursor.Op]

  override lazy val path: String = CursorOp.buildOpsPath(history)

  // modifier
//  def append[T: Encoder](t: => T): Modifier[Node] =
//    modify(_.updateChild(_ ++ Encoder[T].encode(t)))
//
//  def prepend[T: Encoder](t: => T): Modifier[Node] =
//    modify(_.prependedAll(Encoder[T].encode(t)))
//
//  def set[T: Encoder](t: => T): Modifier[Node] =
//    modify(_ => Encoder[T].encode(t))

//  def replace[T: Encoder: Decoder](f: Endo[T]): Modifier[Xml, Xml] =
//    modify(n => f(Decoder[T].decode(n))
//
//  def rename(k: => String): Modifier[Xml, Xml] = ???
//
//  def remove: Modifier[Node] =
//    set(Xml.Empty)

  // node
  def selectDynamic(nodeName: String): NodeCursor =
    down(nodeName)

  def \(nodeName: String): NodeCursor =
    down(nodeName)

  def downPath(path: String): NodeCursor =
    path.split("\\\\").foldLeft(this)(_.down(_))

  def down(nodeName: String): NodeCursor =
    new NodeCursor.Simple(this, NodeCursor.Op.Down(nodeName))

  // content
  def attr(key: String): AttrCursor =
    new AttrCursor(this, AttrCursor.Op.SelectAttr(key))

  def attrHead: AttrCursor =
    new AttrCursor(this, AttrCursor.Op.Head)

  def attrLast: AttrCursor =
    new AttrCursor(this, AttrCursor.Op.Last)

  def text: TextCursor =
    new TextCursor(this)
}

object NodeCursor {

  sealed trait Op extends CursorOp
  object Op {

    case class Down(nodeName: String) extends Op

    implicit final val showCursorOp: Show[Op] = Show.show { case Down(nodeName) =>
      s"/$nodeName"
    }
  }

  // kind
  case object Root extends NodeCursor {

    override protected val lastCursor: NodeCursor = this

    override def history: List[Op] = Nil

    override def focus(xml: XmlTree): CursorResult[XmlTree] =
      CursorResult.Focused(xml)
  }

  class Simple(protected val lastCursor: NodeCursor, protected val lastOp: Op) extends NodeCursor {

    override def focus(ns: XmlTree): CursorResult[XmlTree] = {
      @tailrec
      def rec(
        history: List[NodeCursor.Op],
        currentPath: List[NodeCursor.Op],
        current: XmlTree
      ): CursorResult[XmlTree] = {
        history match {
          case Nil =>
            CursorResult.Focused(current)
          case op :: ops =>
            val result: CursorResult[XmlTree] = op match {
              case NodeCursor.Op.Down(nodeName) =>
                current.findChild(nodeName) match {
                  case Some(node) =>
                    CursorResult.Focused(node)
                  case None =>
                    CursorResult.MissingNode(CursorOp.buildOpsPath(currentPath), nodeName)
                }
            }

            result match {
              case Focused(node)  => rec(ops, currentPath :+ op, node)
              case failed: Failed => failed
            }
        }
      }

      rec(this.history, Nil, ns)
    }

    def history: List[Op] = {
      var next: NodeCursor = this
      val builder          = new ListBuffer[Op]

      while (!next.equals(Root)) {
        val n = next.asInstanceOf[Simple]
        n.lastOp +=: builder
        next = n.lastCursor
      }

      builder.result()
    }
  }
}
