package advxml.experimental

import cats.effect.{Resource, Sync}
import org.w3c.dom.{Document as JDocument, Node as JNode, NodeList}

import java.io.{ByteArrayInputStream, File, FileInputStream, InputStream}
import java.nio.charset.{Charset, StandardCharsets}
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import scala.annotation.tailrec
import scala.util.Try
import scala.xml.{Atom, Document, Elem, InputSource, MetaData, Node, NodeSeq, Null, Text, TopScope}

trait Xml
object Xml {

  import cats.implicits.*

  def fromString[F[_]: Sync](string: String, charset: Charset = StandardCharsets.UTF_8): F[Xml] =
    fromSource[F](new ByteArrayInputStream(string.getBytes(charset))).use(_.pure[F])

  def loadFile[F[_]: Sync](file: File): Resource[F, Xml] =
    fromSource[F](new FileInputStream(file))

  def loadFile[F[_]: Sync](name: String): Resource[F, Xml] =
    fromSource[F](new FileInputStream(name))

  def fromSource[F[_]](inputSource: => InputStream)(implicit F: Sync[F]): Resource[F, Xml] = {

    def parse(inputSource: InputSource): Try[Xml] = Try {

      // Parser that produces DOM object trees from XML content
      val factory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance()

      // Create DocumentBuilder with default configuration//Create DocumentBuilder with default configuration
      val builder: DocumentBuilder = factory.newDocumentBuilder

      // Parse the content to Document object
      Xml.fromJavaxDocument(builder.parse(inputSource))
    }

    Resource
      .fromAutoCloseable(F.delay(inputSource))
      .evalMap(is => F.fromTry(parse(new InputSource(is))))
  }

  // FIXME: DEPENDS ON STD XML - LOW PERFORMANCE
  def fromNodeSeq(ns: NodeSeq): XmlTree =
    ns match {
      case node: Document =>
        fromNodeSeq(node.docElem)
      case e: Text =>
        XmlTree(
          label      = e.label,
          attributes = XmlAttribute.fromMetaData(e.attributes),
          content    = Some(NodeContent.Text(XmlString.fromScalaText(e)))
        )
      case e: Elem =>
        val tree = XmlTree(
          e.label,
          XmlAttribute.fromMetaData(e.attributes)
        )

        val neChild = e.child
          .filterNot(c =>
            c.isInstanceOf[Atom[?]] && c.asInstanceOf[Atom[?]].data.toString.trim.isEmpty
          )
          .toArray
        val neChildLen = neChild.length

        val content = if (neChildLen > 0) {
          val head = neChild.head
          if (head.isAtom) {
            Some(NodeContent.text(head.asInstanceOf[Atom[?]].data.toString.trim))
          } else {

            val res: Array[XmlTree] = new Array[XmlTree](neChildLen)
            for (idx <- 0 until neChildLen) {
              res.update(idx, fromNodeSeq(neChild(idx)).withParent(tree.id))
            }

            NodeContent.childrenSeq(res.toList)
          }

        } else None

        tree.withContent(content)
    }

  def toNodeSeq(tree: XmlTree): NodeSeq = {

    @tailrec
    def rec(ls: List[XmlTree], acc: Seq[Node]): Seq[Node] =
      ls match {
        case ::(head, tail) => rec(tail, (acc :+ toNodeSeq(head)).flatten)
        case Nil            => acc
      }

    val content: Seq[Node] = tree.content match {
      case Some(NodeContent.Text(data))            => new Atom[String](data.toString)
      case Some(NodeContent.Children(childrenNel)) => rec(childrenNel.toList, Nil)
      case None                                    => Nil
    }

    Elem(
      prefix = null,
      label  = tree.label,
      attributes = tree.attributes
        .map(a => XmlAttribute.toMetaData(a))
        .foldLeft[MetaData](Null)(MetaData.concatenate),
      scope         = TopScope,
      minimizeEmpty = true,
      child         = content*
    )
  }

  // FIXME: LOW PERFORMANCE
  def fromJavaxDocument(doc: JDocument): Xml = {

    // TODO: NO STACK SAFE
    def rec(ns: JNode): XmlTree = {
      val baseNode: XmlTree = XmlTree(ns.getNodeName)
        .withAttributes(XmlAttribute.fromJavaNodeMap(ns.getAttributes))

      if (ns.hasChildNodes) {
        val childNodes: NodeList   = ns.getChildNodes
        val len: Int               = childNodes.getLength
        val result: Array[XmlTree] = new Array[XmlTree](len)
        for (i <- 0 until len) {
          result(i) = rec(childNodes.item(i))
        }

        baseNode.withChildren(result.toList.filterNot(_.label == "#text"))
      } else {
        baseNode.withText(ns.getTextContent)
      }
    }

    rec(doc.getDocumentElement)
  }
}
