package advxml.experimental

import advxml.experimental.codec.DataEncoder
import cats.{Eq, Show}

import scala.xml.MetaData

case class XmlAttribute(key: String, value: XmlData) extends Xml with Serializable {

  override def toString: String = XmlAttribute.stringify(this)

  override def equals(obj: Any): Boolean =
    obj != null
      && obj.isInstanceOf[XmlAttribute]
      && Eq[XmlAttribute].eqv(this, obj.asInstanceOf[XmlAttribute])
}
object XmlAttribute extends XmlAttributeInstances {

  def apply[T: DataEncoder](key: String, value: T): XmlAttribute =
    XmlAttribute(key, DataEncoder[T].encode(value))

  def fromTuple(t: (String, XmlData)): XmlAttribute =
    XmlAttribute(t._1, t._2)

  def fromMetaData(metaData: MetaData): List[XmlAttribute] =
    metaData.iterator.map(m => XmlAttribute(m.key, XmlString(m.value.text))).toList

  def stringify(ls: XmlAttribute): String =
    s"${ls.key}=\"${ls.value}\""
}
private[advxml] sealed trait XmlAttributeInstances {

  implicit def showInstanceForAttr(implicit sd: Show[XmlData]): Show[XmlAttribute] =
    attr => XmlAttribute.stringify(attr)

  implicit val eqInstanceForAttr: Eq[XmlAttribute] = (x: XmlAttribute, y: XmlAttribute) =>
    x.key == y.key && x.value == y.value
}
