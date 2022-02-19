package advxml.experimental

import cats.Show

sealed trait XmlData extends Xml with Serializable {
  override def toString: String = Show[XmlData].show(this)
}
object XmlData {
  implicit val showXmlData: Show[XmlData] = {
    case XmlByte(value)   => value.toString
    case XmlNumber(value) => value.toString
    case XmlArray(value)  => value.mkString("Array(", ", ", ")")
    case XmlString(value) => value
  }
}

case class XmlString(value: String) extends XmlData {
  def isEmpty: Boolean = value.isEmpty
}
object XmlString {
  val empty: XmlString                              = XmlString("")
  def fromScalaText(xml: scala.xml.Text): XmlString = XmlString(xml.text.trim)
}

case class XmlNumber[T <: Number](value: T) extends XmlData
case class XmlArray[T <: XmlData](value: Array[T]) extends XmlData
case class XmlByte(value: Byte) extends XmlData
