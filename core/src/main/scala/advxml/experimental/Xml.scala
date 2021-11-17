package advxml.experimental

import cats.effect.{Resource, Sync}
import cats.MonadThrow

import java.io.{ByteArrayInputStream, File, FileInputStream, InputStream}
import java.nio.charset.{Charset, StandardCharsets}
import scala.xml.{InputSource, NodeSeq, XML}

trait Xml
object Xml {

  import cats.implicits.*

  def fromString[F[_]: Sync](string: String, charset: Charset = StandardCharsets.UTF_8): F[Xml] =
    fromSource[F](new ByteArrayInputStream(string.getBytes(charset))).use(_.pure[F])

  def loadFile[F[_]: Sync](file: File): Resource[F, Xml] =
    fromSource[F](new FileInputStream(file))

  def loadFile[F[_]: Sync](name: String): Resource[F, Xml] =
    fromSource[F](new FileInputStream(name))

  def fromSource[F[_]](inputSource: => InputStream)(implicit F: Sync[F]): Resource[F, Xml] =
    Resource
      .fromAutoCloseable(F.delay(inputSource))
      .evalMap(is => F.delay(XML.loadXML(new InputSource(is), XML.parser)))
      .evalMap(fromNodeSeq[F](_))

  def fromNodeSeq[F[_]: MonadThrow](ns: NodeSeq): F[Xml] =
    XmlTree.fromNodeSeq[F](ns).widen[Xml]
}
