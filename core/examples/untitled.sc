import advxml.experimental.codec.Decoder
import advxml.experimental.cursor.NodeCursor.Root
import advxml.experimental.{Xml, XmlTree}

import scala.util.Try


val node: XmlTree = XmlTree.fromNodeSeq[Try](
  <root>
    <foo>
      <bar>
        <root>
          <foo>
            <bar>
              <root>
                <foo>
                  <bar>
                    <roar a="1" b="2" c="3">TEST</roar>
                  </bar>
                </foo>
              </root>
            </bar>
          </foo>
        </root>
      </bar>
    </foo>
  </root>
).get

val result1 =
  Root
    .down("foo")
    .down("bar")
    .down("root")
    .down("foo")
    .down("bar")
    .down("root")
    .down("foo")
    .down("bar")
    .down("roar")
    .attr("ABC")
    .focus(node)

case class Foo(name: String, age: Int)
val dec: Decoder[Foo] =
  Decoder.fromCursor(c =>
    for {
      foo <- c.attr("name").as[String]
      bar <- c.attr("age").as[Int]
    } yield Foo(foo, bar)
  )

dec.decode(
  XmlTree.fromNodeSeq[Try](<Foo name="TEST" age="10"/>).get
)
