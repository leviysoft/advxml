import advxml.experimental.{Xml, XmlNode}
import advxml.experimental.codec.{Decoder, Encoder}
import advxml.experimental.cursor.CursorResult
import advxml.experimental.cursor.NodeCursor.Root
import advxml.experimental.XmlAttribute.XmlAttrStringOps

//############### PARSING from NODESEQ ###############
val n1: XmlNode = Xml.fromNodeSeq(<root>TEST</root>)

//############### CURSOR ###############
val node: XmlNode = Xml.fromNodeSeq(
  <root>
    <foo>
      <bar>
        <root>
          <foo>
            <bar>
              <root>
                <foo>
                  <bar>
                    <roar a="1" b="2" c="3">
                      LOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREA LOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREALOREA
                    </roar>
                  </bar>
                </foo>
              </root>
            </bar>
          </foo>
        </root>
      </bar>
    </foo>
  </root>
)
node.findDeepChild("roar")
Xml.toNodeSeq(node)

val result1: CursorResult[Int] =
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
    .attr("a")
    .as[Int]
    .focus(node)


val result1: CursorResult[Int] =
  (Root \ "foo" \ "bar" \ "root" \ "foo"
    \ "bar" \ "root" \ "foo" \ "bar"
    \ "roar" attr "a").as[Int]
    .focus(node)

// ############### DECODER ###############
val tree: XmlNode = Xml.fromNodeSeq(<Foo name="TEST" age="10">100</Foo>)


val ressa = tree.findChild("foo")


case class Foo(name: Option[String], bar: Int, text: Int)
val dec: Decoder[Foo] =
  Decoder.fromCursor(c =>
    for {
      foo <- c.attr("name").as[Option[String]]
      bar <- c.attr("age").as[Int]
      text <- c.text.as[Int]
    } yield Foo(foo, bar, text)
  )

val result: Decoder.Result[Foo] = dec.decode(tree) //Valid(Foo(None,10))

//############### ENCODER ###############


val encoder: Encoder[Foo] = Encoder.of(t => {
  XmlNode("Foo")
    .withAttributes(
      "name" := t.name.get,
      "age"  := t.bar
    )
    .withText(t.text)
})


val res1 = dec.decode(tree).toOption.map(encoder.encode).get
