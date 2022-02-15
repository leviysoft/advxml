import advxml.experimental.codec.{Decoder, Encoder}
import advxml.experimental.cursor.NodeCursor.Root
import advxml.experimental.{NodeContent, XmlAttribute, XmlNode, XmlRootNode, XmlTree}

//############### CURSOR ###############
val node: XmlTree = XmlTree.fromNodeSeq(
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
)

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
    .attr("a")
    .focus(node)


// ############### DECODER ###############
val tree: XmlTree = XmlTree.fromNodeSeq(<Foo name="TEST" age="10">100</Foo>)

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
  XmlRootNode(
    label = "Foo",
    attributes = List(XmlAttribute("name", t.name.get), XmlAttribute("age", t.bar)),
    content = Some(NodeContent.text(t.text))
  )
})


val res1 = dec.decode(tree).toOption.map(encoder.encode).get
