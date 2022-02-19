package advxml.experimental

import advxml.experimental.XmlAttribute.XmlAttrStringOps

class XmlPrinterSuite extends munit.FunSuite {

  test("XmlPrinter.prettyString convert xml tree to well formatted XML string") {

    val tree: XmlTree = XmlTree("Foo").withChild(
      XmlTree("Bar")
        .withAttributes("F" := 'A')
        .withChild(
          XmlTree("Test")
            .withAttributes("G" := 100L)
            .withChild(
              XmlTree("Node")
                .withAttributes("A" := 10, "B" := true)
                .withText("Lorem ipsum dolor sit amet")
            )
        )
    )

    // assert
    assertEquals(
      obtained = XmlPrinter.prettyString(tree),
      expected = """|<Foo>
                    | <Bar F="A">
                    |  <Test G="100">
                    |   <Node A="10" B="true">Lorem ipsum dolor sit amet</Node>
                    |  </Test>
                    | </Bar>
                    |</Foo>""".stripMargin
    )
  }

  test("XmlPrinter.stringify convert xml tree to un-formatted string") {

    val tree: XmlTree = XmlTree("Foo").withChild(
      XmlTree("Bar")
        .withAttributes("F" := 'A')
        .withChild(
          XmlTree("Test")
            .withAttributes("G" := 100L)
            .withChild(
              XmlTree("Node")
                .withAttributes("A" := 10, "B" := true)
                .withText("Lorem ipsum dolor sit amet")
            )
        )
    )

    // assert
    assertEquals(
      obtained = XmlPrinter.stringify(tree),
      expected =
        """<Foo><Bar F="A"><Test G="100"><Node A="10" B="true">Lorem ipsum dolor sit amet</Node></Test></Bar></Foo>""".stripMargin
    )
  }
}
