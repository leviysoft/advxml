package advxml.traverse

import advxml.experimental.Node
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class NodeTest extends AnyFunSuite with Matchers {

  test("Create root node") {

    val n: Node = Node.fromNodeSeq(<root></root>)

    n === Root(
      label      = "root",
      attributes = Nil,
      content    = _ => NodeContent.Empty
    )
  }

  test("Create root node with text content and attrs") {

    val n: Node = Node.fromNodeSeq(<root a="1" b="2">TEXT</root>)

    n === Root(
      label      = "root",
      attributes = Nil,
      content    = _ => NodeContent.WithText(Text("TEXT"))
    )
  }

  test("Create node nested") {

    val n: Node = Node.fromNodeSeq(<root a="1" b="2"><foo><bar>TEXT</bar></foo></root>)

    n === Node("root")
      .withAttrs(Attr("a", "1"), Attr("b", "2"))
      .withChild(
        Node("foo")
          .withChild(
            Node("bar").withText(Text("TEXT")).apply(_)
          )
          .apply(_)
      )
  }
}
