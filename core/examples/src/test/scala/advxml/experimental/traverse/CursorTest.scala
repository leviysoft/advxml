//package advxml.core.traverse
//
//import org.scalatest.funsuite.AnyFunSuite
//import org.scalatest.matchers.should.Matchers
//
//class CursorTest extends AnyFunSuite with Matchers {
//
//  test("Cursor.downNode") {
//
//    val result: CursorResult = Root
//      .downNode("foo")
//      .downNode("bar")
//      .downNode("roar")
//      .focus(<root>
//        <foo>
//          <bar>
//            <roar>TEST</roar>
//          </bar>
//        </foo>
//      </root>)
//
//    result shouldBe CursorResult.Focused(<roar>TEST</roar>)
//  }
//}
