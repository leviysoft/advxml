import advxml.experimental.Xml
import cats.effect.IO
import cats.effect.unsafe.implicits._


case class Foo(var counter: Int)

val foo: Foo = Foo(0)

val a = Xml.fromString[IO](
  """<root>
    |    <foo>
    |      <bar>
    |        <root>
    |          <foo>
    |            <bar>
    |              <root>
    |                <foo>
    |                  <bar>
    |                    <roar a="1" b="2" c="3">TEST</roar>
    |                  </bar>
    |                </foo>
    |              </root>
    |            </bar>
    |          </foo>
    |        </root>
    |      </bar>
    |    </foo>
    |  </root>""".stripMargin
).unsafeRunSync()
def test1(foo: Foo): Unit = {
  foo.copy().counter += 1
}

def test2(foo: Foo): Unit = {
  foo.copy().counter += 2
}

test1(foo)
test2(foo)


Console.println(foo.counter)
