import advxml.experimental.Xml
import cats.effect.IO
import cats.effect.unsafe.implicits._

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


