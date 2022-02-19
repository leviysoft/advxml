package advxml.experimental

import java.util.UUID

case class NodeId(value: UUID) extends AnyVal
object NodeId {
  def newRandom: NodeId = NodeId(UUID.randomUUID())
}
