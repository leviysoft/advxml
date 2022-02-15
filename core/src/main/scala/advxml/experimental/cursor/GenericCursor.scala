package advxml.experimental.cursor

trait GenericCursor[A, +B] {
  def focus(x: A): CursorResult[B]
}
