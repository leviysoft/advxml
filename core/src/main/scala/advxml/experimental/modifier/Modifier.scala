//package advxml.experimental.modifier
//
//import advxml.experimental.Xml
//import advxml.experimental.cursor.{Cursor, CursorResult}
//import cats.Endo
//
//sealed trait Modifier[Focus] {
//
//  val cursor: Cursor[Focus]
//
//  val modifier: Focus => ModifierResult[Focus]
//
//  def apply(xml: Xml): ModifierResult[Xml] =
//    cursor.focus(xml) match {
//      case failed: CursorResult.Failed => ModifierResult.CursorFailed(failed)
//      case focused: CursorResult.Focused[Focus] =>
//        val newValue: ModifierResult[Focus] = modifier(focused.value)
//
//        Node.fromNodeSeq(xml)
//
//        Console.println(newValue)
//        ModifierResult.Modified(xml)
//    }
//}
//object Modifier {
//
//  import cats.implicits.*
//
//  def apply[Focus](
//    cursor: Cursor[Focus],
//    modifier: Focus => ModifierResult[Focus]
//  ): Modifier[Focus] =
//    ComposableModifier(cursor, modifier)
//
//  def endo[Focus](
//    cursor: Cursor[Focus],
//    modifier: Endo[Focus]
//  ): Modifier[Focus] =
//    Modifier(cursor, modifier.andThen(ModifierResult.Modified(_)))
//
//  def const[Focus](
//    cursor: Cursor[Focus],
//    result: => ModifierResult[Focus]
//  ): Modifier[Focus] =
//    Modifier(cursor, _ => result)
//
//  def pure[Focus](
//    cursor: Cursor[Focus],
//    result: => Focus
//  ): Modifier[Focus] =
//    const(cursor, result.pure[ModifierResult])
//
//  def fail[Focus](
//    cursor: Cursor[Focus],
//    result: => ModifierResult.ModifierFailed
//  ): Modifier[Focus] =
//    const(cursor, result)
//}
//
//case class ComposableModifier[Focus](
//  cursor: Cursor[Focus],
//  modifier: Focus => ModifierResult[Focus]
//) extends Modifier[Focus]
//
//case class FinalModifier[Focus](
//  cursor: Cursor[Focus],
//  modifier: Focus => ModifierResult[Focus]
//) extends Modifier[Focus]
