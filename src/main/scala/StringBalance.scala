/**
 * Given a string containing '(', ')', '{', '}', '[' and ']',
 * check string is balanced. To be balanced we need:
 *  - for every open type of bracket have closed one with the same type
 *  - open-closed should be in correct order
 *  balanced: «[](){}», «[({})]», «»
 *  not balanced: «[}», «[}]», «)(»
 */

object StringBalance extends App{

  import scala.io.StdIn

  val input = StdIn.readLine

  def isBalanced(str: String): Boolean = {
    import scala.annotation.tailrec
    val balanceMap = Map(
      ')' -> '(',
      '}' -> '{',
      ']' -> '['
    )

    val openedBraces = balanceMap.values.toSeq
    val closedBraces = balanceMap.keys.toSeq

    @tailrec
    def inner(input: List[Char], acc: List[Char]): Boolean = input match {
      case list if list.isEmpty => acc.isEmpty
      case closed :: _ if closedBraces.contains(closed) && (acc.head != balanceMap(closed) || acc.isEmpty) => false
      case opened :: tail if openedBraces.contains(opened) => inner(tail, opened :: acc)
      case closed :: tail if closedBraces.contains(closed) && acc.head == balanceMap(closed) => inner(tail, acc.tail)
      case _ :: tail => inner(tail, acc)
    }

    inner(str.toList, Nil)
  }

  println(isBalanced(input))
}
