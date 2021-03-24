import scala.io.StdIn

object PascalTriangle extends App {
  val input = StdIn.readInt()

  for (n <- 0 until input) {
    for (r <- 0 to n) print(factorial(n)/(factorial(r) * factorial(n-r)) + " ")
    println()
  }

  def factorial(number: Int) = (1 to number).product
}
