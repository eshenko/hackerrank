object ListsAndGCD {

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution */
    import scala.io.StdIn
    import scala.collection.immutable.TreeMap

    val n = StdIn.readInt
    val inputs = (1 to n)
      .map(_ => StdIn.readLine
        .split(" ")
        .map(_.toInt)
        .grouped(2)
        .map(arr => (arr(0), arr(1)))
        .to(TreeMap))

    val primes = inputs(0).keys.filter(x => inputs.forall(_.contains(x)))
    val pairs = primes.map(prime => prime -> inputs.map(_(prime)).min).to(TreeMap)

    pairs.foreach(pair => print(pair._1 + " " +  pair._2 + " "))
  }
}
