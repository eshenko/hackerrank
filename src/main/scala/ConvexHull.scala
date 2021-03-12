object ConvexHull {

  import scala.io.StdIn

  case class Point(x: Double, y: Double) {
    def distanceTo(p: Point): Double = Math.sqrt(Math.pow(p.x - x, 2) + Math.pow(p.y - y, 2))
  }
  case class Line(p1: Point, p2: Point) {
    def reverse: Line = Line(p2: Point, p1: Point)
  }
  case class Triangle(p1: Point, p2: Point, p3: Point) {
    def getFirstEdge: Line = Line(p1, p3)
    def getSecondEdge: Line = Line(p3, p2)
  }

  def main(args: Array[String]) {

    val n = StdIn.readInt()
    val inputs = (1 to n).map(_ => StdIn.readLine()).map { str =>
        val coords = str.split(" ")
        Point(coords(0).toDouble, coords(1).toDouble)
    }.toList

    val result = quickHull(inputs)
    println(perimeter(result))
  }

  /**
   * QuickHull algorithm
   */
  def quickHull(points: List[Point]): List[Point] = {

    /**
     * Get distance between the line and the point.
     */
    def getDistance(line: Line, point: Point): Double = {

      val dx = line.p2.x - line.p1.x
      val dy = line.p2.y - line.p1.y
      val rest = line.p2.x * line.p1.y - line.p2.y * line.p1.x

      (dy * point.x - dx * point.y + rest) / Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2))
    }

    def findHull(pointsWithDistance: List[(Point, Double)], line: Line): List[Point] = {
      if (pointsWithDistance.isEmpty) Nil
      else {

        def getPointsOnOneSide(points: List[Point], line: Line): List[(Point, Double)] =
          points.map(p => (p, getDistance(line, p))).filter(_._2 < 0)

        val maxDistancePoint = pointsWithDistance.maxBy(pointWithDistance => Math.abs(pointWithDistance._2))._1

        val triangle = Triangle(line.p1, line.p2, maxDistancePoint)
        val edge1 = triangle.getFirstEdge
        val edge2 = triangle.getSecondEdge

        val outsidePoints = pointsWithDistance.filter(p => (p._1 != maxDistancePoint)
          && !isInsideTriangle(triangle, p._1)).map(_._1)

        val leftPoints = getPointsOnOneSide(outsidePoints, edge1)
        val rightPoints = getPointsOnOneSide(outsidePoints, edge2)

        findHull(leftPoints, edge1) ::: (maxDistancePoint :: findHull(rightPoints, edge2))
      }
    }

    def isInsideTriangle(triangle: Triangle, point: Point): Boolean = {

      def area(p1: Point, p2: Point, p3: Point): Double =
        Math.abs(p1.x * (p2.y - p3.y) + p2.x * (p3.y - p1.y) + p3.x * (p1.y - p2.y))

      //ABC - received triangle, P - point to be checked
      //area of triangle ABC
      val area1 = area(triangle.p1, triangle.p2, triangle.p3)
      //area of triangle PAB
      val area2 = area(point, triangle.p1, triangle.p2)
      //area of triangle PBC
      val area3 = area(point, triangle.p2, triangle.p3)
      //area of triangle PCA
      val area4 = area(point, triangle.p3, triangle.p1)

      area1 == area2 + area3 + area4
    }

    val pointWithMinX = points.minBy(_.x)
    val pointWithMaxX = points.maxBy(_.x)

    val line = Line(pointWithMinX, pointWithMaxX)
    val pointsWithDistance = points.map(p => (p, getDistance(line, p)))

    val (top, bottom) = pointsWithDistance.partition(_._2 < 0)

    (pointWithMinX :: findHull(top, line)) ::: (pointWithMaxX :: findHull(bottom, line.reverse))
  }

  def perimeter(poly: List[Point]): Double = {
    (poly.last :: poly).sliding(2).map(w => w.head.distanceTo(w(1))).sum
  }
}
