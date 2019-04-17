import scala.math.{abs, max}

object Main extends App {
  def doQueens(
      board: Array[Array[Int]]
  ) {
    val queenCount = board.length
    var slns = 0

    case class Point(x: Int, y: Int)

    def threatens(p1: Point, p2: Point) =
      p1.x == p2.x || 
      p1.y == p2.y || 
      (for {
        i <- 0 to max(abs(p1.x - p2.x), abs(p1.y - p2.y))
        if ((p1.x + i == p2.x && p1.y + i == p2.y) ||
          (p1.x - i == p2.x && p1.y - i == p2.y) ||
          (p1.x - i == p2.x && p1.y + i == p2.y) ||
          (p1.x + i == p2.x && p1.y - i == p2.y))
      } yield Point(p1.x + i, p1.y + i)).nonEmpty

    def threatensAny(p1: Point, points: Seq[Point]) =
      (for (p2 <- points if threatens(p1, p2)) yield p2).nonEmpty

    def getAllocatedPoints(): Seq[Point] =
      for {
        x <- 0 to board.length - 1
        y <- 0 to board.length - 1
        if (board(x)(y) != 0)
      } yield Point(x, y)

    def populateBoard(x: Int = 0): Boolean =
      if (x == queenCount) {
        val boardStr = board.deep.mkString("\n")
        println(s"Solution #${slns + 1}: \n----\n$boardStr\n----\n")
        slns += 1
        true
      } else {
        var result = false
        for (y <- 0 to queenCount - 1) {
          val point = Point(x, y)
          if (!threatensAny(point, getAllocatedPoints())) {
            board(x)(y) = 1
            result = populateBoard(x + 1) || result
            board(x)(y) = 0
          }
        }
        result
      }

    populateBoard()
  }

  doQueens(Array.ofDim[Int](8, 8))
  println("Queens Completed!")
  println()
}
