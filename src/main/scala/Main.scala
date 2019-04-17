object Main extends App {
  def doQueens(
      board: Array[Array[Int]]
  ) {
    val queenCount = board.length
    var slns = 0

    case class Point(x: Int, y: Int)

    def threatens(p1: Point, p2: Point) =
      p1.x == p2.x || p1.y == p2.y || threatensDiagonally(p1, p2)

    def threatensAny(p1: Point, points: Seq[Point]) =
      (for (p2 <- points if threatens(p1, p2)) yield p2).nonEmpty

    def threatensDiagonally(p1: Point, p2: Point): Boolean = {
      def threatensDiagonallyUp(p1i: Point = p1): Boolean =
        if (p1i.x == p2.x && p1i.y == p2.y) true
        else if (p1i.x >= p2.x || p1i.y >= p2.y) false
        else threatensDiagonallyUp(Point(p1i.x + 1, p1i.y + 1))

      def threatensDiagonallyDown(p1i: Point = p1): Boolean =
        if (p1i.x == p2.x && p1i.y == p2.y) true
        else if (p1i.x <= p2.x || p1i.y <= p2.y) false
        else threatensDiagonallyDown(Point(p1i.x - 1, p1i.y - 1))

      def threatensDiagonallyUpInverse(p1i: Point = p1): Boolean =
        if (p1i.x == p2.x && p1i.y == p2.y) true
        else if (p1i.x <= p2.x || p1i.y >= p2.y) false
        else threatensDiagonallyUpInverse(Point(p1i.x - 1, p1i.y + 1))

      def threatensDiagonallyDownInverse(p1i: Point = p1): Boolean =
        if (p1i.x == p2.x && p1i.y == p2.y) true
        else if (p1i.x >= p2.x || p1i.y <= p2.y) false
        else threatensDiagonallyDownInverse(Point(p1i.x + 1, p1i.y - 1))

      threatensDiagonallyDown() ||
      threatensDiagonallyUp() ||
      threatensDiagonallyUpInverse() ||
      threatensDiagonallyDownInverse()
    }

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
