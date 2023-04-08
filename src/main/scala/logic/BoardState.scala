package logic

import java.io._

/**
 * Represents a single state of a sudoku board.
 * `BoardState`s are immutable.
 */
class BoardState private (bsl: Int, private val contents: Vector[Vector[Option[Int]]]) {


  /**
   * The side length of blocks in the sudoku.
   * This number is the square root of the [[sideLength]].
   * A 9x9 sudoku has a blockSideLength of 3.
   */
  val blockSideLength = bsl

  /** The side-length of the sudoku, for a 9x9 sudoku this is 9. */
  val sideLength: Int = this.blockSideLength * this.blockSideLength

  // Shortcuts
  private def numbers = 1 to this.sideLength
  private def indices = 0 until this.sideLength
  private def blockIndices = 0 until this.blockSideLength

  /**
   * Returns the contents of a cell on this board in the specified location.
   *
   * '''NOTE!''' Not a lift method, returns '''None''' for empty cells only,
   * this method will throw exceptions when trying to access locations outside the board.
   *
   * @param location a location on the board where `Coords(0,0)` is the upper leftmost cell
   * @return the contents of the cell wrapped in an '''Option''', '''None''' if the cell is empty
   */
  def apply(location: Coords): Option[Int] = this.contents(location.y)(location.x)


  /** Returns a vector containing [[Coords]] to all the cells on this board. */
  def allLocations: Vector[Coords] = {
    val s = for (y <- this.indices; x <- this.indices)
      yield new Coords(x, y)
    s.toVector
  }


  /** Returns a vector containing [[Coords]] to all the empty cells on this board. */
  def emptyLocations: Vector[Coords] = this.allLocations.filter(this(_).isEmpty)


  /** Returns a vector containing [[Coords]] to all cells on this board that have a number in them. */
  def filledLocations: Vector[Coords] = this.allLocations.filter(this(_).isDefined)


  /**
   * Returns a new [[BoardState]] with changes.
   * @param changes a map where each key-value pair contains the [[Coords]] to a cell on the board and its new contents
   * @return a new, edited [[BoardState]] object
   */
  def withChanges(changes: Map[Coords, Option[Int]]): BoardState = {
    val contents = Vector.tabulate(this.sideLength,this.sideLength){
      (y,x) => changes.getOrElse(Coords(x, y), this(Coords(x, y)))
    }
    new BoardState(this.blockSideLength,contents)
  }


  /**
   * Returns a printable string representing the contents of the sudoku, for debugging purposes.
   * @return a multiline string representing the board contents, with lines seperating the blocks of the sudoku
   */
  def printableString = {
    // horizontal spacer line:
    val spacerLine: String = "-" * ((1+this.sideLength.toString.length)*this.sideLength - 1) + "\n"
    // prints contents of a cell padded with spaces to the length of the number with most digits this board can have
    def printCell(c: Option[Int]): String = c.map(_.toString).getOrElse("").padTo(this.sideLength.toString.length, ' ')

    // go through the cells one at a time
    val arr = for ((row, y) <- this.contents.zipWithIndex; (cell, x) <- row.zipWithIndex)
      yield {
        printCell(cell) + (
          // are we on the last cell of the row or not?
          if (x == this.sideLength-1) {
            // is this the last row?
            if (y == this.sideLength-1) {
              // after the last cell of the last row add nothing
              ""
            } else {
              // after other last cells of row: newline and spacer if necessary
              "\n" + (if ((y+1) % this.blockSideLength == 0) spacerLine else "")
            }
          } else {
            // after cells that are not last cell of row: vertical line or space
            (if ((x+1) % this.blockSideLength == 0) "|" else " ")
          }
        )
      }
    arr.mkString("")
  }


  /** Returns a percentage representing the ratio of filled cells to all cells on the board. */
  def filledPercentage: Double = 100.0 * this.filledLocations.length.toDouble / (this.sideLength*this.sideLength)


  /**
   * Returns a textual representation of the board, including its size and fill percentage.
   * @return for a 9x9 sudoku this could for example be `BoardState(9x9, 34.6% full)`
   * */
  override def toString = f"BoardState(${this.sideLength}x${this.sideLength}, ${this.filledPercentage}%1.1f%% full)"


  private def row(y: Int): Vector[Option[Int]] = this.contents(y)

  private def column(x: Int): Vector[Option[Int]] = this.contents.map(row => row(x))

  private def block(blockX: Int, blockY: Int): Vector[Option[Int]] = {
    val xOffset = blockX * this.blockSideLength
    val yOffset = blockY * this.blockSideLength
    val s = for (y <- this.blockIndices; x <- this.blockIndices)
      yield this.contents(y+yOffset)(x+xOffset)
    s.toVector
  }


  /**
   * Returns true for valid sudoku boards. In this case valid means that
   * all rows, columns and blocks contain all of the numbers from 1 to [[sideLength]] at most once.
   * @return '''true''' if the board is valid, '''false''' otherwise
   */
  def isValid = {
    val rows = Vector.tabulate(this.sideLength)(n => this.row(n))
    val cols = Vector.tabulate(this.sideLength)(n => this.column(n))
    val blocks = Vector.tabulate(this.blockSideLength, this.blockSideLength)((y,x) => this.block(x,y)).flatten

    val zones: Vector[Vector[Option[Int]]] = rows ++ cols ++ blocks

    zones.forall(zone => {
      val numbers: Vector[Int] = zone.flatten
      numbers.distinct.length == numbers.length
    })
  }

  /**
   * Called from equality methods.
   * @param that the object to compare this with
   * @return '''true''' if '''that''' is also an instance of BoardState, '''false''' otherwise
   */
  def canEqual(that: Any) = that.isInstanceOf[BoardState]

  /**
   * Compares this BoardState to other objects.
   * @param other object to compare this with
   * @return '''true''' if '''other''' is a BoardState of the same size and has exactly the same cells as '''this''', '''false''' otherwise
   */
  override def equals(other: Any): Boolean = {
    other match {
      case other: BoardState => {
        // this order of lazy evaluation is important to avoid trying to access board locations out of bounds
        this.blockSideLength == other.blockSideLength && this.allLocations.forall(loc => this(loc) == other(loc))
      }
      case _ => false
    }
  }

  /**
   * Returns a list containing all the numbers that can be entered to a location on the
   * board without creating a situation where the board has duplicate numbers.
   * If the location already has a (in)valid number it will be ignored in the calcullation.
   * @param location [[Coords]] to a location on the board
   * @return a '''Vector''' containing all the valid numbers
   */
  def possibleNumbers(location: Coords): Vector[Int] = {
    val bs = if (this(location).isEmpty) this else this.withChanges(Map(location -> None))
    val nearby: Vector[Int] = (
      bs.row(location.y) ++ bs.column(location.x) ++ bs.block(location.x/this.blockSideLength, location.y/this.blockSideLength)
    ).flatten.distinct
    Vector.from(this.numbers).diff(nearby)
  }

  /**
   * Calculates the possible numbers for all empty locations on the board.
   *
   * This method is much faster than the calculating [[possibleNumbers]] for all locations seperately.
   * @return A Map whose keys are all '''empty''' locations on the board, and values are Vectors containing the valid numbers for the corresponding locations.
   */
  def markup: Map[Coords, Vector[Int]] = {
    val v = Vector.fill(this.sideLength,this.sideLength)(scala.collection.mutable.Set.from(this.numbers))

    for (y <- this.indices) {
      val rowNums = this.row(y).flatten
      v(y).foreach(cell => cell --= rowNums)
    }

    for (x <- this.indices) {
      var colNums = this.column(x).flatten
      v.foreach(row => row(x) --= colNums)
    }

    for (blockX <- this.blockIndices; blockY <- this.blockIndices) {
      val blockNums = this.block(blockX,blockY).flatten
      for (x <- this.blockIndices; y <- this.blockIndices) {
        v(blockY*this.blockSideLength+y)(blockX*this.blockSideLength+x) --= blockNums
      }
    }

    this.emptyLocations.map(c => (c, v(c.y)(c.x).toVector)).toMap
  }


  /**
   * Returns a string representing this BoardState in .csv format.
   * @return a multiline string that contains the contents of this BoardState
   */
  def toCsv: String = {
    // the amount of digits in the largest number that can be on this board
    val paddingWidth = this.numbers.last.toString.length
    def printCell(c: Option[Int]): String = c.map(_.toString).getOrElse("").padTo(paddingWidth, ' ')

    val boardCsvData = this.contents.map(line => {
      line.map(cell => printCell(cell)).mkString(",")
    }).mkString("\n")

    this.blockSideLength.toString + "\n" + boardCsvData + "\n"
  }

  /**
   * Tests if the given coordinates point to a valid location on this board.
   * @param location the coordinates to test
   * @return '''true''' if the location exists on the board, '''false''' otherwise
   */
  def containsLocation(location: Coords): Boolean = {
    0 <= location.x && location.x < this.sideLength && 0 <= location.y && location.y < this.sideLength
  }


}


/**
 * This companion object contains methods to create new [[BoardState]]s.
 */
object BoardState {

  /**
   * Creates a new [[BoardState]] with blockSideLength and contents.
   * @param blockSideLength length of the side of a block in the sudoku, i.e. the square root of the side length.
   * @param contents 2-dimensional array containing the cells on the board.
   * @return a new [[BoardState]] of requested size and contents
   */
  def apply(blockSideLength: Int, contents: Vector[Vector[Option[Int]]]) = {
    require(blockSideLength > 0, "blockSideLength must be positive")

    val sideLength = blockSideLength*blockSideLength

    require(contents.length == sideLength && contents.forall(_.length == sideLength), "invalid contents size")
    require(contents.flatten.forall(_.forall(n => n > 0 && n <= sideLength)), "invalid contents")

    new BoardState(blockSideLength, contents)
  }


  /**
   * Creates an empty [[BoardState]] of the requested dimensions.
   * @param blockSideLength the length of the side a block for the new sudoku
   * @return a new, completely empty sudoku
   */
  def createEmpty(blockSideLength: Int) = {
    val sideLength = blockSideLength*blockSideLength
    val contents: Vector[Vector[Option[Int]]] = Vector.fill(sideLength,sideLength)(None)
    new BoardState(blockSideLength, contents)
  }


  /**
   * Attemps to parse a BoardState from provided csv data.
   *
   * Returns None if
   *   - a block side length cannot be parsed from the file
   *   - the bsl doesn't fulfill: `0 < bsl <= maxBlockSideLength`
   *
   * Otherwise returns a [[BoardState]] where cells are filled if
   * an integer in the range valid for the board can be parsed from
   * the equivalent position in the csv data.
   *
   * The upper limit for the block side length exists to stop
   * this method from attempting to parse arbitrarily big boards.
   *
   * This method doesn't deal with possible [[java.io.IOException]]s thrown by the reader.
   *
   * @param reader a [[java.io.BufferedReader]] that provides the .csv data to parse
   * @param maxBlockSideLength the upper limit for the `blockSideLength` this method will try to parse
   * @return the parsed [[BoardState]] wrapped in an '''Option''' if successful, otherwise '''None'''
   */
  def parseFromCsv(reader: BufferedReader, maxBlockSideLength: Int = 6): Option[BoardState] = {
    // from the BufferedReader create a 2-dimensional lazy-list of options where we can try to access a certain position in the file:
    val lines: LazyList[String] = LazyList.continually(reader.readLine()).takeWhile(_ != null)
    val lines2d: LazyList[Array[Option[Int]]] = lines.map( _.split(",").map(_.trim.toIntOption) )
    def getCell(line: Int, index: Int): Option[Int] = lines2d.lift(line).flatMap(ar => ar.lift(index)).flatten

    // the blockSideLength must be positive and smaller than the set max value
    val blockSideLength: Option[Int] = getCell(0,0).filter(bsl => 0 < bsl && bsl <= maxBlockSideLength)

    blockSideLength.map(bsl => {
      val sideLength = bsl*bsl
      // the numbers inside the board must fulfill: 0 < n <= sidelength
      val contents: Vector[Vector[Option[Int]]] = Vector.tabulate(sideLength,sideLength)(
        (y,x) => getCell(y+1,x).filter(i => 0 < i && i <= sideLength)
      )
      new BoardState(bsl, contents)
    })
  }


}
