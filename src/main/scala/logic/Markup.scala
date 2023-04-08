package logic

import scala.collection.immutable.{Map => ImmutableMap}
import scala.collection.mutable.Map

/**
 * Represents the small markings scribbled into empty cells
 * of a Sudoku that tell what numbers can occupy that cell called the "markup" of the cell.
 *
 * An instance of this class contains the markup for all empty cells of a sudoku.
 *
 * Based on a paper by J. F. Crook:
 * [[https://www.ams.org/notices/200904/rtx090400460p.pdf A Pencil-and-Paper Algorithm for Solving Sudoku Puzzles]]
 *
 * This class contains methods to find preemptive sets in the markup that can be used
 * to remove possible numbers from cells in the markup that can't actually occupy those cells.
 *
 * The reduce method goes through all the cells in the markup and attempts to find preemptive in the markup,
 * for the sets it finds, it removes numbers that aren't actually possible from the markup.
 *
 * For sudokus with a single solution reducing the markup might on it's own be enough to solve the sudoku:
 *
 * {{{
 *   scala> val bs = BoardState.parseFromCsv(new BufferedReader(new FileReader("example_boards/b3.csv"))).get
 *   val bs: logic.BoardState = BoardState(16x16, 46.1% full)
 *
 *   scala> println(bs.printableString)
 *      5     13|7  14      |10    9  11|4
 *      11    2 |12    9    |13         |   10    3
 *   3        16|5        4 |6     14   |11
 *   9          |      10   |           |5
 *   -----------------------------------------------
 *   1     2    |   4     12|         8 |6  11    10
 *      7     9 |10         |12 6       |   13 15 1
 *   12 13 14   |8     3  6 |   10      |7  5  16 4
 *      6       |1  15      |2        14|   8  12
 *   -----------------------------------------------
 *      15 16   |14       9 |      6  5 |      1
 *   11 12 7  1 |      5    |14 15    10|   4  2  13
 *   2  3  9    |      7  15|         13|16    5
 *   6     5  4 |13         |11    8    |   12    15
 *   -----------------------------------------------
 *            7 |           |   9       |         6
 *            12|   5     2 |3        15|13       8
 *   14    3    |         13|   11    6 |15    10
 *            6 |3  9     14|      10 12|1     4
 *
 *   scala> val m = Markup.from(bs)
 *   val m: logic.Markup = logic.Markup@3691d4da
 *
 *   scala> m.reduceCompletely()
 *
 *   scala> m.isSolved
 *   val res2: Boolean = true
 *
 *   scala> val changes = m.locations.map(l => (l, Some(m(l).head))).toMap
 *   val changes: scala.collection.immutable.Map[logic.Coords,Some[Int]] = HashMap(Coords(7,9) -> Some(8), ...
 *
 *   scala> println(bs.withChanges(changes).printableString)
 *   8  5  12 13|7  14 15 16|10 3  9  11|4  1  6  2
 *   7  11 15 2 |12 6  9  1 |13 4  5  16|14 10 8  3
 *   3  10 1  16|5  13 8  4 |6  12 14 2 |11 15 9  7
 *   9  4  6  14|2  3  10 11|1  8  15 7 |5  16 13 12
 *   -----------------------------------------------
 *   1  16 2  3 |9  4  13 12|15 5  7  8 |6  11 14 10
 *   4  7  8  9 |10 11 14 5 |12 6  16 3 |2  13 15 1
 *   12 13 14 15|8  2  3  6 |9  10 11 1 |7  5  16 4
 *   5  6  10 11|1  15 16 7 |2  13 4  14|3  8  12 9
 *   -----------------------------------------------
 *   13 15 16 10|14 12 4  9 |7  2  6  5 |8  3  1  11
 *   11 12 7  1 |6  16 5  8 |14 15 3  10|9  4  2  13
 *   2  3  9  8 |11 10 7  15|4  1  12 13|16 6  5  14
 *   6  14 5  4 |13 1  2  3 |11 16 8  9 |10 12 7  15
 *   -----------------------------------------------
 *   16 2  11 7 |15 8  1  10|5  9  13 4 |12 14 3  6
 *   10 9  4  12|16 5  6  2 |3  14 1  15|13 7  11 8
 *   14 1  3  5 |4  7  12 13|8  11 2  6 |15 9  10 16
 *   15 8  13 6 |3  9  11 14|16 7  10 12|1  2  4  5
 * }}}
 *
 */
class Markup private (bsl: Int, initialMarkup: ImmutableMap[Coords,Vector[Int]]) {

  /**
   * The side length of blocks in the size sudoku this markup belongs to.
   * This number is the square root of the [[sideLength]].
   * A markup of a 9x9 sudoku has a blockSideLength of 3.
   */
  val blockSideLength: Int = bsl

  /** The side-length of the size of sudoku this markup belongs to, for a markup of a 9x9 sudoku this is 9. */
  val sideLength: Int = this.blockSideLength * this.blockSideLength

  private val markup: Map[Coords,Vector[Int]] = Map()
  markup ++= initialMarkup

  /**
   * Returns the '''Coords''' to all the locatins this Markup has information about.
   * @return an '''Iterable''' containing the locations this Markup has information about.
   */
  def locations: Iterable[Coords] = this.markup.keys

  /**
   * Returns the possible numbers for a location this markup has information about, according to this markup.
   * @param location a location this markup has information about
   * @return a '''Vector''' containing the possible numbers for '''location'''
   */
  def apply(location: Coords): Vector[Int] = this.markup(location)

  /**
   * Optionally returns the possible numbers for a location if this markup has information about it.
   *
   * Similar to `scala.collection.Map.get`.
   *
   * @param location a location
   * @return a '''Vector''' containing the possible numbers for '''location''' wrapped in an '''Option''' if found, otherwise '''None'''
   */
  def get(location: Coords): Option[Vector[Int]] = this.markup.get(location)

  // gives the (blockX, blockY) pair for a location on the board:
  private def toBlockCoords(location: Coords): (Int, Int) = (location.x/this.blockSideLength, location.y/this.blockSideLength)



  /**
   * Scans through this Markup to find preemptive sets.
   * When sets are found, removes the numbers in the set from cells that are in the range of the set, but not part of the set.
   * @return true if changes were made
   */
  def reduce(): Boolean = {
    val rows    = this.markup.keys.groupBy(_.y).values
    val columns = this.markup.keys.groupBy(_.x).values
    val blocks  = this.markup.keys.groupBy(toBlockCoords(_)).values
    val zones = rows ++ columns ++ blocks

    var didChanges = false

    for (zone <- zones; cell <- zone) {
      // attempt to find a preemptive set:
      val possibleSet = preemptiveSet(cell, zone.toVector.diff(Vector(cell)))
      possibleSet match {
        // no set, do nothing:
        case None => {}
        // if a set was found:
        case Some((setCoords, setNums)) => {
          val notInSet = zone.filterNot(setCoords.contains(_))
          // cells in the same zone (row, column or block) cannot contain numbers in the
          // preemptive set so we remove those
          notInSet.foreach(l => {
            val oldMarkup = this.markup(l)
            val newMarkup = oldMarkup.diff(setNums)
            if (newMarkup.length != oldMarkup.length)
              didChanges = true
            this.markup(l) = newMarkup
          })
        }
      }
    }

    didChanges
  }


  /**
   * Runs `reduce()` on this Markup until attempting to
   * reduce no longer makes any changes
   * (i.e. `reduce()` returns '''false''').
   */
  def reduceCompletely() = {
    var complete: Boolean = false
    while (!complete) {
      complete = !this.reduce()
    }
  }


  /**
   * Returns true if all cells in this Markup can only be occupied by one possible number.
   * meaning that this markup contains the solution to it's board.
   */
  def isSolved: Boolean = this.markup.values.forall(_.lengthIs == 1)

  /**
   * Returns true if this Markup has a cell that can't take any number, meaning that the board it represents has no solutions.
   *
   * '''NOTE!''' a Markup not being "clearly unsolvable" is not equivalent to it having a solution.
   */
  def isClearlyUnsolvable: Boolean = !this.markup.values.forall(_.nonEmpty)

  /**
   * Calls `BoardState.possibleNumbers` for '''board''',
   * then improves that guess using the information contained in this markup.
   *
   * Returns a '''Vector''' containing those numbers that are in both
   * `BoardState.possibleNumbers(location)` and `this.apply(location)`.
   *
   * @param board '''BoardState''' to get original guess from
   * @param location the location to get possible numbers for
   * @return '''Vector''' of numbers that can go in '''location'''
   */
  def possibleNumbers(board: BoardState, location: Coords): Vector[Int] =
    board.possibleNumbers(location).filter(this.markup(location).contains(_))


  /**
   * Attempts to return a preemptive set using a greedy algorithm.
   *
   * Only returns meaningful sets, will not return sets containing all the free cells in a column, row or block.
   *
   * @param start the first location in the preemptive set
   * @param rest rest of the locations to build the set from
   * @return a valid preemptive set wrapped in an '''Option''' if found, otherwise '''None'''
   */
  def preemptiveSet(start: Coords, rest: Vector[Coords]): Option[(Vector[Coords], Vector[Int])] =
    preemptiveSet(Vector(start), rest, this.markup(start))

  private def preemptiveSet(set: Vector[Coords], rest: Vector[Coords], inSet: Vector[Int]): Option[(Vector[Coords], Vector[Int])] = {
    // the current set is a valid preemptive set if the amount of Coords
    // in the set is the same as the amount of possible numbers in the set

    // ... on the other hand, in a sudoku that has at least a single solution,
    // the free cells in any column, row or block form a preemptive set,
    // but thoes kinds of sets aren't meaningful for solving the sudoku,
    // so we only return sets that are smaller than the input set

    if (rest.isEmpty) {
      // non-meaningful preemptive set
      None
    } else if (set.length == inSet.length) {
      // a valid preemptive set
      Some(set, inSet)
    } else {
      // create a vector of (coords, score) pairs, where score is the amount of possible numbers
      // that cells has that are not already in inSet
      // lower score better, that means least new numbers introduced
      val newLocationCandidates: Vector[(Coords, Int)] = rest.map(l => {
        (l, this.markup(l).diff(inSet).length)
      })
      val newLocation: Coords = newLocationCandidates.minBy(_._2)._1
      preemptiveSet(set ++ Vector(newLocation), rest.filterNot(_ == newLocation), (inSet ++ this.markup(newLocation)).distinct)
    }
  }

  /**
   * Returns a new completely reduced markup by creating a copy of this markup with
   * the given cells filled with the given numbers, then reducing the new markup completely before returning it.
   *
   * Does not affect this Markup.
   *
   * @param cells a Map containing '''Coords'''-Number -pairs for the cells to be filled
   * @return a new completely reduced Markup with the given cells filled
   */
  def withCellsFilled(cells: ImmutableMap[Coords, Int]): Markup = {
    val newMarkup = Markup.from(this)

    // remove the filled locations
    newMarkup.markup --= cells.keys

    cells.foreach(p => {
      val (location, number) = p
      val block = this.toBlockCoords(location)

      // locations affected (i.e. locations on the same row column or block as the filled cell):
      val nearby = newMarkup.markup.keys.filter(l => l.x == location.x || l.y == location.y || this.toBlockCoords(l) == block)

      // remove the added number from the nearby locations
      nearby.foreach(l => newMarkup.markup(l) = newMarkup.markup(l).diff(Vector(number)))
    })

    // return a completely reduced markup
    newMarkup.reduceCompletely()

    newMarkup
  }

}

/**
 * Companion object that contains methods to create new Markups.
 */
object Markup {

  /**
   * Creates a Markup from a BoardState using [[BoardState.markup]].
   * @param board BoardState to create a Markup from
   * @return a new Markup containing the empty locations of `board`
   */
  def from(board: BoardState): Markup = new Markup(board.blockSideLength, board.markup)

  /**
   * Creates a copy of a Markup.
   * @param other Markup to copy
   * @return a new Markup containing the same information as '''other'''
   */
  def from(other: Markup): Markup = new Markup(other.blockSideLength, other.markup.toMap)

  /**
   * Creates a Markup from a BoardState using [[BoardState.markup]],
   * then runs `reduceCompletely()` on the newly created Markup
   * to return a completely reduced markup.
   * @param board BoardState to create a Markup from
   * @return a new reduced Markup containing the empty locations of `board`
   */
  def reducedFrom(board: BoardState): Markup = {
    val m = this.from(board)
    m.reduceCompletely()
    m
  }

}
