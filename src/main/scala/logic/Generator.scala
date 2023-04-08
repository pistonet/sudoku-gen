package logic

import scala.util.Random
import scala.util.Try
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.CollectionConverters._

/**
 * This object contains methods for generating new Sudokus.
 */
object Generator {

  /** The iterationLimit for running Solver functions while generating Sudokus. */
  private val iterationLimit = 225

  /** The iterationLimit for running Solver functions in the refine function. */
  private val refineIterationLimit = 190

  /**
   * The amount of refining attempts per each iteration, a higher number will result in
   * worse performance (slower generation), but better Sudokus (i.e less clues) with diminishing returns.
   */
  private val refineAttemptsPerIteration: Int = 8

  /**
   * Generates a new Sudoku of the requested size.
   *
   * The generated Sudokus are "proper" in the sense that they always have one
   * and exactly one solution.
   *
   * @param blockSideLength the side length of a block in the new Sudoku, this number is the square root of the side length of a sudoku
   * @return a tuple containing the generated Sudoku and its solution
   */
  def create(blockSideLength: Int): (BoardState, BoardState) = {

    // attempts to create a new sudoku,
    // returns the board and its solution or None if iteration limit is reached
    def possibleNewBoard: Option[(BoardState, BoardState)] = Try {
      // create new random filled board
      // this step can throw util.IterationLimitException:
      val filledBoard = newFilledBoard(blockSideLength)

      val steps = boardToSteps(filledBoard)

      // get the last step with a single solution,
      // this step can throw util.IterationLimitException:
      val board = steps.take(bifurcationPoint(steps)).last

      (board, filledBoard)
    }.toOption

    // get the first successfully generated board
    val (board, solution) = LazyList.continually(possibleNewBoard).flatten.head

    // the generated board is ok but some numbers can still be removed
    // refine the board, return the refined board and the solution
    (refine(board), solution)

  }

  /**
   * Generates a new random filled Sudoku board by solving an empty board in random order.
   * @param blockSideLength the side length of a block in the new Sudoku
   * @return a new filled board of requested size
   */
  private def newFilledBoard(blockSideLength: Int): BoardState = {
    Solver.solve(
      board = BoardState.createEmpty(blockSideLength),
      shuffle = true,
      assumeValidBoard = true,
      iterationLimit = Some(iterationLimit)
    ).head
  }

  /**
   * Takes a completely filled Sudoku board and returns a Vector where each board is the same as the last but has one more cell emptied.
   * @param filledBoard a completely filled Sudoku board
   * @return a Vector where the first board is the original board and each board after that has one more cell emptied.
   */
  private def boardToSteps(filledBoard: BoardState): Vector[BoardState] = {
    // random order to start clearing locations
    val locations: Vector[Coords] = Random.shuffle(filledBoard.allLocations)
    // return vector of increasingly empty BoardStates
    locations.scanLeft(filledBoard)((board, location) => board.withChanges(Map(location -> None)))
  }

  /**
   * Sets the left and right limit to the index of the first and last board, then runs the method bifurcationPoint on the Vector.
   * @param boards the Vector of boards to run the seearch on
   * @return the index of the first board that has more than one solution
   */
  private def bifurcationPoint(boards: Vector[BoardState]): Int = this.bifurcationPoint(boards, 0, boards.indices.last)

  /**
   * Uses a binary search algorithm on a Vector of BoardStates to find the point where the boards start to have more than one solution.
   *
   * It is assumed that the first n (> 0) board have only a single solution while the rest have multiple solutions.
   *
   * @param boards the Vector of boards to run the seearch on
   * @param leftLimit the index of a board that is known to have a single solution
   * @param rightLimit the index of a board that is known to have multiple solutions
   * @return the index of the first board that has more than one solution
   */
  private def bifurcationPoint(boards: Vector[BoardState], leftLimit: Int, rightLimit: Int): Int = {
    if (leftLimit + 1 == rightLimit) {
      rightLimit
    } else {
      val middle: Int = (leftLimit + rightLimit) / 2
      Solver.getSolvability(boards(middle), assumeValidBoard = true, iterationLimit = Some(iterationLimit)) match {
        case SingleSolution => bifurcationPoint(boards, middle, rightLimit)
        case MultipleSolutions => bifurcationPoint(boards, leftLimit, middle)
        case Unsolvable => throw new Exception(s"board with index ${middle} is unsolvable")
      }
    }
  }

  /**
   * Attempts to remove filled cells from a previously generated (assumed single solution) board,
   * without increasing the amount of solutions the board has.
   * @param board previously generated board that has a single solution
   * @return the refined board
   */
  private def refine(board: BoardState): BoardState = {

    // the amount of cells to empty per attempt, remove more for higher fill percentage
    // fill percentage <= 40% -> 1 cell
    // fill percentage <= 43% -> 2 cells
    // fill percentage <= 46% -> 3 cells
    // ...
    // fill percentage <= 65% -> 10 cells
    // ...
    // fill percentage <= 90% -> 18 cells
    // ...
    val removalAmount: Int = Option((board.filledPercentage - 35) / 3).map(_.toInt).filter(_ >= 1).getOrElse(1)

    // each Vector inside the ParVector corresponds to a new board
    // where all the Coords inside the Vector are cleared from the board
    val possibleRemovals: ParVector[Vector[Coords]] =
      Vector.fill(refineAttemptsPerIteration)(Random.shuffle(board.filledLocations).take(removalAmount)).par

    val possibleBoards: ParVector[BoardState] = possibleRemovals.map(removals => board.withChanges(removals.map(l => (l, None)).toMap))

    // filter out boards that:
    // a) have multiple solutions or
    // b) getSolvability reaches iterationLimit while calculating
    val singleSolutionBoards: ParVector[BoardState] = possibleBoards.filter(b => {
      val possibleSolvability: Option[Solvability] = Try {
        Solver.getSolvability(b, assumeValidBoard = true, iterationLimit = Some(refineIterationLimit))
      }.toOption
      possibleSolvability.contains(SingleSolution)
    })

    // if refining doesn't result in a better board, return the original board,
    // if refining succeeds, try to refine the result again
    singleSolutionBoards.headOption match {
      case None => board
      case Some(newBoard) => refine(newBoard)
    }
  }

}
