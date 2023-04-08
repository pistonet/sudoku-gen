package logic

import scala.util.Random
import util.IterationLimiter

/**
 * This object contains methods to solve Sudokus and approximathe the amount of solutions a Sudoku has.
 */
object Solver {

  /** once the filledPercentage of a BoardStage being solved reaches this threshold
   * useOptimalAlgorithm will start using crookMarkupSolver. */
  private val crookSolverThreshold: Double = 50

  /**
   * Solves a Sudoku and returns all the solutions for it in no specific order.
   *
   * A possible '''iterationLimit''' can be set to limit the amount of iterations the algorithm will go through.
   * If a limit is set and it is reached, an [[util.IterationLimitException]] will be thrown when
   * more solutions are requested from the LazyList.
   *
   * @param board the [[BoardState]] to solve
   * @param shuffle whether to add randomization or not, if '''true''' the solutions appear in a random order
   * @param assumeValidBoard set to '''true''' if the board is known to be valid to skip validity testing
   * @param iterationLimit a possible limit to the amount of iterations the algorithm will go through
   * @return a LazyList containing all the solutions to the Sudoku
   */
  def solve(
    board: BoardState,
    shuffle: Boolean = false,
    assumeValidBoard: Boolean = false,
    iterationLimit: Option[Int] = None
  ): LazyList[BoardState] = {
    if (assumeValidBoard || board.isValid) {
      iterationLimit match {
        case None => useOptimalAlgorithm(board, shuffle = shuffle)
        case Some(limit) => {
          val limiter = IterationLimiter(limit)
          useOptimalAlgorithm(board, shuffle = shuffle, onIteration = limiter.increment())
        }
      }
    } else {
      LazyList()
    }
  }


  /**
   * Approximates the amount of solutions a Sudoku board has.
   *
   * Returns either [[SingleSolution]], [[MultipleSolutions]] or [[Unsolvable]].
   *
   * A possible '''iterationLimit''' can be set to limit the amount of iterations the algorithm will go through.
   * If a limit is set and it is reached, an [[util.IterationLimitException]] will be thrown.
   *
   * @param board the [[BoardState]] to solve.
   * @param assumeValidBoard set to '''true''' if the board is known to be valid to skip validity testing
   * @param iterationLimit a possible limit to the amount of iterations the algorithm will go through
   * @return a [[Solvability]] object representing the amount of solutions the board has
   */
  def getSolvability(board: BoardState, assumeValidBoard: Boolean = false, iterationLimit: Option[Int] = None): Solvability = {
    if (!(assumeValidBoard || board.isValid)) {
      return Unsolvable
    }
    if (board.sideLength == 9 && board.filledLocations.length < 17) {
      // According an article published in the journal Experimental Mathematics
      // (Taylor & Francis, Volume 23, 2014 - Issue 2) by a team lead by Gary McGuire,
      // a (valid) 9x9 sudoku with 16 (or less) clues will always have more than 1 solution.
      // https://www.tandfonline.com/doi/abs/10.1080/10586458.2013.870056
      // It seems they went through all of the possible boards.
      MultipleSolutions
    } else {
      this.solve(board, shuffle = false, assumeValidBoard = false, iterationLimit = iterationLimit).lengthIs match {
        case l if (l == 0) => Unsolvable
        case l if (l == 1) => SingleSolution
        case _ => MultipleSolutions
      }
    }
  }

  /**
   * Tests the filledPercentage of a BoardState and then uses the algorithm best suited for it to solve the BoardState.
   * @param board the board to solve
   * @param markup a [[Markup]] for the board if one can be easily generated from a previous step
   * @param shuffle whether to add randomization or not, if '''true''' the solutions appear in a random order
   * @param onIteration a function that is ran on each iteration
   * @return a LazyList containing all the solutions to the board
   */
  private def useOptimalAlgorithm(
    board: BoardState,
    markup: Option[Markup] = None,
    shuffle: Boolean = false,
    onIteration: => Unit = {}
  ): LazyList[BoardState] = {
    onIteration
    if (board.filledPercentage > crookSolverThreshold) {
      crookMarkupSolver(board, markup, shuffle, onIteration)
    } else {
      dynamicBacktrackingSolver(board, shuffle, onIteration)
    }
  }

  /**
   * A basic backtracking algorithm that solves sudokus by inserting a number
   * to the cell with least possible numbers on each iteration.
   * @param board the board to solve
   * @param shuffle whether to add randomization or not, if '''true''' the solutions appear in a random order
   * @param onIteration a function that is ran on each iteration
   * @return a LazyList containing all the solutions to the board
   */
  private def dynamicBacktrackingSolver(board: BoardState, shuffle: Boolean = false, onIteration: => Unit): LazyList[BoardState] = {
    board.markup.minByOption(_._2.length) match {
      case None => LazyList(board)
      case Some((c: Coords, nums: Vector[Int])) => {
        val tryOrder = if (shuffle) Random.shuffle(nums) else nums
        tryOrder.to(LazyList).flatMap(n => {
          useOptimalAlgorithm(
            board.withChanges(Map(c -> Some(n))),
            shuffle = shuffle,
            onIteration = onIteration
          )
        })
      }
    }
  }

  /**
   * A more intelligent algorithm that requires less iterations to solve a board, but is more costly to run each iteration.
   * Uses [[Markup]]s to solve the board.
   * @param board the board to solve
   * @param markup a [[Markup]] for the board if one can be easily generated from a previous step
   * @param shuffle whether to add randomization or not, if '''true''' the solutions appear in a random order
   * @param onIteration a function that is ran on each iteration
   * @return a LazyList containing all the solutions to the board
   */
  private def crookMarkupSolver(
    board: BoardState,
    markup: Option[Markup] = None,
    shuffle: Boolean = false,
    onIteration: => Unit
  ): LazyList[BoardState] = {
    val markup_ = markup.getOrElse(Markup.reducedFrom(board))

    // forced cells are cells that only have 1 possible number that can go there
    val (forcedCells, otherCells) = markup_.locations.map(l => (l, markup_(l))).partition(_._2.lengthIs == 1)

    // a copy of the board with all the forced cells filled
    val board2 = board.withChanges(forcedCells.map(p => (p._1, Some(p._2.head))).toMap)
    // the same changes for the markup
    val markupChanges: Map[Coords, Int] = forcedCells.map(p => (p._1, p._2.head)).toMap

    otherCells.minByOption(_._2.length) match {
      case None => LazyList(board2) // the board is filled
      case Some((c: Coords, nums: Vector[Int])) => {
        // pick a cell and try to put a number there, then attempt to solve this slightly easier board
        val tryOrder = if (shuffle) Random.shuffle(nums) else nums
        tryOrder.to(LazyList).flatMap({
          n => useOptimalAlgorithm(
            board = board2.withChanges(Map(c -> Some(n))),
            markup = Some(markup_.withCellsFilled(markupChanges ++ Map(c -> n))),
            shuffle = shuffle,
            onIteration = onIteration
          )
        })
      }
    }

  }


}
