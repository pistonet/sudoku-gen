package logic

/**
 * Represents the amount of solutions a sudoku has.
 */
sealed trait Solvability

/**
 * A sudoku that cannot be solved, either because it is not valid,
 * or because adding numbers will always result in a dead end.
 * */
case object Unsolvable extends Solvability

/** A sudoku that has a single unambiguous solution. */
case object SingleSolution extends Solvability

/** A sudoku that has more than 1 solution. */
case object MultipleSolutions extends Solvability
