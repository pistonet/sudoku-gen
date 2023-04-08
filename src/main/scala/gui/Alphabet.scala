package gui

import scala.collection.BuildFrom

/**
 * This object contains the symbols used for displaying Sudoku boards,
 * they can be accessed via the apply method:
 *
 * {{{
 *   scala> Alphabet(4)
 *   val res0: String = 4
 *
 *   scala> Alphabet(Vector.from(1 to 16))
 *   val res1: scala.collection.immutable.Vector[String] = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F, G)
 * }}}
 */
object Alphabet {

  // if this map doesn't contain a symbol for a number, number.toString will be used
  private val exceptions: Map[Int,String] = Map(10 -> "A", 11 -> "B", 12 -> "C", 13 -> "D", 14 -> "E", 15 -> "F", 16 -> "G")

  /**
   * Returns the symbol for a number.
   * @param number a number on a sudoku board
   * @return the symbol corresnponding to the number
   */
  def apply(number: Int): String = {
    exceptions.getOrElse(number, number.toString)
  }

  /**
   * Returns the symbols for an iterable of numbers.
   * @param numbers iterable of numbers
   * @tparam T type of source collection
   * @tparam U type of destination collection
   * @return iterable of corresponding symbols
   */
  def apply[T <: Iterable[Int], U](numbers: T)(implicit bf: BuildFrom[T, String, U]): U = {
    bf.fromSpecific(numbers)(numbers.map(n => this.apply(n)))
  }

}
