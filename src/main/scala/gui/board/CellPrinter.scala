package gui.board

import logic._
import gui.{Alphabet, ColorScheme}
import scalafx.Includes._
import scalafx.beans.binding._
import scalafx.scene.layout._
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import scalafx.scene.paint.Color._
import scalafx.beans.property._
import scalafx.geometry.Insets
import scalafx.scene.input.MouseEvent
import scala.util.Try

/**
 * Prints a cell on a sudoku board.
 *
 * Selected cells will be printed with a different coloured background.
 *
 * @param coords the '''Coords''' of this cell on its board
 * @param board an '''ObjectProperty''' containing the '''BoardState''' this cell gets its number from
 * @param selection an '''ObjectProperty''' containing the '''Coords''' to the selected cell
 */
class CellPrinter(
  val coords: Coords,
  val board: ObjectProperty[BoardState],
  val selection: ObjectProperty[Option[Coords]]
) extends StackPane {

  private val isSelected: BooleanBinding = Bindings.createBooleanBinding(
    () => selection().contains(this.coords), // value
    selection // dependencies
  )

  val cellSize: Int = 50

  val backdrop = new Rectangle {
    fill <== when (isSelected) choose ColorScheme.cellBackgroundSelected otherwise ColorScheme.cellBackground
    width = cellSize
    height = cellSize
  }

  private val textContent: StringBinding = Bindings.createStringBinding(
    () => { // value of binding:
      // when changing board size from bigger to smaller this location
      // may not be on the board for a split second,
      // wrap in Try to avoid throwing out of bound exceptions
      val cell: Option[Int] = Try(board()(this.coords)).toOption.flatten
      cell.map(Alphabet(_)).getOrElse("")
    },
    board // dependencies for binding
  )

  val text = new Text {
    text <== textContent
    style = "-fx-font-family: Tahoma, sans-serif; -fx-font-size: 28px"
  }

  // add extra spacing between the blocks of the sudoku
  // there is no need to create a binding for this since cells will be re-created when the board size is changed
  private val extraPaddingAmount: Int = 3
  private val extraOnLeft = this.coords.x != 0 && this.coords.x % board().blockSideLength == 0
  private val extraAbove  = this.coords.y != 0 && this.coords.y % board().blockSideLength == 0
  padding = {
    Insets(
      if (extraAbove) extraPaddingAmount else 0,
      0, // right
      0, // bottom
      if (extraOnLeft) extraPaddingAmount else 0
    )
  }


  // select this cell when clicked
  this.handleEvent(MouseEvent.MouseClicked) {
    e: MouseEvent => {
      selection() = Some(this.coords)
    }
  }

  children = List(backdrop, text)

}
