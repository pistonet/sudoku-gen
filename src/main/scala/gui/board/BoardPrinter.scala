package gui.board

import logic._
import gui.ColorScheme
import scalafx.scene.layout._
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.beans.property._
import scalafx.scene.input.{KeyEvent, MouseEvent}

/**
 * Renders a Sudoku board using [[CellPrinter]]s.
 *
 * The size of this component changes based on the size of its board.
 *
 * @param board an '''ObjectProperty''' containing the '''BoardState''' to be printed
 * @param selection an '''ObjectProperty''' containing the '''Coords''' to the selected cell
 */
class BoardPrinter(val board: ObjectProperty[BoardState], val selection: ObjectProperty[Option[Coords]]) extends GridPane {

  background = new Background(Array(new BackgroundFill((ColorScheme.boardBackground), CornerRadii.Empty, Insets.Empty)))

  // padding on the outside of the cells
  padding = Insets(6)

  // padding between the cells
  val cellGap: Int = 3
  vgap = cellGap
  hgap = cellGap


  // clears old CellPrinters if there are any and generates new ones for a correctly sized board
  private def populateWithCells(): Unit = {
    this.children.clear()
    val cells = Vector.tabulate(board().sideLength,board().sideLength)((y,x) => new CellPrinter(Coords(x,y), board, selection))
    cells.flatten.foreach(cell => this.add(cell, cell.coords.x, cell.coords.y))
  }

  // initialize the board
  this.populateWithCells()

  // recreate the board if board size changes
  board.onChange({
    (_, oldBoard, newBoard) => {
      if (oldBoard.blockSideLength != newBoard.blockSideLength) {
        this.populateWithCells() // repopulate
      }
    }
  })


  // request focus when board clicked
  this.handleEvent(MouseEvent.MouseClicked) {
    e: MouseEvent => {
      this.requestFocus()
    }
  }

  // set selection to none on lost focus:
  this.focusedProperty().onChange((_,_,newValue) => {
    if (newValue == false) {
      selection() = None
    }
  })

  // handling of keyboard events:
  val keyboardHandler = new BoardKeyEventHandler(board, selection)
  onKeyPressed = (e: KeyEvent) => {
    keyboardHandler.handle(e)
  }

}
