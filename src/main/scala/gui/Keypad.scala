package gui

import logic._
import scalafx.scene.layout._
import scalafx.beans.property._
import scalafx.geometry.Insets
import scalafx.Includes._
import scalafx.scene.shape.Rectangle
import scalafx.beans.binding._
import scalafx.scene.text._
import scalafx.scene.input.MouseEvent

/**
 * Renders a keypad for inputting numbers to a Sudoku board.
 *
 * The keys for numbers that cannot be input into the selected cell on
 * the board (i.e. would cause the board to become incorrect)
 * are rendered with a different coloured background.
 *
 * The size of this component changes based on the size of the board it edits.
 *
 * @param board an '''ObjectProperty''' containing the '''BoardState''' to be edited
 * @param selection an '''ObjectProperty''' containing the '''Coords''' to the selected cell
 */
class Keypad(val board: ObjectProperty[BoardState], val selection: ObjectProperty[Option[Coords]]) extends GridPane {

  background = new Background(Array(new BackgroundFill((ColorScheme.boardBackground), CornerRadii.Empty, Insets.Empty)))

  // padding on the outside of the keys
  padding = Insets(6)

  // padding between the keys
  val keyGap: Int = 3
  vgap = keyGap
  hgap = keyGap

  // the numbers that can be input into the selected cell:
  val possibleNumbers: ObjectBinding[Vector[Int]] = Bindings.createObjectBinding(
    () => selection() match { // value:
      case None => Vector.from(1 to board().sideLength)
      case Some(coords) => board().possibleNumbers(coords)
    },
    board, selection // dependencies
  )



  private def populateWithKeys(): Unit = {
    this.children.clear()
    val lines: Iterator[Vector[Int]] = Vector.from(1 to board().sideLength).grouped(board().blockSideLength)
    for ((line, y) <- lines.zipWithIndex; (keyNumber, x) <- line.zipWithIndex) {
      this.add(new KeyPrinter(keyNumber), x, y)
    }
  }

  // initialize
  this.populateWithKeys()

  // repopulate when board size changes
  board.onChange({
    (_, oldBoard, newBoard) => {
      if (oldBoard.blockSideLength != newBoard.blockSideLength) {
        this.populateWithKeys()
      }
    }
  })

  /**
   * A key on the keypad.
   * @param number the number on this key.
   */
  private class KeyPrinter(val number: Int) extends StackPane {

    val keySize: Int = 50

    private val isIncorrect: BooleanBinding = Bindings.createBooleanBinding(
      () => !possibleNumbers().contains(this.number),
      possibleNumbers
    )

    val backdrop = new Rectangle {
      fill <== when (isIncorrect) choose ColorScheme.cellBackgroundIncorrect otherwise ColorScheme.cellBackground
      width = keySize
      height = keySize
    }

    val text = new Text {
      text = Alphabet(number)
      style = "-fx-font-family: Tahoma, sans-serif; -fx-font-size: 28px"
    }

    children = List(backdrop, text)

    // when clicked input this number into the selected cell
    this.handleEvent(MouseEvent.MouseClicked) {
      e: MouseEvent => {
        // if something is selected:
        selection().foreach(coords => {
          board() = board().withChanges(Map(coords -> Some(this.number)))
        })
      }
    }

  }



}
