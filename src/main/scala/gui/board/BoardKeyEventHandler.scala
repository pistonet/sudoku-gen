package gui.board

import logic._
import scalafx.beans.property.ObjectProperty
import scalafx.scene.input.{KeyCode, KeyEvent}

/**
 * Handles keyboard events for a [[BoardPrinter]] via the [[handle]] method.
 * @param board an '''ObjectProperty''' containing the '''BoardState''' to be edited
 * @param selection an '''ObjectProperty''' containing the '''Coords''' to the selected cell
 */
class BoardKeyEventHandler(val board: ObjectProperty[BoardState], val selection: ObjectProperty[Option[Coords]]) {

  /**
   * Attempts to handle a KeyEvent. If the event is successfully handled, consumes the event.
   * @param event event to be handled
   */
  def handle(event: KeyEvent): Unit = {

    // assume event was handled, set to false if not
    var handled = true

    event.code match {
      // moving selection:
      case KeyCode.Right => moveSelection(1,0)
      case KeyCode.Left  => moveSelection(-1,0)
      case KeyCode.Up    => moveSelection(0,-1)
      case KeyCode.Down  => moveSelection(0,1)
      // clearing cell:
      case KeyCode.BackSpace | KeyCode.Delete => setSelectedCell(None)
      // typing into cell:
      case KeyCode.Digit1 | KeyCode.Numpad1 => setSelectedCell(Some(1))
      case KeyCode.Digit2 | KeyCode.Numpad2 => setSelectedCell(Some(2))
      case KeyCode.Digit3 | KeyCode.Numpad3 => setSelectedCell(Some(3))
      case KeyCode.Digit4 | KeyCode.Numpad4 => setSelectedCell(Some(4))
      case KeyCode.Digit5 | KeyCode.Numpad5 => setSelectedCell(Some(5))
      case KeyCode.Digit6 | KeyCode.Numpad6 => setSelectedCell(Some(6))
      case KeyCode.Digit7 | KeyCode.Numpad7 => setSelectedCell(Some(7))
      case KeyCode.Digit8 | KeyCode.Numpad8 => setSelectedCell(Some(8))
      case KeyCode.Digit9 | KeyCode.Numpad9 => setSelectedCell(Some(9))
      case KeyCode.A => setSelectedCell(Some(10))
      case KeyCode.B => setSelectedCell(Some(11))
      case KeyCode.C => setSelectedCell(Some(12))
      case KeyCode.D => setSelectedCell(Some(13))
      case KeyCode.E => setSelectedCell(Some(14))
      case KeyCode.F => setSelectedCell(Some(15))
      case KeyCode.G => setSelectedCell(Some(16))
      // event not handled:
      case _ => {
        handled = false
      }
    }

    if (handled) {
      event.consume()
    }

  }

  /**
   * Moves the selection to a new location relative to the old selection.
   * @param xShift how many cells to move horizontally
   * @param yShift how many cells to move vertically
   */
  private def moveSelection(xShift: Int, yShift: Int): Unit = {
    if (selection().isDefined) {
      val currentSelection = selection().get
      val newSelection = Coords(currentSelection.x + xShift, currentSelection.y + yShift)
      // only move selection if the new selection is on the board
      if (board().containsLocation(newSelection)) {
        selection() = Some(newSelection)
      }
    }
  }

  /**
   * If a cell is selected, sets its contents to a new value.
   * @param newContent the new contents of the cell
   */
  private def setSelectedCell(newContent: Option[Int]): Unit = {
    // only set if selected, and the current board can take that number
    if (selection().isDefined && newContent.forall(_ <= board().sideLength)) {
      board() = board().withChanges(Map(selection().get -> newContent))
    }
  }

}
