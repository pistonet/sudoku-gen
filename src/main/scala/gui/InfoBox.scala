package gui

import logic._
import scalafx.scene.layout._
import scalafx.scene.text._
import scalafx.beans.property._
import scalafx.beans.binding._
import scalafx.Includes._
import scalafx.geometry.Insets

/**
 * Renders a small panel that show information about a Sudoku board.
 * @param board an '''ObjectProperty''' containing the '''BoardState''' to show info about
 */
class InfoBox(val board: ObjectProperty[BoardState]) extends VBox {

  spacing = 5

  padding = Insets(5)

  background = new Background(Array(new BackgroundFill((ColorScheme.infoBoxBackground), CornerRadii.Empty, Insets.Empty)))

  children = List(
    new Text {
      text <== Bindings.createStringBinding(
        () => s"Current ${board().sideLength}x${board().sideLength} board:",
        board
      )
    },
    new TextFlow {
      children = List(
        new Text {
          text = "Valid: "
        },
        new Text {
          text <== Bindings.createStringBinding(
            () => if (board().isValid) "yes" else "no",
            board
          )
          style = "-fx-font-weight: bold"
        }
      )
    },
    new TextFlow {
      children = List(
        new Text {
          text = "Filled: "
        },
        new Text {
          text <== Bindings.createStringBinding(
            () => f"${board().filledPercentage}%1.1f%%",
            board
          )
          style = "-fx-font-weight: bold"
        }
      )
    }
  )

}
