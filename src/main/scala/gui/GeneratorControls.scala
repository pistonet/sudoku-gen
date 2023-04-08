package gui

import logic._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scalafx.scene.layout._
import scalafx.beans.property._
import scalafx.scene.control._
import scalafx.collections._
import scalafx.beans.binding._
import scalafx.application.Platform
import scalafx.scene.text.Text
import scalafx.geometry._
import scalafx.Includes._

/**
 * Renders a control panel that contains controls for generating Sudokus.
 * @param board the '''ObjectProperty''' the generated boards will be output to
 */
class GeneratorControls(val board: ObjectProperty[BoardState]) extends VBox {

  spacing = 5

  padding = Insets(5)

  background = new Background(Array(new BackgroundFill((ColorScheme.generatorBackground), CornerRadii.Empty, Insets.Empty)))

  // true if a sudoku is being currently generated
  val generating: BooleanProperty = BooleanProperty(false)

  // disable all the controls in this panel when currently generating
  disable <== generating

  val sizeSelector = new ChoiceBox[String] {
    items = ObservableBuffer[String]("9x9","16x16")
    // default value
    value = "9x9"
  }

  // the block side length for the generated sudoku
  val blockSideLength = IntegerProperty(3)

  blockSideLength <== Bindings.createIntegerBinding(
    () => sizeSelector.value() match {
      case "16x16" => 4
      case _ => 3 // default to 9x9
    },
    sizeSelector.value
  )

  val buttonGenerate = new Button {
    mnemonicParsing = true
    text <== when (generating.not()) choose "_Generate" otherwise "_Generating..."
    maxWidth = Double.MaxValue
    onAction = (event: javafx.event.ActionEvent) => {
      val bsl: Int = blockSideLength()

      generating() = true

      val newBoard: Future[BoardState] = Future {
        Generator.create(bsl)._1
      }

      newBoard.onComplete(t => {
        // run in scalafx application thread:
        Platform.runLater(() => {
          generating() = false
          t match {
            case Success(b) => board() = b
            case Failure(exception) => {} // doesn't really throw exceptions
          }
        })
      })

    }
  }

  val sizeSelectorBox = new HBox {
    val text = new Text {
      text = "New Board Size:"
    }
    val spacer = new Region {
      hgrow = Priority.Always
    }
    alignment = Pos.CenterLeft
    spacing = 3
    children = List(text,spacer,sizeSelector)
  }


  children = List(sizeSelectorBox,buttonGenerate)

}
