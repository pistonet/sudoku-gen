package gui

import logic._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success,Failure}
import scalafx.application.Platform
import scalafx.scene.layout._
import scalafx.beans.property._
import scalafx.beans.binding._
import scalafx.geometry.Insets
import scalafx.scene.text._
import scalafx.scene.control._
import scalafx.Includes._
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType

/**
 * Renders a control panel that contains controls for generating Sudokus.
 * @param board an '''ObjectProperty''' containing the '''BoardState''' to be solved and edited
 */
class SolverControls(val board: ObjectProperty[BoardState]) extends VBox {

  // the upper limit of solutions to calculate when approximating the solution amount
  private val solutionCalculationMax: Int = 50

  spacing = 5

  padding = Insets(5)

  background = new Background(Array(new BackgroundFill((ColorScheme.solverBackground), CornerRadii.Empty, Insets.Empty)))

  // not the best solution, would be better if
  // some sort of custom bidirectional bindings could be created,
  // or the source of the change in a property could be detected in an onChange
  private var editingBoard: Boolean = false
  private def softEditBoard(newBoard: BoardState) = {
    editingBoard = true
    board() = newBoard
    editingBoard = false
  }

  // this LazyList contains the solutions to the current board
  val solutions: ObjectProperty[LazyList[BoardState]] = ObjectProperty(Solver.solve(board()))

  // contains the index of the solution currently on view, and the original sudoku the solution is for
  val solutionOnView: ObjectProperty[Option[(Int,BoardState)]] = ObjectProperty(None)

  // if board is edited from outside this component, set
  board.onChange((_,_,newBoard) => {
    if (!editingBoard) {
      solutions() = Solver.solve(newBoard)
      solutionTextContent() = solutionTextDefault
      solutionOnView() = None
    }
  })


  private val solutionTextDefault = "Not calculated..."
  val solutionTextContent = new StringProperty(solutionTextDefault)


  val solutionText = new Text {
    text <== Bindings.createStringBinding(() => "Solution count:\n") + solutionTextContent
  }

  val buttonApproximate = new Button {
    mnemonicParsing = true
    text = "_Approximate solutions"
    maxWidth = Double.MaxValue

    // enable only when solution text shows default ("not calculated"):
    disable <== solutionTextContent =!= solutionTextDefault

    onAction = (event) => {

      solutionTextContent() = "Calculating..."

      val newSolutionText: Future[String] = Future {
        solutions().lengthIs match {
          case l if (l == 0)                      => "No solutions."
          case l if (l == 1)                      => "Exactly one solution."
          case l if (l <= solutionCalculationMax) => s"Exactly ${solutions().length} solutions."
          case _                                  => s"More than ${solutionCalculationMax} solutions."
        }
      }

      newSolutionText.onComplete(t => {
        // run in scalafx application thread:
        Platform.runLater(() => {
          t match {
            case Success(text) => solutionTextContent() = text
            case Failure(exception) => // doesn't realy throw exceptions
          }
        })
      })

    }
  }

  val buttonOriginal = new Button {
    mnemonicParsing = true
    text = "View _Original"

    // enable only when original sudoku is defined
    disable <== Bindings.createBooleanBinding(
      () => solutionOnView().isEmpty, // value
      solutionOnView // dependencies
    )

    onAction = (event) => {
      // if original exists
      solutionOnView().foreach(p => {
        solutionOnView() = None
        softEditBoard(p._2)
      })
    }
  }

  val buttonPrev = new Button {
    mnemonicParsing = true
    text = "_Previous"

    // disable when viewing first solution or not viewing any solution
    disable <== Bindings.createBooleanBinding(
      () => solutionOnView().forall(_._1 == 0), // value
      solutionOnView // dependencies
    )

    onAction = (event) => {
      goToSolution(-1)
    }
  }
  val buttonNext = new Button {
    mnemonicParsing = true
    text = "_Next"
    onAction = (event) => {
      goToSolution(1)
    }
  }

  // acts as a spacer between the original and previous buttons
  // negative margin is to ensure that when there is no extra space,
  // this gap is the same as the gap between the "previous" and "next" buttons
  val buttonSpacer = new Region {
    // negative margin on the right, same amount as padding on the HBox this will go into
    margin = Insets(0,-5,0,0)
    hgrow = Priority.Always
  }

  val navigationButtons = new HBox {
    spacing = 5
    children = List(buttonOriginal, buttonSpacer, buttonPrev, buttonNext)
  }


  val viewSolutionText = new Text{
    text <== Bindings.createStringBinding(
      () => solutionOnView().map(p => s"Viewing solution ${p._1+1}").getOrElse("View Solution:"),
      solutionOnView
    )
  }

  children = List(solutionText, buttonApproximate, viewSolutionText, navigationButtons)



  /**
   * Views the solution '''shift''' away from the current solution,
   * if no current solution is being viewed, views first solution.
   *
   * If no solution with the new index exists, does nothing.
   *
   * `goToSolution(1)` will view next solution,
   * `goToSolution(-1)` will view previous
   *
   * @param shift the difference `nextSolution - currentSolution`
   */
  private def goToSolution(shift: Int) = {
    val newIndex = solutionOnView().map(_._1 + shift).getOrElse(0)

    val nextSolution: Future[Option[BoardState]] = Future {
      solutions().lift(newIndex)
    }

    nextSolution.onComplete(t => {
      // run in scalafx application thread:
      Platform.runLater(() => {
        t match {
          case Success(Some(newSolution)) => {
            val originalBoard = solutionOnView().map(_._2).getOrElse(board())
            solutionOnView() = Some((newIndex,originalBoard))
            softEditBoard(newSolution)
          }
          case Success(None) => {
            val stage = this.getScene.getWindow
            val alert = new Alert(AlertType.Information) {
              initOwner(stage)
              title = "Info"
              headerText = "No next solution"
              contentText = s"Solution number ${newIndex + 1} doesn't exist."
            }
            alert.show()
          }
          case Failure(exception) => throw exception // doesn't throw exceptions...
        }
      })
    })

  }



}
