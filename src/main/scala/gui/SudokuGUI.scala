package gui

import gui.board.BoardPrinter
import logic._
import java.io._
import scala.util.{Failure, Success, Try}
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.beans.property._
import scalafx.stage.FileChooser
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.paint.Color._

/**
 * A ScalaFX GUI for solving and generating sudokus.
 */
object SudokuGUI extends JFXApp  {

  stage = new JFXApp.PrimaryStage {
    title.value = "SudokuGen"
    resizable = false
  }

  val root = new VBox
  val scene = new Scene(root)
  stage.scene = scene


  /** This ObjectProperty contains the currently rendered board. */
  val board: ObjectProperty[BoardState] = ObjectProperty(BoardState.createEmpty(3))
  /** This ObjectProperty contains Coords to the currently celected cell on the board. */
  val selection: ObjectProperty[Option[Coords]] = ObjectProperty(None)

  // create the menu bar at the top of the application
  val menu = new MenuBar {
    menus = List(
      new Menu("_File") {
        mnemonicParsing = true
        items = List(
          new MenuItem("_Import .csv") {
            mnemonicParsing = true
            onAction = () => importCsv()
          },
          new MenuItem("_Export .csv") {
            mnemonicParsing = true
            onAction = () => exportCsv()
          }
        )
      },
      new Menu("_Board") {
        mnemonicParsing = true
        items = List(
          new MenuItem("_Clear") {
            mnemonicParsing = true
            onAction = () => board() = BoardState.createEmpty(board().blockSideLength)
          },
          new Menu("_New") {
            mnemonicParsing = true
            items = List(
              new MenuItem("9x9") {
                onAction = () => board() = BoardState.createEmpty(3)
              },
              new MenuItem("16x16") {
                onAction = () => board() = BoardState.createEmpty(4)
              }
            )
          }
        )
      }
    )
  }


  // responsible for rendering the current board
  val boardPrinter = new BoardPrinter(board, selection)

  // resize the window when the board size changes
  boardPrinter.heightProperty().onChange(
    (_,_,_) => {
      stage.sizeToScene()
      // force a scene refresh
      // fixes a strange bug with scalafx on windows where the right edge of the window wouldn't render when the window grows
      root.setBackground(new Background(Array(new BackgroundFill((color(0.99,0.99,0.99)), CornerRadii.Empty, Insets.Empty))))
      root.setBackground(new Background(Array(new BackgroundFill((color(1,1,1)), CornerRadii.Empty, Insets.Empty))))
    }
  )

  val controls = new VBox {
    background = new Background(Array(new BackgroundFill((ColorScheme.applicationBackground), CornerRadii.Empty, Insets.Empty)))
    // space on the outside of the controls:
    padding = Insets(6)
    // space between different controls:
    spacing = 6


    children += new InfoBox(board)
    children += new SolverControls(board)
    children += new GeneratorControls(board)

    // acts as spacer, ensures that keypad is at bottom of the vbox
    children += new Region {
      vgrow = Priority.Always
    }

    // ensure that the keypad is in the rightmost corner:
    children += new HBox {
      children = List(
        new Region {
          // this region acts as a horizontal spacer
          hgrow = Priority.Always
        },
        new Keypad(board, selection)
      )
    }

  }

  // the main area represents everything in the window below the menu bar
  var mainArea = new HBox {
    children += boardPrinter
    children += controls
  }

  // add everything to the root VBox
  root.children += menu
  root.children += mainArea


  /**
   * Opens the a FileChooser dialog and attemps to parse a BoardState from the selected file.
   * Sets the current board to the parsed board on success.
   */
  private def importCsv(): Unit = {
    val fileChooser = new FileChooser() {
      title = "Import from .csv"
      extensionFilters += new FileChooser.ExtensionFilter("Comma-seperated values (.csv)", List("*.csv"))
    }

    val file: Option[File] = Option(fileChooser.showOpenDialog(stage))

    // if user selected file read from it:
    file.foreach(f => {
      var fr: FileReader = null
      var br: BufferedReader = null

      val parsedBoard: Try[Option[BoardState]] = Try {
        fr = new FileReader(f)
        br = new BufferedReader(fr)
        BoardState.parseFromCsv(br)
      }

      // try to close the streams
      try {
        if (br != null)
          br.close()
        if (fr != null)
          fr.close()
      } catch {
        case _: IOException =>
      }

      parsedBoard match {
        case Success(Some(b)) => {
          board() = b
          selection() = None
        }
        // file successfully read but parsing failed:
        case Success(None) => {
          val alert = new Alert(AlertType.Warning) {
            initOwner(stage)
            title = "Import Error"
            headerText = "Board parsing failed."
            contentText = s"No valid board was found in file ${f.getPath}."
          }
          alert.show()
        }
        // reader exception, couldn't open file,
        // getMessage gives meaningful enough feedback,
        // for FileNotFoundExceptions and IOExceptions,
        case Failure(e: Throwable) => {
          val alert = new Alert(AlertType.Error) {
            initOwner(stage)
            title = "Import Error"
            headerText = "Failed to open file."
            contentText = e.getMessage
          }
          alert.show()
        }
      }

    })
  }

  /**
   * Opens the a FileChooser dialog and attemps to export the current board to the selected file.
   */
  private def exportCsv(): Unit = {
    val fileChooser = new FileChooser() {
      title = "Export to .csv"
      initialFileName = "board.csv"
      extensionFilters += new FileChooser.ExtensionFilter("Comma-seperated values (.csv)", List("*.csv"))
    }

    val file: Option[File] = Option(fileChooser.showSaveDialog(stage))

    // if user selected file, try to write to it:
    file.foreach(f => {

      var fw: FileWriter = null
      var bw: BufferedWriter = null

      try {
        fw = new FileWriter(f)
        bw = new BufferedWriter(fw)
        bw.write(board().toCsv)
      } catch {
        // if an exception is thrown while writing, show a popup
        case e: Throwable => {
          val alert = new Alert(AlertType.Error) {
            initOwner(stage)
            title = "Export Error"
            headerText = "Error writing to file"
            contentText = e.getMessage
          }
          alert.show()
        }
      } finally {
        // attempt to close streams
        try {
          if (bw != null)
            bw.close()
          if (fw != null)
            fw.close()
        } catch {
          case _: IOException =>
        }
      }

    })

  }




}
