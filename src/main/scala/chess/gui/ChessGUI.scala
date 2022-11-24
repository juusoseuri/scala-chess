package chess.gui

import chess.ChessApp
import chess.game.Board
import chess.gui.DrawBoard

import scala.swing.*

/** ChessGUI extends MainFrame from Swing library
 * 
 * This objects main purpose is to establish the GUI. The
 * contents of the GUI are built in DrawBoard
 */
object ChessGUI extends MainFrame:
  val app = ChessApp
  val board = Board

  val panel = FlowPanel()
  panel.contents += DrawBoard
  contents = panel

  visible = true
