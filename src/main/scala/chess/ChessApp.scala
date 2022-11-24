package chess

import scala.swing.*
import o1.*
import chess.gui.ChessGUI

object ChessApp extends SimpleSwingApplication:  
  val squareSize = 70
  
  val top: MainFrame = ChessGUI
end ChessApp

