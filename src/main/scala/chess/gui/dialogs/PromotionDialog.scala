package chess.gui.dialogs

import chess.gui.ChessGUI

import scala.swing.Dialog
import scala.swing.Swing.EmptyIcon

/** Adds the dialog where you can choose the promotion item
 *
 *  PromotionDialog.show adds a multiple choice dialog which
 *  returns Option[String] where the string is the option selected
 */
object PromotionDialog:
  private val popup = Dialog

  private val options = Vector[String]("Queen", "Knight", "Bishop", "Rook")

  def show =
    popup.showInput(ChessGUI,
      "Choose promotion",
      "",
      popup.Message.Question,
      EmptyIcon,
      options,
      0
    )
