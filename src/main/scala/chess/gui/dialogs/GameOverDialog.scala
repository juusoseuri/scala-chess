package chess.gui.dialogs

import chess.gui.ChessGUI

import scala.swing.Dialog
import scala.swing.Swing.{EmptyIcon, LineBorder}

object GameOverDialog:
  private val popup = Dialog

  private val options = Vector(
    "Aloita peli alusta",
    "Sulje sovellus",
    "Peruuta"
  )

  def show(situation: String) =
    popup.showOptions(ChessGUI,
      s"$situation, voit aloittaa pelin alusta tai sulkea sovelluksen",
      "",
      popup.Options.YesNoCancel,
      popup.Message.Info,
      EmptyIcon,
      options,
      0
    )