package chess.gui

import chess.ChessApp
import chess.components.{Piece, Square}
import chess.game.{Board, GameState}
import chess.gui.dialogs.{GameOverDialog, PromotionDialog}

import java.awt.Color.*
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D, *}
import javax.swing.ImageIcon
import scala.swing.*
import scala.swing.Swing.*
import scala.swing.event.*

/** DrawBoard draws the current state of the board
 *
 *  Board is redrawn everytime a piece is moving or moves
 *  DrawBoard extends a Swing component
 *
 *  Feel free to change the color of the squares
 */
object DrawBoard extends swing.Label {
  board=>

  // Feel free to change these colors
  val lightSquare = Color(212, 185, 148)
  val darkSquare = Color(139, 98, 71)
  val moveSquare = Color(70, 70, 70, 170)
  val currentSquare = Color(255, 255, 51, 100)

  // Here we calculate essential size values for drawing the board
  val squareSize = ChessApp.squareSize
  val moveIndicatorSize = (squareSize / 4.0).toInt
  val moveIndicatorCenterDiff = ((squareSize - moveIndicatorSize)/2.0).toInt

  val attackCircleSize = squareSize - (squareSize / 14.0).toInt
  val attackCircleCenterDiff = ((squareSize - attackCircleSize)/2.0).toInt

  // RenderPieces is called everytime a new "picture" of the board is needed
  // mouseLocation represents the current place where a piece is being held
  // if the mouse is not pressed the mouseLocation gets a value of (0,0)
  def renderPieces(mouseLocation: (Int, Int)) =
    // For every render establish a new empty image
    val pic = new BufferedImage(8*squareSize, 8*squareSize, BufferedImage.TYPE_INT_ARGB)
    icon = ImageIcon(pic)
    val g = pic.getGraphics.asInstanceOf[Graphics2D]
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    // Iterate through all the elements on the board
    for square <- Board.allElements do
      // Get the upper left corner for the square, this where it is drawn
      val (x, y) = (0 + square.column * squareSize, 0 + square.row * squareSize)

      // Set the color
      val color = (
        if square.isLight then
          lightSquare
        else
          darkSquare
        )
      g.setColor(color)

      // Check if the current square is in the possibleMoves buffer
      // if this is the case draw the right indicators
      if Board.possibleMoves.map(_.newSquare).contains(square) then
        g.fillRect(x, y, squareSize, squareSize)
        g.setColor(moveSquare)
        if Board.allPieces.exists(_.square == square) then
          g.setStroke(new BasicStroke(squareSize-attackCircleSize))
          g.drawOval(x + attackCircleCenterDiff, y + attackCircleCenterDiff, attackCircleSize, attackCircleSize)
        else
          val x_circle = x + moveIndicatorCenterDiff
          val y_circle = y + moveIndicatorCenterDiff
          g.fillOval(x_circle, y_circle, moveIndicatorSize, moveIndicatorSize)
      else
        g.fillRect(x, y, squareSize, squareSize)

    var dragginPiece: Option[Piece] = None
    for piece <- Board.allPieces do
      val (x, y) = (0 + piece.square.column * squareSize, 0 + piece.square.row * squareSize)
      if piece.moving then
        g.setColor(currentSquare)
        g.fillRect(x, y, squareSize, squareSize)
      if piece.dragging then
        dragginPiece = Some(piece)
      else
        g.drawImage(piece.image, x, y, null)

    dragginPiece match
      case Some(p) =>
        val center = (squareSize/2.0).toInt
        val newLoc = (mouseLocation._1-center, mouseLocation._2-center)
        g.drawImage(p.image, newLoc._1, newLoc._2, null)
      case None =>

  renderPieces((0,0))

  reactions += {
    case pressed: MousePressed => {
      val mouseLocation: (Int, Int) = (pressed.point.x, pressed.point.y)
      val square = Board.elementAt(Board.findPos(mouseLocation))
      Board.movePiece(mouseLocation)
      renderPieces(mouseLocation)
    }
    case dragged: MouseDragged => {
      val mouseLocation: (Int, Int) = (dragged.point.x, dragged.point.y)
      renderPieces(mouseLocation)
    }
    case released: MouseReleased => {
      val mouseLocation: (Int, Int) = (released.point.x, released.point.y)
      Board.dropSquare(mouseLocation)
      renderPieces((0,0))
      if Board.checkmate then
        val value = GameOverDialog.show("Shakkimatti").toString
        if value == "Ok" then
          Board.restart()
          renderPieces((0,0))
        else if value == "No" then
          System.exit(0)
        end if
      else if Board.draw then
        val value = GameOverDialog.show("Pattitilanne").toString
        if value == "Ok" then
          Board.restart()
          renderPieces((0,0))
        else if value == "No" then
          System.exit(0)
      else
        println(s"Turn ${GameState.turnNumber}, move: ${GameState.moveNumber}")
    }
  }

  listenTo(this.mouse.clicks)
  listenTo(this.mouse.moves)
}
