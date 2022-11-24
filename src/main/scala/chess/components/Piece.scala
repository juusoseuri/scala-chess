package chess.components

import chess.ChessApp
import chess.components.*
import chess.game.Board
import o1.GridPos

import java.awt.Image
import java.io.File
import javax.imageio.ImageIO

/** PieceColor determines the color of a piece
 *
 *  It has one boolean variable "isWhite" that returns
 *  true when the piece is white, false otherwise
 */
trait PieceColor:
  val isWhite: Boolean
object Black extends PieceColor:
  val isWhite = false
  override def toString: String = "Black"
object White extends PieceColor:
  val isWhite = true
  override def toString: String = "White"

/** Abstract class Piece represents a single piece
 *
 *  Below this abstract class is a concrete class for
 *  every piece. Piece contains information on the location,
 *  the color, and the image path of the piece
 *
 *  Additionally, the moving and dragging variables are used
 *  to find moving pieces. Moving means that the piece is currently
 *  moved -> the possible moves can be seen in the GUI.
 *  Dragging means that the piece is being held by the player at the moment
 *  this variable enables the image to be rendered everytime the piece is
 *  being dragged
 *
 */
abstract class Piece {

  var imagePath = "pictures/pieces"

  def getImage(path: String) = ImageIO.read(new File(path))
                                      .getScaledInstance(ChessApp.squareSize, ChessApp.squareSize, Image.SCALE_DEFAULT)

  val color: PieceColor

  val image: Image

  def square: Square = Board.elementAt(position)

  def isWhite = color.isWhite

  var moving = false

  var dragging = false

  var position: GridPos
}

class King(val color: PieceColor, var position: GridPos) extends Piece:
  if isWhite then
    imagePath += "/WhiteKing.png"
  else
    imagePath += "/BlackKing.png"

  val image = getImage(imagePath)

  var isInCheck = false
  // For castling
  var hasMoved = false

  override def toString: String = s"King-${color.toString}: (${position.x},${position.y})"

class Queen(val color: PieceColor, var position: GridPos) extends Piece:
  if isWhite then
    imagePath += "/WhiteQueen.png"
  else
    imagePath += "/BlackQueen.png"

  val image = getImage(imagePath)

  override def toString: String = s"Queen-${color.toString}: (${position.x},${position.y})"

class Rook(val color: PieceColor, var position: GridPos) extends Piece:
  if isWhite then
    imagePath += "/WhiteRook.png"
  else
    imagePath += "/BlackRook.png"

  val image = getImage(imagePath)

  // For castling
  var hasMoved = false

  override def toString: String = s"Rook-${color.toString}: (${position.x},${position.y})"

class Bishop(val color: PieceColor, var position: GridPos) extends Piece:
  if isWhite then
    imagePath += "/WhiteBishop.png"
  else
    imagePath += "/BlackBishop.png"

  val image = getImage(imagePath)

  override def toString: String = s"Bishop-${color.toString}: (${position.x},${position.y})"

class Knight(val color: PieceColor, var position: GridPos) extends Piece:
  if isWhite then
    imagePath += "/WhiteKnight.png"
  else
    imagePath += "/BlackKnight.png"

  val image = getImage(imagePath)

  override def toString: String = s"Knight-${color.toString}: (${position.x},${position.y})"

class Pawn(val color: PieceColor, var position: GridPos) extends Piece:
  if isWhite then
    imagePath += "/WhitePawn.png"
  else
    imagePath += "/BlackPawn.png"

  val image = getImage(imagePath)

  // for en passant
  var doubleMoveDone = false

  override def toString: String = s"Pawn-${color.toString}: (${position.x},${position.y})"