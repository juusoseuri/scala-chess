package chess.components

import chess.game.*
import o1.GridPos

/** Move represents a single possible move for a piece
 *
 * Most important variable is the isLegal, that checks
 * if the proposed move can be done without letting the opponent
 * to eat the king.
 *
 * Test is done by doing the move temporarily and then checking whether
 * the threatmap contains the king. If this is the case isLegal returns false,
 * otherwise true. Finally undo the move so that the state of the game doesn't change
 *
 * This class does not represent a ready move made on the board but
 * rather a proposition of a move
 *
 * @param piece the piece that is going to be moving
 * @param newPosition the new square where the piece is possible going to move
 */
class Move(val piece: Piece, val newPosition: GridPos):
  val oldSquare = piece.square
  val newSquare = Board.elementAt(newPosition)
  private val newSquarePiece = newSquare.piece

  // Temporarily do the move
  piece.position = newPosition
  newSquarePiece match
    case Some(p) => Board.allPieces = Board.allPieces.filter(_ != p)
    case None =>

  // Increase the move and turn numbers
  if GameState.blackToMove() then GameState.turnNumber += 1
  GameState.moveNumber += 1

  // Get new threat map
  private val map = GameState.threatMap

  // Determines whether this move puts the king in danger and is therefore illegal move
  val isLegal: Boolean = (
    !map.exists( pos =>
      pos.piece match
        case Some(p) =>
          p match {
            case _: King if p.color == piece.color => true
            case _ => false
          }
        case None => false
    )
  )
  
  // Undo the move
  piece.position = oldSquare.position
  newSquarePiece match
    case Some(p) => Board.allPieces = Board.allPieces :+ p
    case None =>

  if GameState.whiteToMove() then GameState.turnNumber -= 1
  GameState.moveNumber -= 1
