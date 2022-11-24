package chess.components

import o1.*
import chess.game.Board

import scala.collection.mutable.Buffer

/** Represents a single square in the grid
 *
 * @param column the column of the square
 * @param row the row of the square
 * @param isLight the color of the square
 */
class Square(val column: Int, val row: Int, val isLight: Boolean):

  val position = GridPos(column, row)

  def piece = Board.allPieces.find(_.position == position)

  private val n = row
  private val e = 7 - column
  private val s = 7 - row
  private val w = column

  // Tells how many squares is to each edge
  // Values are read in the clockwise direction starting from north and including diagonals
  val squaresToEdge = Buffer(
    n,
    math.min(n, e),
    e,
    math.min(e, s),
    s,
    math.min(s, w),
    w,
    math.min(n, w),
  )
