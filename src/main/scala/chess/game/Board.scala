package chess.game

import chess.ChessApp
import chess.components.*
import chess.gui.DrawBoard
import chess.gui.dialogs.{GameOverDialog, PromotionDialog}
import o1.grid.*

import scala.collection.mutable.Buffer
import scala.language.postfixOps

/** Board represents a single gameboard
 *
 *  Contains information on the squares and pieces on the board.
 *  Is also responsible for initialising the board
 */
object Board extends Grid[Square](8,8):

  var allPieces = Vector[Piece]()
  var checkmate = false
  var draw      = false
  var whiteKing = King(White, GridPos(4, 0))
  var blackKing = King(Black, GridPos(4, 7))
  val startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  loadPositionFromFen(startFen)

  def whitePieces = getPieces(allElements).filter(_.isWhite)
  def blackPieces = getPieces(allElements).filter(!_.isWhite)
  var possibleMoves = Vector[Move]()

  def restart() =
    loadPositionFromFen(startFen)
    GameState.moveNumber = 1
    GameState.turnNumber = 1
    checkmate = false
    draw      = false

  override def initialElements: Seq[Square] =
    allPieces = Vector[Piece]()
    var squares = Vector[Square]()
    for row <- 0 until 8 do
      for col <- 0 until 8 do
        squares = squares :+ Square(col, row, (col + row) % 2 != 1)
    squares.toSeq

  /** Loads a position from a FEN string
   *
   * Can be used to download different positions
   *
   * @param fen Forsyth–Edwards Notation: https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
   */
  def loadPositionFromFen(fen: String) =
    allPieces = Vector[Piece]()
    var pieceColor: PieceColor = White
    var piecePos = GridPos(0,0)
    def pieceTypeFromSymbol = Map(
      'k' -> King(pieceColor, piecePos), 'p' -> Pawn(pieceColor, piecePos), 'n' -> Knight(pieceColor, piecePos),
      'b' -> Bishop(pieceColor, piecePos), 'r' -> Rook(pieceColor, piecePos), 'q' -> Queen(pieceColor, piecePos)
    )

    val fenBoard = fen.split(' ')(0)
    var file = 0
    var rank = 0

    for symbol <- fenBoard do
      if symbol == '/' then
        file = 0
        rank += 1
      else
        if symbol.isDigit then
          file += symbol.asDigit
        else
          piecePos = GridPos(file, rank)
          pieceColor = if symbol.isUpper then White else Black
          val pieceType = pieceTypeFromSymbol(symbol.toLower)
          if pieceColor.isWhite && pieceType.isInstanceOf[King] then whiteKing = pieceType.asInstanceOf[King]
          else if !pieceColor.isWhite && pieceType.isInstanceOf[King] then blackKing = pieceType.asInstanceOf[King]
          allPieces = allPieces :+ pieceType
          file += 1
  end loadPositionFromFen

  /** Gets all the pieces from the set of squares
   *
   * @param squares the squares of which pieces you want to get
   * @return
   */
  def getPieces(squares: Vector[Square]): Vector[Piece] = allPieces.filter(p => squares.contains(p.square))

  /** Finds a square from coordinates
   *
   * @param xy the coordinates in pixels
   * @return Some(Square) if the coordinates were valid, otherwise None
   */
  def findPos(xy: (Int, Int)): GridPos =
    val (x, y) = (xy._1, xy._2)
    val squareSize = ChessApp.squareSize
    GridPos( (x.toDouble/squareSize).floor.toInt, (y.toDouble/squareSize).floor.toInt )
  end findPos

  /** Initializes the movement of a piece
   *
   * Method is called when the piece is first time touched
   *
   * @param xy Piece coordinates in pixels
   */
  def movePiece(xy: (Int, Int)) =
    val pos = findPos(xy)
    if allPieces.map(_.position).contains(pos) then
      if !possibleMoves.map(_.newPosition).contains(pos) then
        val piece = allPieces.filter(_.position == pos).head
        allPieces.foreach(_.moving = false)
        piece.moving = true
        piece.dragging = true
        possibleMoves = Vector()
        possibleMoves = GameState.possibleMoves(piece)
  end movePiece

  /** Determines whether the piece can be dropped in this location
   *
   * Checks if the desired square is valid. If not, return the piece
   * to the original square
   *
   * @param xy Piece coordinates in pixels
   */
  def dropSquare(xy: (Int, Int)) =
    val newPos = findPos(xy)
    val piece = allPieces.find(_.moving)
    if this.contains(newPos) then
      val newSquare = this.elementAt(newPos)
      piece match
        case Some(piece) =>
          val moveToDo = possibleMoves.filter(move => move.newSquare == newSquare)
          if moveToDo.nonEmpty then
            executeMove(moveToDo.head)
            possibleMoves = Vector()
        case None =>
    piece match
      case Some(piece) =>
        piece.dragging = false
      case None =>
  end dropSquare

  /** Handles the movement on board
   *
   * @param move the move that is going to be executed
   */
  def executeMove(move: Move) =
    var placedPiece = move.piece
    val newSquare = move.newSquare
    val oldSquare = move.oldSquare
    var isCheck = false

    newSquare.piece match
      case Some(oldPiece) =>
        allPieces = allPieces.filter(_ != oldPiece)
      case None =>

    placedPiece match
      case king: King =>
        // Check if king moved for the first time
        if !king.hasMoved then king.hasMoved = true

        // Check if the move was a castle
        val oldLoc = oldSquare.position
        if newSquare.position.x + 2 == oldLoc.x then
          val leftRookPos = elementAt(GridPos(oldLoc.x - 4, oldLoc.y))

          leftRookPos.piece match
            case Some(rook) => rook.position = (GridPos(oldLoc.x - 1, oldLoc.y))
            case None =>

        else if newSquare.position.x - 2 == oldLoc.x then
          val rightRookPos = elementAt(GridPos(oldLoc.x + 3, oldLoc.y))

          rightRookPos.piece match
            case Some(rook) => rook.position = (GridPos(oldLoc.x + 1, oldLoc.y))
            case None =>

      // Check if rook moved for the first time
      case rook: Rook => if !rook.hasMoved then rook.hasMoved = true
      case pawn: Pawn =>

        // Check for en passant and remove the pawn if en passant
        val newLoc = newSquare.position
        val yDiff = if placedPiece.isWhite then 1 else -1
        val behindLoc = GridPos(newLoc.x, newLoc.y + yDiff)
        if this.contains(behindLoc) then
          val behindSquare = elementAt(behindLoc)
          behindSquare.piece match
            case Some(piece: Piece) =>
              piece match {
                case pawn: Pawn =>
                  if pawn.doubleMoveDone then
                    allPieces = allPieces.filter(_.position != behindLoc)
                case _ =>
              }
            case None =>

        // Clear the old double moves
        allPieces.foreach( x =>
          x match
            case pawn: Pawn =>
              if pawn != placedPiece.asInstanceOf[Pawn] then
                pawn.doubleMoveDone = false
              else
                if math.abs(newSquare.row - oldSquare.row) == 2 then
                  pawn.doubleMoveDone = true
                else pawn.doubleMoveDone = false
            case _ =>
        )

        // Promotion
        if (newLoc.y == 7 && !placedPiece.isWhite) || (newLoc.y == 0 && placedPiece.isWhite) then
          val value = PromotionDialog.show
          value match
            case Some(entry) =>
              var option: Piece = Queen(placedPiece.color, placedPiece.position)
              entry match
                case "Knight" => option = Knight(placedPiece.color, placedPiece.position)
                case "Bishop" => option = Bishop(placedPiece.color, placedPiece.position)
                case "Rook"   => option = Rook(placedPiece.color, placedPiece.position)
                case _        =>
              allPieces = allPieces.filter(_ != placedPiece)
              allPieces = allPieces :+ option
              placedPiece = option
            case None =>
      case _ =>

    // Set the piece to its new square
    placedPiece.position = newSquare.position

    // Check if the move was a check
    val opponentKing = if placedPiece.isWhite then blackKing else whiteKing
    if GameState.possibleLocations(placedPiece).contains(opponentKing.position) then
      isCheck = true
      println("Check")
      opponentKing.isInCheck = true
    else opponentKing.isInCheck = false

    // Increase the move and turn numbers
    if GameState.blackToMove() then GameState.turnNumber += 1
    GameState.moveNumber += 1

    // Check if the move is a checkmate
    if opponentKing.isInCheck then
      val pieces = getPieces(allElements).filter(_.color != placedPiece.color)
      var allMoves = Vector[Move]()
      var i = 0
      while allMoves.isEmpty && i < pieces.length do
        GameState.possibleMoves(pieces(i)).foreach(move =>
          while allMoves.isEmpty do allMoves = allMoves :+ move)
        i += 1
      if allMoves.isEmpty then
        val king = pieces.find(_.isInstanceOf[King])
        val isInCheck = (
        king match
          case Some(k) => k.asInstanceOf[King].isInCheck
          case None => false
        )
        if isInCheck then
          checkmate = true
        else
          draw = true

  end executeMove

end Board
