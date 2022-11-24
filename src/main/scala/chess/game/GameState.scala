package chess.game

import chess.components.*
import chess.game.Board
import chess.game.GameState.*
import o1.*

object GameState:
  var turnNumber = 1
  var moveNumber = 1

  def whiteToMove(): Boolean = moveNumber % 2 == 1

  def blackToMove(): Boolean = moveNumber % 2 == 0

  def possibleLocations(movingPiece: Piece): Vector[GridPos] =
    val pos = movingPiece.position
    val currentSquare = movingPiece.square
    val isWhite = movingPiece.isWhite

    def isTurn = (isWhite && whiteToMove()) || (!isWhite && !whiteToMove())

    var locations = Vector[GridPos]()

    if movingPiece.isInstanceOf[Bishop] || movingPiece.isInstanceOf[Queen] || movingPiece.isInstanceOf[Rook] then
      if isTurn then locations = slidingMoves(movingPiece, false)

    if movingPiece.isInstanceOf[King] && isTurn then
      locations = kingMoves(movingPiece, false)
      locations = locations ++ castling(movingPiece, false)

    if movingPiece.isInstanceOf[Knight] && isTurn then
      locations = knightMoves(movingPiece, false)

    if movingPiece.isInstanceOf[Pawn] && isTurn then
      locations = pawnMoves(movingPiece, false)

    locations

  def possibleMoves(movingPiece: Piece): Vector[Move] =
    val locations = possibleLocations(movingPiece)


    val t0moveGenerator = System.nanoTime()

    val moves = locations.map( pos => Move(movingPiece, pos))

    val t1moveGenerator = System.nanoTime()

    moves.filter(_.isLegal)
  end possibleMoves

  def threatMap: Vector[Square] =
    var allPieces = Vector[Piece]()
    var result = Vector[GridPos]()

    if whiteToMove() then allPieces = Board.whitePieces
    else allPieces = Board.blackPieces

    for (elem <- allPieces) do
      if elem.isInstanceOf[Bishop] || elem.isInstanceOf[Queen] || elem.isInstanceOf[Rook] then
        slidingMoves(elem, true).foreach(move => result = result :+ move)
      if elem.isInstanceOf[King] then kingMoves(elem, true).foreach(move => result = result :+ move)
      if elem.isInstanceOf[Knight] then knightMoves(elem, true).foreach(move => result = result :+ move)
      if elem.isInstanceOf[Pawn] then pawnMoves(elem, true).foreach(move => result = result :+ move)
    result.map(Board.elementAt(_)).distinct


  private def slidingMoves(movingPiece: Piece, threatMap: Boolean): Vector[GridPos] =
    var moves = Vector[GridPos]()
    val startLocation = movingPiece.position
    var neighbors = Board.neighbors(startLocation, true).map(_.position)

    movingPiece match {
      case _: Bishop =>
        val notDiagonals = Board.neighbors(startLocation, false).map(_.position)
        neighbors = neighbors.filter(x => !notDiagonals.contains(x))
      case _: Rook => neighbors = Board.neighbors(startLocation, false).map(_.position)
      case _ =>
    }

    def addMove(pos: GridPos, xDiff: Int, yDiff: Int): Unit =
      val newPos = GridPos(pos.x + xDiff, pos.y + yDiff)
      if Board.contains(newPos) then
        Board(newPos).piece match
          case Some(piece) =>
            if piece.color != movingPiece.color then
              moves = moves :+ newPos
            else if threatMap then
              moves = moves :+ newPos
          case None =>
            moves = moves :+ newPos
            addMove(newPos, xDiff, yDiff)
    end addMove

    for i <- neighbors.indices do
      val xDiff = startLocation.xDiff(neighbors(i))
      val yDiff = startLocation.yDiff(neighbors(i))
      addMove(startLocation, xDiff, yDiff)

    moves
  end slidingMoves

  def pawnMoves(movingPiece: Piece, threatMap: Boolean): Vector[GridPos] =

    val isWhite = movingPiece.isWhite
    val allLocations = Board.getPieces(Board.allElements).map(_.position)
    val whiteLocations = Board.whitePieces.map(_.position)
    val blackLocations = Board.blackPieces.map(_.position)

    val currentSquare = movingPiece.square
    val pos = movingPiece.position

    var moves = Vector[GridPos]()

    // Helper function
    def getPawn(pawnLoc: GridPos): Option[Pawn] =
      val piece = Board.elementAt(pawnLoc).piece
      if piece.isDefined then
        piece match
          case Some(piece) =>
            piece match
              case pawn: Pawn => Some(pawn)
              case _ => None
          case None => None
      else
        None

    if isWhite && currentSquare.squaresToEdge.head != 0 then
      // Moving
      val oneUp = GridPos(pos.x, pos.y - 1)
      if !allLocations.contains(oneUp) && !threatMap then
        moves = moves :+ oneUp
        if pos.y == 6 then
          val oneMore = GridPos(oneUp.x, oneUp.y - 1)
          if !allLocations.contains(oneMore) then moves = moves :+ oneMore

      // Eating
      val (eatLeft, eatRight) = (GridPos(pos.x - 1, pos.y - 1), GridPos(pos.x + 1, pos.y - 1))
      if blackLocations.contains(eatLeft) || (threatMap && Board.contains(eatLeft)) then moves = moves :+ eatLeft
      if blackLocations.contains(eatRight) || (threatMap && Board.contains(eatRight)) then moves = moves :+ eatRight

      // En passant
      if pos.y == 3 then
        val leftLoc = GridPos(pos.x - 1, pos.y)
        if Board.contains(leftLoc) then
          val leftPiece = getPawn(leftLoc)
          if leftPiece.isDefined then
            leftPiece match
              case Some(pawn) =>
                if pawn.doubleMoveDone then moves = moves :+ GridPos(pos.x - 1, pos.y - 1)
              case None =>

        val rightLoc = GridPos(pos.x + 1, pos.y)
        if Board.contains(rightLoc) then
          val rightPiece = getPawn(rightLoc)
          if rightPiece.isDefined then
            rightPiece match
              case Some(pawn) =>
                if pawn.doubleMoveDone then moves = moves :+ GridPos(pos.x + 1, pos.y - 1)
              case None =>

    if (!isWhite && currentSquare.squaresToEdge(4) != 0) then
      // Moving
      val oneUp = GridPos(pos.x, pos.y + 1)
      if !allLocations.contains(oneUp) && !threatMap then
        moves = moves :+ oneUp
        if pos.y == 1 then
          val oneMore = GridPos(oneUp.x, oneUp.y + 1)
          if !allLocations.contains(oneMore) then moves = moves :+ oneMore

      // Eating
      val (eatLeft, eatRight) = (GridPos(pos.x - 1, pos.y + 1), GridPos(pos.x + 1, pos.y + 1))
      if whiteLocations.contains(eatLeft) || (threatMap && Board.contains(eatLeft)) then moves = moves :+ eatLeft
      if whiteLocations.contains(eatRight) || (threatMap && Board.contains(eatRight)) then moves = moves :+ eatRight

      // En passant
      if pos.y == 4 then
        val leftLoc = GridPos(pos.x - 1, pos.y)
        if Board.contains(leftLoc) then
          val leftPiece = getPawn(leftLoc)
          if leftPiece.isDefined then
            leftPiece match
              case Some(pawn) =>
                if pawn.doubleMoveDone then moves = moves :+ GridPos(pos.x - 1, pos.y + 1)
              case None =>

        val rightLoc = GridPos(pos.x + 1, pos.y)
        if Board.contains(rightLoc) then
          val rightPiece = getPawn(rightLoc)
          if rightPiece.isDefined then
            rightPiece match
              case Some(pawn) =>
                if pawn.doubleMoveDone then moves = moves :+ GridPos(pos.x + 1, pos.y + 1)
              case None =>

    moves
  end pawnMoves

  def knightMoves(movingPiece: Piece, threatMap: Boolean) =
    val pos = movingPiece.position
    val allCoords = Vector[(Int, Int)](
      (pos.x - 1, pos.y + 2),
      (pos.x + 1, pos.y + 2),
      (pos.x - 1, pos.y - 2),
      (pos.x + 1, pos.y - 2),
      (pos.x + 2, pos.y - 1),
      (pos.x + 2, pos.y + 1),
      (pos.x - 2, pos.y - 1),
      (pos.x - 2, pos.y + 1)
    )
    var allPos = allCoords.map( x => GridPos(x._1, x._2))

    allPos.filter(loc =>
      if Board.contains(loc) then
        Board(loc).piece match
          case Some(piece) =>
           if piece.color == movingPiece.color && !threatMap then false
            else true
          case None => true
      else
        false
    )
  end knightMoves

  def kingMoves(movingPiece: Piece, threatMap: Boolean) =
    val allMoves = Board.neighbors(movingPiece.position, true)
    allMoves.filter(square =>
      square.piece match
        case Some(piece) =>
          if piece.color == movingPiece.color && !threatMap then false
          else true
        case None => true
    ).map(_.position)

  def castling(movingPiece: Piece, threatMap: Boolean) =
    val king = movingPiece.asInstanceOf[King]
    val pos = king.position

    var result = Vector[GridPos]()

    if !king.hasMoved && !king.isInCheck then
      val positionsLeft: Vector[Square] = Vector((pos.x - 1, pos.y), (pos.x - 2, pos.y), (pos.x - 3, pos.y)).map( p => Board.elementAt(GridPos(p._1, p._2)))
      val positionsRight: Vector[Square] = Vector((pos.x + 1, pos.y), (pos.x + 2, pos.y)).map(p => Board.elementAt(GridPos(p._1, p._2)))

      val whiteRooks = Board.whitePieces.filter(p => p.isInstanceOf[Rook]).filter(!_.asInstanceOf[Rook].hasMoved)
      val blackRooks = Board.blackPieces.filter(_.isInstanceOf[Rook]).filter(!_.asInstanceOf[Rook].hasMoved)

      // Left
      val piecesLeft = Board.getPieces(positionsLeft)
      if piecesLeft.isEmpty && movingPiece.isWhite && whiteRooks.nonEmpty then
        if whiteRooks.map(_.position).contains(GridPos(0, 7)) then result = result :+ GridPos(pos.x - 2, pos.y)
      else if piecesLeft.isEmpty && blackRooks.nonEmpty then
        if blackRooks.map(_.position).contains(GridPos(0, 0)) then result = result :+ GridPos(pos.x - 2, pos.y)

      // Right
      val piecesRight = Board.getPieces(positionsRight)
      if piecesRight.isEmpty && movingPiece.isWhite && whiteRooks.nonEmpty then
        if whiteRooks.map(_.position).contains(GridPos(7, 7)) then result = result :+ GridPos(pos.x + 2, pos.y)
      else if piecesRight.isEmpty && blackRooks.nonEmpty then
        if blackRooks.map(_.position).contains(GridPos(7, 0)) then result = result :+ GridPos(pos.x + 2, pos.y)
    result

  end castling

end GameState