package me.jooohn.othello.strategy

import me.jooohn.othello.{BoardLike, Position, Stone}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait Tactic {

  def decide(stone: Stone, board: BoardLike): Future[Option[Position]]

  def or(tactic: Tactic): Tactic = Tactics(Seq(this, tactic))

  def when(condition: Condition): Tactic = Conditional(this, condition)

  def orAtLeast(strategy: Strategy): Strategy = CombinedStrategy(this, strategy)

}

object TacticUtils {

  def nextPossibilities(stone: Stone, board: BoardLike): Set[BoardLike] = {
    val boardsForOpponent = board.availablePositions(stone).flatMap(board.put(stone, _))
    if (boardsForOpponent.isEmpty) Set(board) else boardsForOpponent
  }

}

case class Tactics(tactics: Seq[Tactic]) extends Tactic {

  override def decide(stone: Stone, board: BoardLike) = {
    tactics.foldLeft[Future[Option[Position]]](Future.successful(None))((future, tactic) => {
      future.flatMap {
        case Some(position) => Future.successful(Some(position))
        case None => tactic.decide(stone, board)
      }
    })
  }

}

case class Conditional(tactic: Tactic, condition: Condition) extends Tactic {

  override def decide(stone: Stone, board: BoardLike) =
    if (condition.fit(stone, board)) tactic.decide(stone, board) else Future.successful(None)

}

object TakeCorner extends Tactic {

  override def decide(stone: Stone, board: BoardLike) = Future {
    board.corners.find(corner => board.available(stone, corner))
  }

}

case class DefendCorner(bound: Tactic) extends Tactic {

  override def decide(stone: Stone, board: BoardLike) = {
    val positions = board.availablePositions(stone)
    val avoidingPositions = positions.filter { position =>
      board.put(stone, position) match {
        case Some(nextBoard) => nextBoard.corners.exists(nextBoard.available(stone.opposite, _))
        case None => false
      }
    }
    bound.decide(stone, LimitedBoard(board, avoidingPositions))
  }

}

case class LimitedBoard(board: BoardLike, avoidingPositions: Set[Position]) extends BoardLike {

  lazy val columns = board.columns

  lazy val rows = board.rows

  lazy val stones = board.stones

  override def available(stone: Stone, position: Position): Boolean = {
    !avoidingPositions.contains(position) && board.available(stone, position)
  }

}

