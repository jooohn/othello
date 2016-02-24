package me.jooohn.othello.strategy

import me.jooohn.othello.strategy.TacticUtils._
import me.jooohn.othello.{BoardLike, Position, Stone}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait Strategy extends Tactic {

  def decide(stone: Stone, board: BoardLike): Future[Option[Position]]

}

case class CombinedStrategy(tactic: Tactic, fallback: Strategy) extends Strategy {

  override def decide(stone: Stone, board: BoardLike) = tactic.decide(stone, board).flatMap {
    case Some(position) => Future.successful(Some(position))
    case None => fallback.decide(stone, board)
  }

}

object RandomOne extends Strategy {

  import scala.util.Random

  override def decide(stone: Stone, board: BoardLike) = Future {
    val positions = board.availablePositions(stone)
    Random.shuffle(positions.toList).headOption
  }

}

object MaximumGain extends Strategy {

  override def decide(stone: Stone, board: BoardLike) = Future {
    val positions = board.availablePositions(stone)
    if (positions.isEmpty) {
      None
    } else {
      val decision = positions.maxBy(board.put(stone, _).map(_.countOf(stone)).getOrElse(0))
      Some(decision)
    }
  }

}

object MinimumGain extends Strategy {

  override def decide(stone: Stone, board: BoardLike) = Future {
    val positions = board.availablePositions(stone)
    if (positions.isEmpty) {
      None
    } else {
      val decision = positions.minBy(board.put(stone, _).map(_.countOf(stone)).getOrElse(Int.MaxValue))
      Some(decision)
    }
  }

}

object ReduceOpponentChoices extends Strategy {

  override def decide(stone: Stone, board: BoardLike) = {
    val positions = board.availablePositions(stone)
    if (positions.isEmpty) {
      Future.successful(None)
    } else {
      val availablePositionsFutures = positions.map { position =>
        Future {
          (position, board.put(stone, position).map(_.availablePositions(stone.opposite).size).getOrElse(Int.MaxValue))
        }
      }
      Future.sequence(availablePositionsFutures).map { futures =>
        Some(futures.minBy(_._2)._1)
      }
    }
  }

}

object BestChoice extends Strategy {

  override def decide(stone: Stone, board: BoardLike) = {
    def betterConsequence(forStone: Stone, currentBoard: BoardLike): Future[Int] = {
      if (currentBoard.isFinished) {
        Future.successful(currentBoard.countOf(forStone))
      } else {
        val boardsForOpponent = nextPossibilities(forStone, currentBoard)
        val pessimisticFutures = boardsForOpponent.map { boardForOpponent =>
          val bestConsequenceFutures = nextPossibilities(forStone.opposite, boardForOpponent).map { nextBoard =>
            betterConsequence(forStone, nextBoard)
          }
          Future.sequence(bestConsequenceFutures).map(_.min)
        }
        Future.sequence(pessimisticFutures).map(_.max)
      }
    }
    val positions = board.availablePositions(stone)
    if (positions.isEmpty) {
      Future.successful(None)
    } else {
      print(s"${positions.size}: ")
      val betterChoiceFutures = positions.map { position =>
        val boardForOpponent = board.put(stone, position).getOrElse(board)
        val consequence = betterConsequence(stone.opposite, boardForOpponent).map((position, _))
        print(".")
        consequence
      }
      Future.sequence(betterChoiceFutures).map { betterChoices =>
        println
        Some(betterChoices.minBy(_._2)._1)
      }
    }
  }

}
