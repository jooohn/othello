package me.jooohn.othello

import me.jooohn.othello.Stone._
import me.jooohn.othello.strategy._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global


object Main extends App {

  case class Player(stone: Stone, strategy: Strategy)

  val size = 8
  val initialMap = Map(
    Position(size / 2, size / 2) -> Black,
    Position(size / 2, size / 2 + 1) -> White,
    Position(size / 2 + 1, size / 2) -> White,
    Position(size / 2 + 1, size / 2 + 1) -> Black
  )
  val board = Board(size, size, initialMap)
  val players = Seq(
    Player(White,
      TakeCorner
        or DefendCorner(ReduceOpponentChoices when EarlierThan(0.8) or MaximumGain)
        orAtLeast RandomOne),
    Player(Black,
      (BestChoice when LaterThan(0.9))
        or TakeCorner
        or DefendCorner(MinimumGain)
        orAtLeast MaximumGain)
  )
  Await.result(play(board, players, 30), Duration.Inf)

  def play(board: BoardLike, order: Seq[Player], interval: Int = 300): Future[Any] = {
    def sleep: Unit = if (0 < interval) Thread.sleep(interval)
    def iter(currentBoard: BoardLike, currentOrder: Seq[Player]): Future[BoardLike] = {
      sleep
      currentBoard.show()
      if (currentBoard.isFinished) {
        Future.successful(currentBoard)
      } else {
        val player = currentOrder.head
        val nextOrder = currentOrder.tail :+ player
        player.strategy.decide(player.stone, currentBoard).flatMap { decision =>
          val nextBoard = decision.flatMap(currentBoard.put(player.stone, _)).getOrElse(currentBoard)
          iter(nextBoard, nextOrder)
        }
      }
    }

    iter(board, order).map(result => {
      println(s"${White.symbol}: ${result.countOf(White)}, ${Black.symbol}: ${result.countOf(Black)}")
    })
  }
}
