package me.jooohn.othello.strategy

import me.jooohn.othello.{Stone, BoardLike}

trait Condition {

  def fit(stone: Stone, board: BoardLike): Boolean

}

case class EarlierThan(border: Double) extends Condition {

  def fit(stone: Stone, board: BoardLike) = {
    val progress = board.stones.size.toDouble / (board.columns * board.rows)
    progress < border
  }

}

case class LaterThan(border: Double) extends Condition {

  def fit(stone: Stone, board: BoardLike) = {
    val progress = board.stones.size.toDouble / (board.columns * board.rows)
    border < progress
  }

}
