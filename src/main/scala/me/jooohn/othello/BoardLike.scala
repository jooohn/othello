package me.jooohn.othello

import me.jooohn.othello.Stone.{Black, White}

case class Direction(val x: Int, val y: Int) {

  def toUnit: Direction = {
    val size = Seq(x.abs, y.abs).max
    Direction(x / size, y / size)
  }

  def inverse: Direction = Direction(-x, -y)

}

object Direction {

  lazy val units = (for (x <- Seq(-1, 0, 1); y <- Seq(-1, 0, 1)) yield Direction(x, y)).toSet

}

case class Position(x: Int, y: Int) {

  lazy val flanks: Set[Position] = Direction.units.map(flank)

  def flank(direction: Direction) = Position(x + direction.x, y + direction.y)

  def direction(target: Position) = Direction(target.x - x, target.y - y)

  def line(target: Position): Set[Position] = if (target == this) {
    Set(this)
  } else {
    flank(direction(target).toUnit).line(target) + this
  }
}

case class Board(columns: Int, rows: Int, stones: Map[Position, Stone]) extends BoardLike

trait BoardLike {

  val columns: Int
  val rows: Int
  val stones: Map[Position, Stone]

  lazy val isFinished: Boolean = Seq(White, Black).flatMap(availablePositions).isEmpty

  lazy val corners: Set[Position] = Set(
    Position(1, 1),
    Position(columns, 1),
    Position(1, rows),
    Position(columns, rows)
  )

  def countOf(stone: Stone): Int = stones.values.count(_ == stone)

  def show(): Unit = {
    show(position => stones.get(position).map(_.symbol).getOrElse(' '))
  }

  def show(nextStone: Stone): Unit = {
    show(position => {
      stones.get(position).map(_.symbol).getOrElse {
        if (available(nextStone, position)) '^' else ' '
      }
    })
  }

  def show(rule: Position => Char): Unit = {
    for (y <- 1 to rows) {
      for (x <- 1 to columns) {
        print(s"[${rule.apply(Position(x, y))}]")
      }
      println
    }
    println
  }

  def put(stone: Stone, onPosition: Position): Option[BoardLike] = if (available(stone, onPosition)) {
    val enclosure = Direction.units.flatMap(enclosingWith(stone, onPosition, _, enclosing = false))
    val affectedPositions = enclosure.flatMap(onPosition.line)
    val newEntries = affectedPositions.map((_, stone))
    Some(Board(columns, rows, stones ++ newEntries))
  } else {
    None
  }

  def availablePositions(stone: Stone): Set[Position] = {
    stones.flatMap {
      case (position, existing) => if (existing == stone) Seq() else position.flanks
    }.toSet.filter(available(stone, _))
  }

  def available(stone: Stone, onPosition: Position): Boolean = {
    if (!includes(onPosition) || stones.contains(onPosition)) {
      false
    } else {
      Direction.units.exists(enclosingWith(stone, onPosition, _, enclosing = false).isDefined)
    }
  }

  private def includes(position: Position): Boolean =
    1 <= position.x && position.x <= columns && 1 <= position.y && position.y <= rows

  private def enclosingWith(stone: Stone, from: Position, direction: Direction, enclosing: Boolean): Option[Position] = {
    val flank = from.flank(direction)
    if (includes(flank)) {
      stones.get(flank).flatMap { existing =>
        if (existing == stone) {
          if (enclosing) {
            Some(flank)
          } else {
            None
          }
        } else {
          enclosingWith(stone, flank, direction, enclosing = true)
        }
      }
    } else {
      None
    }
  }

}
