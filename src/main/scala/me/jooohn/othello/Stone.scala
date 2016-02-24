package me.jooohn.othello

sealed abstract class Stone(val symbol: Char) {
  val opposite: Stone
}

object Stone {

  case object White extends Stone('o') {
    lazy val opposite = Black
  }
  case object Black extends Stone('x') {
    lazy val opposite = White
  }

}
