package com.github.alphaneet.suparobo

case class GameMaker(
  inside:  Option[Player] = None,
  outside: Option[Player] = None,
  board:   Option[Board]  = None
) {
  def createGame() = Game(
    inside  getOrElse (throw new NoSuchInsidePlayerException),
    outside getOrElse (throw new NoSuchOutsidePlayerException),
    board   getOrElse (throw new NoSuchBoardException)
  )
}

object Game {
  abstract sealed case class Status(id: Int) extends NotNull  
  object SETUP extends Status(1)
  object PLAY  extends Status(2)
  object END   extends Status(3)
}

case class Game(
  inside:  Player,
  outside: Player,
  board: Board
) extends NotNull {
  import Game._
  
  private var _status: Status = SETUP
  private def status_=(status: Status) { _status = status }
  def status  = _status
  
  def isSetup = status == SETUP
  def isPlay  = status == PLAY
  def isEnd   = status == END

  val players = List(inside, outside)
  
  def charactersSet: Set[Character] = {
    (Set[Character]() /: players)(_ ++ _.deck.characters)
  }
  
  def setup() {
    // TK: チェック入れなあかん。山（移動不可）だったらおけないとか
    {
      var i = board.lastIndex
      inside.deck.characters.foreach {
        c =>
        c.pos = board.pos(i)
        i -= 1
      }
    }

    {
      var i = 0
      outside.deck.characters.foreach {
        c =>
        c.pos = board.pos(i)
        i += i
      }
    }
  }

  def isComplateSetup: Boolean = {
    if(!isSetup) return false

    
    true
  }
  
  def gotoSetup() {
  }
  
  def gotoPlay() {
    if (isComplateSetup) {
      status = PLAY
    } else {
      throw new IllegalGameStatusException(status + " -> " + PLAY)
    }
  }
  
  def gotoEnd() {}       
}
