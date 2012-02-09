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
  sealed case class Status(id: Int) extends NotNull  
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
  def status = _status
  
  def isSetup = status == SETUP
  def isPlay  = status == PLAY
  def isEnd   = status == END

  def gotoSetup() {}
  def gotoPlay() {    
    if (isComplateSetup) {
      status = PLAY
    } else {
      throw new IllegalGotoGameStatusException(status + " -> " + PLAY)
    }
  }
  def gotoEnd() {}
      
  def isComplateSetup: Boolean = {
    if(!isSetup) return false

    
    true
  }
}
 
