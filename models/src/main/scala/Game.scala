package com.github.alphaneet.suparobo


// TK: GameReady とかの Option 用のクラスを用意すべきかも
// GameDeliver という名前を思いついた
case class Game(
  self:  Option[Player] = None,
  other: Option[Player] = None,
  board: Option[Board]  = None
) extends NotNull
