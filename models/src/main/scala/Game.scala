package com.github.alphaneet.suparobo

case class Game(
  self:  Option[Player] = None,
  other: Option[Player] = None,
  board: Option[Board]  = None
) extends NotNull
