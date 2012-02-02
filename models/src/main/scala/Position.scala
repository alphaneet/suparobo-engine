package com.github.alphaneet.suparobo

case class Position (
  private var _x: Int,
  private var _y: Int
)(implicit
  board: Board
) extends NotNull {
  board.checkRectangle(_x, _y)
  
  def x = _x
  def y = _y

  def x_=(x: Int) { board.checkWidth(x);  _x = x }
  def y_=(y: Int) { board.checkHeight(y); _y = y }

  def +=(pos: Position) { x += pos.x; y += pos.y }    
  def -=(pos: Position) { x -= pos.x; y -= pos.y }

  def index = x + y * board.width
}
