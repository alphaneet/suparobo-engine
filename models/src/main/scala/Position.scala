package com.github.alphaneet.suparobo

case class Position(var x: Int = 0, var y: Int = 0) extends NotNull {
   def +=(pos: Position) { x += pos.x; y += pos.y }    
   def -=(pos: Position) { x -= pos.x; y -= pos.y }  
}
