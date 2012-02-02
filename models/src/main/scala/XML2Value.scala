package com.github.alphaneet.suparobo

// TK: model に util 的なのあるの良くない気がするので lib っぽい場所に移す。
case class XML2Value(node: scala.xml.Node) {
  def text(name: String) = (node \ name).text
  
  def str(name: String):   String = text(name)
  def str(symbol: Symbol): String = text(symbol.name)
  
  def sym(name: String):   Symbol = Symbol(text(name))
  def sym(symbol: Symbol): Symbol = Symbol(text(symbol.name))

  def int(symbol: Symbol): Int = int(symbol.name)
  def int(name: String):   Int = try {
    text(name).toInt
  } catch {
    case _ => 0
  }
}
