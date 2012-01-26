package com.github.alphaneet.suparobo

// どっか util 的な場所に移す
class XML2Value(node: scala.xml.Node) {
  def text(name: String) = (node \ name).text
  def str(name: String): String = text(name)
  def sym(name: String): Symbol = Symbol(text(name))
  def int(name: String): Int = try {
    text(name).toInt
  } catch {
    case _ => 0
  }  
}
