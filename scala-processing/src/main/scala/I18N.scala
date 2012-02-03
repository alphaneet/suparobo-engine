package com.github.alphaneet.scala_processing

class I18N(val locale: String, xml: scala.xml.Node) {
  def this(locale: String, filename: String) = this(
    locale,
    scala.xml.XML.loadFile(filename)
  )

  def translateOrException(name: String): String = {       
    (name.split('.')).foldLeft(xml) {
      case (node, word) =>
      (node \ word).head
    }.text
  }
  
  def translate(name: String): String = {
    try {
      translateOrException(name)
    } catch {
      case _: NoSuchElementException => name.replaceAll("\\.", " ")
    }      
  }

  val t = translate _
}
