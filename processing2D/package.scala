package com.github.alphaneet

package object suparobo_engine {

  // TK: 大文字にするお
  val DataPath    = "data/"
  val ImagesPath  = DataPath + "images/"
  val LayoutsPath = DataPath + "layouts/"
  val CHARACTERS_PATH = DataPath + "characters/"
  
  val MAX_DECK = 3  
  val MAX_COST = 10
  def createDeck() = new Deck(MAX_COST)

  type Rectangle = java.awt.Rectangle
  
  type PImage = processing.core.PImage  

//  import com.github.alphaneet.scala_processing
  type PApplet = com.github.alphaneet.scala_processing.PApplet
  type Scene = com.github.alphaneet.scala_processing.Scene  
  type ButtonManager = com.github.alphaneet.scala_processing.ButtonManager
  type GraphicsGenerator = com.github.alphaneet.scala_processing.GraphicsGenerator  
  type LayoutXML = com.github.alphaneet.scala_processing.LayoutXML
  
  def loadCharacterParameters(filename: String): List[CharacterParameter] = {
    (scala.xml.XML.loadFile(filename) \ "parameter").map {
      xml =>
      def toInt(name: String): Int = try {
        (xml \ name).text.toInt
      } catch { case _ => 0 }
        
      CharacterParameter(
        name = (xml \ "name").text,
        hitPoint = toInt("hitPoint"),
        moveRangePoint = toInt("moveRangePoint"),
        attackPoint = toInt("attackPoint"),
        attackRangePoint = toInt("attackRangePoint"),
        guardPoint = toInt("guardPoint"),
        cost = toInt("hitPoint")
      )
    }.toList
  }

  def loadCharacterProfiles(filename: String): Map[String, String] = {
    Map("" -> "")
  }  
}
