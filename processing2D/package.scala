package com.github.alphaneet

package object suparobo {

  // TK: 大文字にするお
  val DataPath    = "data/"
  val ImagesPath  = DataPath + "images/"
  val LayoutsPath = DataPath + "layouts/"
  val CHARACTERS_PATH = DataPath + "characters/"
  
  val MAX_DECK = 3  
  val MAX_COST = 10
  def createDeck() = new Deck(MAX_COST)

  type Rectangle = java.awt.Rectangle
  
  type PImage = _root_.processing.core.PImage  

  import com.github.alphaneet._
  type SPApplet = scala_processing.SPApplet
  type Scene = scala_processing.Scene  
  type ButtonManager = scala_processing.ButtonManager
  type GraphicsGenerator = scala_processing.GraphicsGenerator  
  type LayoutXML = scala_processing.LayoutXML
  
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
