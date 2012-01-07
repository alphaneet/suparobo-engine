package com.github.alphaneet.suparobo_engine.game

package object scenes {
  import com.github.alphaneet.suparobo_engine.game.models.{
    Deck,
    CharacterParameter
  }

  // TK: 大文字にするお
  val DataPath    = "data/"
  val ImagesPath  = DataPath + "images/"
  val LayoutsPath = DataPath + "layouts/"
  val ParamsPath  = DataPath + "params/"

  val MAX_DECK = 3  
  val MAX_COST = 10
  def createDeck() = new Deck(MAX_COST)

  type Rectangle = java.awt.Rectangle
  
  type PImage = processing.core.PImage  

  type ButtonManager = com.github.alphaneet.processing.ButtonManager
  type GraphicsGenerator = com.github.alphaneet.processing.GraphicsGenerator  
  type LayoutXML = com.github.alphaneet.processing.LayoutXML
  type PApplet = com.github.alphaneet.processing.PApplet
  type Scene = com.github.alphaneet.processing.Scene  
  
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

  def loadCharacterFilenames(filename: String): Map[String, String] = {
  }  
}
