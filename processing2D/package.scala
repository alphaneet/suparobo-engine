package com.github.alphaneet

package object suparobo {

  val DATA_PATH    = "data/"
  val IMAGES_PATH  = DATA_PATH + "images/"
  val IMAGES_EXT   = ".png"  
  val LAYOUTS_PATH = DATA_PATH + "layouts/"
  val CHARACTERS_PATH = DATA_PATH + "characters/"
  val DECKS_PATH = DATA_PATH + "decks/"  
  
  val MAX_DECK = 3
  val MAX_COST = 13
  def createDeck() = new Deck(MAX_COST)

  // thanks for http://kuler.adobe.com/#themeID/1692819
  
  val C1  = 0xFFE8E8
  val C1R = 255
  val C1G = 232
  val C1B = 232  
  
  val C2  = 0xE8E3D1
  val C2R = 232
  val C2G = 227
  val C2B = 209  
  
  val C3  = 0xC9C2B1
  val C3R = 201
  val C3G = 194
  val C3B = 177
  
  val C4  = 0xA19C8D
  val C4R = 161
  val C4G = 156
  val C4B = 141
  
  val C5  = 0x706666
  val C5R = 112
  val C5G = 102
  val C5B = 102
  
  type Rectangle = java.awt.Rectangle

  type PImage = processing.core.PImage

  import com.github.alphaneet._
  type SPApplet = scala_processing.SPApplet
  type Scene = scala_processing.Scene  
  type ButtonManager = scala_processing.ButtonManager
  type GraphicsGenerator = scala_processing.GraphicsGenerator  
  type LayoutXML = scala_processing.LayoutXML
  type MyUtil = scala_processing.MyUtil
}
