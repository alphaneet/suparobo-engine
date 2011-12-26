package com.github.alphaneet.suparobo_engine.game

package object scenes {
  val DataPath    = "data/"
  val ImagesPath  = DataPath + "images/"
  val LayoutsPath = DataPath + "layouts/"

  type PImage = processing.core.PImage

  type ButtonManager = com.github.alphaneet.processing.ButtonManager
  type GraphicsGenerator = com.github.alphaneet.processing.GraphicsGenerator  
  type LayoutXML = com.github.alphaneet.processing.LayoutXML
  type PApplet = com.github.alphaneet.processing.PApplet
  type Scene = com.github.alphaneet.processing.Scene
  
}
