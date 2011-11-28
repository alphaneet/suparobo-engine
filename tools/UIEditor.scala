package com.github.alphaneet.suparobo_engine.tools
import com.github.alphaneet.processing.{
  TextManager
}

object UIEditor extends EditorPApplet {
  def createEditorScene = new UIEditorScene(this)
}

class UIEditorScene(applet: EditorPApplet) extends EditorScene(applet) {
  editor =>

  import processing.core.PConstants

  /*
  import applet._
  val textManager = new TextManager(applet)  
  textManager.registerTextField(200 + random(width - 400).toInt, 20 + random(height - 40).toInt - 20, 200, 20)
  textManager.registerTextField(600, 550, 200, 20)
  textManager.registerTextField(100, 50, 200, 20)
  */

  // initialize
  {
    val createMenuLabel = gg.createLabel(
      _: String,
      width  = 130,
      height = 20,
      size   = 13,
      frontColor = 0x333333,
      backColor = 0xFF0000,
      align  = PConstants.LEFT
    )
    
    List(
      ("キャンバスの幅", 10, 50),
      ("キャンバスの高さ", 10, 90),
      ("グリッドの幅", 10, 130),
      ("グリッドの高さ", 10, 170)
    ) foreach {  case (text, x, y) => new Image(x, y, createMenuLabel(text)) }
  }

  override def draw() {
    applet.background(255, 0, 0)
    applet.fill(60)
    applet.rect(150, 50, 500, 500)

    images.drawAll()
  }
}
