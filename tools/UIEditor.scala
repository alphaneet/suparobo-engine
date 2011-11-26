object UIEditor extends EditorPApplet {
  def createEditorScene = new UIEditorScene(this)
}

class UIEditorScene(val applet: EditorPApplet) extends EditorScene {
  editor =>

  import processing.core.PConstants

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
    
//    def registerMenuLabel(text: String, x: Int, y: Int) = new Image(x, y, createMenuLabel(text))

    List(
      ("キャンバスの幅", 10, 50),
      ("キャンバスの高さ", 10, 90),
      ("グリッドの幅", 10, 130),
      ("グリッドの高さ", 10, 170)
    ) foreach {  case (text, x, y) => new Image(x, y, createMenuLabel(text)) }
  }

    

  
  override def draw() {
    applet.background(255)
    applet.fill(60)
    applet.rect(150, 50, 500, 500)

    images.drawAll()
  }
}
