object DiagramEditor extends EditorPApplet {
  def createEditorScene = new DiagramEditorScene(this)
}

class DiagramEditorScene(val applet: EditorPApplet) extends EditorScene {  
  editor =>
    
  val buttonManager = new ButtonManagerEx

  // initialize
  {
    import buttonManager.createButtonByBasicColor

    val createKindButton = createButtonByBasicColor(40, 40, 18)

    createKindButton(30, 120, "四") {}
    createKindButton(80, 120, "三") {}
    createKindButton(30, 170, "丸") {}
    createKindButton(80, 170, "棒") {}
  }

  override def draw() {
    applet.background(255)
    buttonManager.checkMouse()
    buttonManager.draw()
  }
}
