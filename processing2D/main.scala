package com.github.alphaneet.suparobo

object Main extends SPApplet {
  val screenSize = new Dimension(800, 600)
  override def setup() {
    size(processing.core.PConstants.P2D)
    frameRate(24)
    title = "スーパー東方大戦"
    DeckMakeScene(this)
//    TitleScene(this)
  }
}

// TK: scala-processing にいつか移行
abstract class Dialog(val applet: SPApplet) {
  dialog =>

  import processing.core.PConstants
  
  private abstract sealed class Mode extends ButtonManager(applet) {
    private var _body: Option[PImage] = None
    def body = _body
    def body_=(text: String) {
      _body = Option(dialog.createBody(text))
    }
        
    override def draw() {
      dialog.background()
      body foreach { applet.image(_, centerX(width), dialog.bodyY) }
      super.draw()
    }

    def createButton(text: String, x: Int, y: Int): Button = {
      val images = dialog.createButtonImages(text)
      register(images, x, y).action {
        dialog.isOpen = false
      }
    }
  }

  private object Message extends Mode {
    val ok = createButton(okText, centerX(buttonWidth), buttonY)
  }

  private object Confirm extends Mode {   
    val ok = createButton(okText, centerX(buttonWidth) - buttonSpan, buttonY)
    val cancel = createButton(cancelText, centerX(buttonWidth) + buttonSpan, buttonY)
  }

  val width: Int
  val height: Int

  val buttonWidth: Int
  val buttonHeight: Int

  val bodyY: Int
  val buttonY: Int

  lazy val buttonSpan = ((buttonWidth.toFloat / 4) * 3).toInt

  val okText: String
  val cancelText: String

  def background(): Unit  
  def createBody(text: String): PImage
  def createButtonImages(text: String): List[PImage]

  val centerX = applet.width  >> 1
  val centerY = applet.height >> 1
  
  def centerX(width: Int):  Int = centerX - (width  >> 1)
  def centerY(height: Int): Int = centerY - (height >> 1)
  
  private var _isOpen = false
  def isOpen = _isOpen
  private def isOpen_=(isOpen: Boolean) { _isOpen = isOpen }

  private var mode: Option[Mode] = None
  
  def confirm(body: String)(action: => Unit) {
    isOpen = true
    Confirm.ok.action {
      isOpen = false
      action
    }
    Confirm.body = body
    mode = Option(Confirm)
  }

  def message(body: String) {
    isOpen = true
    Message.body = body
    mode = Option(Message)
  }

  def checkMouse(): Unit = mode foreach { _.checkMouse() }
  def draw(): Unit = mode foreach { _.draw() }
}
