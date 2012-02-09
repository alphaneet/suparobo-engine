package com.github.alphaneet.suparobo

case class GameScene(
  game: Game
)(implicit
  applet: SPApplet,
  i18n: I18N
) extends Scene(applet) with MyUtil {  
//  implicit val layout = new LayoutXML(LAYOUTS_PATH + "BoardSelectScene.xml")
  implicit val gg     = new GraphicsGenerator(applet)
  
  val board = game.board
  object boardViewer {
    val width  = applet.width
    val height = width * (board.height / board.width)
    
    private var _scale = 1.0f
    def scale = _scale
    def scale_=(scale: Float) {
      _scale = rangeOfNumber(scale, 0.375f, 1.0f)
      val halfW = width >> 1      
      val prevH = sprite.height
        
      sprite.width  = (width  * _scale).toInt
      sprite.height = (height * _scale).toInt

      sprite.x =  (halfW - _scale * halfW).toInt
      y += (prevH - sprite.height) / 2
    }

    def y = sprite.y
    def y_=(y: Int) {
      sprite.y = rangeOfNumber(y, -sprite.height + applet.height, 0)
    }
    
    val sprite: Sprite = createBoardSprite(board, new Rectangle(0, 0, width, height))

    def draw() {
      sprite.draw()
      
      applet.stroke(C5R, C5G, C5B)
      applet.noFill()

      val w = sprite.width.toFloat  / board.width
      val h = sprite.height.toFloat / board.height
      val (sx, ex) = (sprite.x, sprite.x + sprite.width)
      val (sy, ey) = (sprite.y, sprite.y + sprite.height)
      (0 to board.width) foreach {
        i =>
        val x = sx + w * i
        applet.line(x, sy, x, ey)
      }
      (0 to board.height) foreach {
        i =>
        val y = sy + h * i
        applet.line(sx, y, ex, y)
      }
    }
  }

  var action: Action = readyAction

  trait Action extends NotNull {
    action = this
    def draw(): Unit
  }

  def readyAction = new Action {
    def draw() {
    }
  }

  override def draw() {
    applet.background(C2)
    boardViewer.draw()
  }

  override def mouseWheelMoved() {
    boardViewer.scale += applet.mouseWheelRotation.toFloat / 100
  }

  override def mouseDragged() {
    boardViewer.y += (applet.mouseY - applet.pmouseY)
  }
}
