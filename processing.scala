class PApplet extends processing.core.PApplet {
  var scene = new Scene {}
  
  private var _isKeyPressed = false
  private var _isMousePressed = false

  lazy val isApplet = (frame == null)

  def title = if (!isApplet) "" else frame.getTitle
  def title_=(title: String) = if (!isApplet) frame.setTitle(title)

  override def draw() = scene.draw()

  def isKeyPressed = _isKeyPressed
  def isMousePressed = _isMousePressed

  override def keyPressed() {
    _isKeyPressed = true
    scene.keyPressed()
  }
  override def keyReleased() {
    _isKeyPressed = false
    scene.keyReleased()
  }
  override def keyTyped() = scene.keyTyped()

  override def mousePressed() {
    _isMousePressed = true
    scene.mousePressed()
  }
  override def mouseReleased() {
    _isMousePressed = false
    scene.mouseReleased()
  }
  override def mouseDragged() {
    scene.mouseDragged()
  }

  def main(args: Array[String]) = runSketch()
}

trait Scene extends NotNull {
  def draw() {}
  def keyPressed() {}
  def keyReleased() {}
  def keyTyped() {}
  def mousePressed() {}
  def mouseReleased() {}
  def mouseDragged() {}
}

trait MyUtil {
  this: { val applet: PApplet } =>

  import processing.core.PGraphics     
  import applet.{ mouseX, mouseY }
  
  def isMouseInside(x: Int, y: Int, w: Int, h: Int): Boolean = {
    mouseX > x && mouseX < x + w && mouseY > y && mouseY < y + h
  }

  def drawPGraphics(g: PGraphics)(draw: PGraphics => Unit) {
    g.beginDraw()
    draw(g)
    g.endDraw()
  }
  
  // TK: こういうの標準でありませんか？あったら教えてぴょ
  def rangeOfNumber[T: Ordering](v: T, min: T, max: T): T = {
    val ord = implicitly[Ordering[T]]

    if (ord.lt(v, min)) min
    else if (ord.gt(v, max)) max
    else v
  }
}

object ButtonStatus extends Enumeration {
  type ButtonStatus = Value
  val UP, OVER, DOWN, DISABLED = Value
}

object ButtonManager {
  private var isLock = false
}

class ButtonManager(applet: PApplet) extends NotNull {
  buttonManager =>
    
  import processing.core.{ PImage, PVector, PConstants }
  
  class Button(
    _images: Tuple4[PImage, PImage, PImage, PImage],
    val pos: PVector,
    val action: () => Unit
  ) extends NotNull {
    
    val images = List(_images._1, _images._2, _images._3, _images._4)
    var status = ButtonStatus.UP

    def checkMouse: Boolean = {
      if (status == ButtonStatus.DISABLED) return false

      val isOver = buttonManager.isOverMouse(images(status.id), this)
      val result = isOver && buttonManager.mouseClicked(this)

      if (mousePressed) {
        if (isOver && !ButtonManager.isLock) {
          status = ButtonStatus.DOWN
          ButtonManager.isLock = true
        }
      } else {
        status = if (isOver) ButtonStatus.OVER else ButtonStatus.UP
        ButtonManager.isLock = false
      }
      result
    }

    def image() = applet.image(images(status.id), pos.x, pos.y)
  }
  private val buttons = scala.collection.mutable.ArrayBuffer[Button]()

  def isOverMouse(image: PImage, button: Button): Boolean =
    applet.mouseX > button.pos.x &&
    applet.mouseX < button.pos.x + image.width &&
    applet.mouseY > button.pos.y &&
    applet.mouseY < button.pos.y + image.height

  /**
   * override 例
   * applet.mousePressed - マウスのどのボタンでも true
   * applet.mousePressed && applet.mouseButton == PConstants.LEFT
   *   - 左クリックの時 true
   */
  def mousePressed: Boolean =
    applet.isMousePressed && applet.mouseButton == PConstants.LEFT
  
  /**
   * override 例
   * mousePressed - ボタンを押してる間ずっと true
   * mousePressed && button.status == Button.OVER
   *   - 押した時に true
   * !mousePressed && button.status == Button.DOWN
   *   - 押して離した時に true
   */
  def mouseClicked(button: Button): Boolean =
    !mousePressed && button.status == ButtonStatus.DOWN

  def register(image: PImage, pos: PVector)(action: => Unit): Button =
    register((image, image, image), pos)(action)

  def register(images: Triple[PImage, PImage, PImage],
               pos: PVector)(action: => Unit): Button =
    register((images._1, images._2, images._3,
              applet.createImage(0, 0, PConstants.ARGB)), pos)(action)
  
  def register(images: Tuple4[PImage, PImage, PImage, PImage],
               pos: PVector)(action: => Unit): Button = {
    val button = new Button(images, pos, action _)
    buttons += button
    button
  }

  def unregister(button: Button): Button = {
    buttons -= button
    button
  }

  def checkMouse() =
    buttons withFilter(_.checkMouse) foreach(_.action())

  def draw() = buttons.foreach(_.image())
}

class GraphicsGenerator(applet: processing.core.PApplet) extends NotNull {
  import processing.core.{ PGraphics, PImage }
  import processing.core.PConstants.{ JAVA2D, ARGB, HSB, CENTER, LEFT, RIGHT }
  
  def rgb(c: Int): (Float, Float, Float) = {
    val g = applet.g
    (g.red(c), g.green(c), g.blue(c))
  }

  def hsb(c: Int): (Float, Float, Float) = {
    val g = applet.g
    (g.hue(c), g.saturation(c), g.brightness(c))
  }

  def createLabel(text: String, width: Int, height: Int, size: Int,
                  frontColor: Int, backColor: Int = -1, align: Int = CENTER): PImage = {
    createAndDrawPImage(width + 1, height + 1) {
      g =>

      g.smooth

      if (backColor >= 0) {
        val (red, green, blue) = rgb(backColor)
        g.background(red, green, blue)
      }

      val (red, green, blue) = rgb(frontColor)
      g.fill(red, green, blue)
      g.textFont(applet.createFont("", size))
      g.textAlign(align)
      val des = g.textDescent.toInt
      val x: Int = align match {
        case LEFT   => 2
        case CENTER => (width >> 1)
      }
      g.text(text, x, (height >> 1) + des + (des >> 1) )
    }
  }
  
  def createCircle(size: Int, hue: Int): PImage = createCircle(size, size, hue)
  def createCircle(width: Int, height: Int, hue: Int): PImage = {        
    val halfW = width >> 1
    val halfH = height >> 1
    val weight = 2
        
    createAndDrawPImage(width + 1, height + 1) {
      g =>
        
      def ellipse(w: Int, h: Int) = g.ellipse(halfW, halfH, w - weight, h - weight)
      
      g.colorMode(HSB, 255)
      g.smooth()
      
      g.strokeWeight(weight)
      g.stroke(hue, 255, 100)
      g.fill(hue, 255, 255)
      ellipse(width, height)
      g.noStroke()

      val loop = (if (halfW < halfH) halfW else halfH) - weight
      val div = 255.0f / loop
      (1 to loop) foreach {
        x =>
        g.fill(hue, 255 - (x * div), 255)
        ellipse(width - (x << 1), height - (x << 1))
      }
    }
  }  

  def createAndDrawPImage(width: Int, height: Int)(draw: PGraphics => Unit): PImage = {
    val g: PGraphics = applet.createGraphics(width, height, JAVA2D)
    
    g.beginDraw()
    draw(g)
    g.endDraw()

    val image = applet.createImage(width, height, ARGB)
    image.set(0, 0, g)
    g.dispose

    image
  }
}
