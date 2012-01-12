package com.github.alphaneet.suparobo
import com.github.alphaneet.scala_processing.{
  SPApplet,
  Scene,
  MyUtil,
  GraphicsGenerator
}

class ConfigXML(val elem: scala.xml.NodeSeq) {
  def this(filename: String) = this(
    try {
      scala.xml.XML.loadFile(filename)      
    } catch {
      case ex =>
      Console.err.println(ex)
      <dummy></dummy>
    }
  )
  
  def apply[T](symbol: Symbol, default: T): T =
    apply(symbol.name, default)
    
  def apply[T](name: String, default: T): T = {
    if (name == "None") return default
    
    try {
      val text = (elem \ name).text      
      (default match {
        case _: Int    => java.lang.Integer.decode(text)
        case _: Float  => text.toFloat        
        case _: String => text
        case _: Symbol => Symbol(text)
        case _: Char   => text.head
      }).asInstanceOf[T]
    } catch {
      case ex => {
        Console.err.println(name + " use default: " + default + " --- " + ex)
        default
      }
    }
  }
}

trait EditorPApplet extends SPApplet {
  val clazzName = {
    val tmp = getClass.getSimpleName
    if (tmp.endsWith("$")) tmp.init else tmp
  }

  // TK: ここ古いのでいつか直す
  // Layout と Config の XML は分かれている。
  // screenSize は Layout の XML を見るようにする。
  val config = new ConfigXML("data/" + clazzName + ".xml")  
  val screenSize = new Dimension(config('width, 800), config('height, 600))
  
  def createEditorScene: Scene
  
  override def setup() {
    size(processing.core.PConstants.P2D)
    frameRate(24)
    title = clazzName    
    createEditorScene
  }
}

class EditorScene(val applet: EditorPApplet) extends Scene(applet) with MyUtil {
  editor =>
    
  import processing.core.{ PImage, PVector }

  type LayoutXML = com.github.alphaneet.scala_processing.LayoutXML
    
  implicit val gg = new GraphicsGenerator(applet)
  val images = new scala.collection.mutable.ArrayBuffer[Image] {
    def draw(): Unit = foreach { _.draw() }
    def update(v: Image, image: PImage) {
      find(_ == v) foreach { _.image = Option(image) }
    }
  }
  
  class ButtonManagerEx extends ButtonManager(applet) {
    def createEasyButton
    (releasedFront: Int, pressedFront: Int, back: Int)
    (width: Int = 0, height: Int = 0, size: Int = 0)
    (x: Int, y: Int, text: Any)
    (action: => Unit): Button =
    {
      val txt = text.toString
      val labels = List(
        gg.createLabel(txt, width, height, size, releasedFront, back),
        gg.createLabel(txt, width, height, size, releasedFront, back),
        gg.createLabel(txt, width, height, size, pressedFront,  back)
      )
      register(labels, x, y).action { action }
    }

    val createButtonByBasicColor = createEasyButton(0xFFFFFFF, 0xAAAAAA, 0x333333)_
  }

  class Image(x: Int, y: Int, _image: PImage = null) {    
    var image = Option(_image)
    
    editor.images += this
    def draw(): Unit = image foreach { applet.image(_, x, y) }
  }
}
