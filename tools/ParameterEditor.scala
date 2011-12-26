package com.github.alphaneet.suparobo_engine.tools

object ParameterEditor extends EditorPApplet {
  def createEditorScene = new ParameterEditorScene(this)
}

class ParameterEditorScene(applet: EditorPApplet) extends EditorScene(applet) {
  editor =>
    
  import processing.core.{ PImage, PConstants }
  import com.github.alphaneet.processing.{
    ListManager,
    TextManager
  }

  val SCROLL_WIDTH = 20
  val IMAGE_DIR = "./data/"
  val IMAGE_EXT = ".png"

  case class Parameter(
    name: String,
    hitPoint: Int,
    moveRangePoint: Int,
    attackPoint: Int,
    attackRangePoint: Int,
    cost: Int
  ) extends NotNull

  case class Pack(
    private var _param: Parameter,
    listButton: listManager.Button,
    var imageFilename: String
  ) extends NotNull {
    def param = _param
    def param_=(param: Parameter) {
      _param = param
      listButton.images = createListButtonImages(param.name)
    }
  }

  val packs = new scala.collection.mutable.ArrayBuffer[Pack]() {
    def findByListButton(listButton: listManager.Button): Option[Pack] =
      find(_.listButton == listButton)
  }
  
  val layout = new LayoutXML(applet.config.elem)
  val buttonManager = new ButtonManagerEx
  val paramManager  = new TextManager(applet)
  val listManager   = new ListManager(applet) {
    val rect = layout.rect('list)
    x = rect.x
    y = rect.y
    background(rect.width - SCROLL_WIDTH, rect.height, 0xA36D5C)
  }

  object canvas {
    val rect = layout.rect('canvas)
    var image: Option[PImage] = None
    val centerX = rect.x + (rect.width  >> 1)
    val centerY = rect.y + (rect.height >> 1)
    
    def update(filename: String) {
      paramManager('imageFilename).foreach {
        _.setText(filename)
      }
      image = Option(applet.loadImage(imagePath(filename)))
    }
    
    def draw() {
      applet.noStroke()
      applet.fill(255)
      applet.rect(rect.x, rect.y, rect.width, rect.height)

      image foreach {
        img =>
        applet.imageMode(PConstants.CENTER)
        applet.image(img, centerX, centerY)
        applet.imageMode(PConstants.CORNER)
      }
    }
  }
  
  // initialize
  {
    val createButton = buttonManager.createEasyButton(0x473027, 0xFFFFFF, 0xD9CEAD)_
    val createScrollButton = createButton(SCROLL_WIDTH, SCROLL_WIDTH, 15)

    {
      import listManager._

      def createScrollButton(hex: Int): List[PImage] = {
        val text = hex.toChar.toString
        val size = SCROLL_WIDTH
        val create = gg.createLabel(text, size, size, 15, _: Int, 0xD9CEAD)

        List(0x473027, 0x473027, 0xFFFFFF).map(create)
      }
      
      scrollWidth = SCROLL_WIDTH
      scrollBackground = 0x473027
      scrollUpButton   = createScrollButton(0x25b2)
      scrollDownButton = createScrollButton(0x25bc)
      scrollBarButton  = createScrollButton(0x25a0)
    }
    
    List(
      ('create, create _),
      ('delete, delete _),
      ('save, save _),
      ('load, load _)
    ) foreach {
      case (symbol, action) =>
      layout(symbol) {
        rect =>
        createButton(rect.width, rect.height, 18)(rect.x, rect.y, symbol.name) {
          action()
        }
      }
    }

    List(
      ('hitPoint, "HP", 0, 9999),
      ('moveRangePoint, "移動力", 0, 20),
      ('attackPoint, "攻撃力", 0, 9999),
      ('attackRangePoint, "攻撃範囲", 0, 20),
      ('cost, "コスト", 1, 10)
    ) foreach {
      case (symbol, text, min, max) =>
      val rect = layout.rect(symbol)

      registerMenuLabel(text, rect)
      
      val intField = new paramManager.IntField(symbol, min, max) {
        override def updateValue(): Unit = editor.update()
        
        value = min
        setBounds(rect)
      }
      
      List(
        (0x25C0, -40,  -10),
        (0x25C1, -20,  -1),
        (0x25B7,  50,   1),
        (0x25B6,  70,  10)
      ) foreach {
        case (arrow, addX, addValue) =>
        createButton(20, 20, 15)(rect.x + addX, rect.y + 2, arrow.toChar) {
          intField.value += addValue
          editor.update()
        }
      }
    }

    List(
      ('name, "名前", update _),
      ('imageFilename, "画像ファイル", loadImage _)
    ) foreach {
      case (symbol, text, f) =>
      layout(symbol) {
        rect =>
        registerMenuLabel(text, rect)
        new paramManager.ValidateField(symbol) {
          override def updateValue(): Unit = f()
          setBounds(rect)
        }
      }
    }

    List(
      (IMAGE_DIR, -80),
      (IMAGE_EXT, 120)
    ) foreach {
      case (text, addX) =>
      layout('imageFilename) {
        rect =>
        new Image(
          rect.x + addX, rect.y,
          gg.createLabel(text, 100, rect.height, size = 18, frontColor = 0xFFFFFF)
        )
      }
    }
  }

  def registerMenuLabel(text: String, rect: java.awt.Rectangle) {
    new Image(
      rect.x - 25, rect.y - 25,
      gg.createLabel(
        text,
        rect.width + 50,
        rect.height,
        size = 15,
        frontColor = 0xFFFFFF
      )
    )
  }

  def createListButtonImages(text: String): List[PImage] = {
    def createImage(color: Int): PImage = {
      gg.createLabel(text, listManager.width, 25, 15, 0xFFFFFF, color)
    }

    List(0x8A4442, 0xD9CEAD, 0x473027).map(createImage)
  }

  def create() { create(parameter, focus.map(_.imageFilename).getOrElse("")) }
  def create(p: Parameter, imageFilename: String) {
    def action(listButton: listManager.Button) {
      listManager.focus = Option(listButton)
      packs.findByListButton(listButton).foreach {
        pack =>
        editor.parameter = pack.param        
        canvas.update(pack.imageFilename)
      }
    }
    
    val listButton = listManager.register(createListButtonImages(p.name)).action {
      action(_)
    }

    packs += Pack(p, listButton, imageFilename)
    if (listManager.focus.isEmpty) { action(listButton) }
  }

  def delete() {
    listManager.remove().foreach {
      packs.findByListButton(_).foreach(packs.-=)
    }
  }

  def update() {
    focus foreach {
      _.param = parameter
    }
  }

  def focus: Option[Pack] = {
    listManager.focus.flatMap {
      packs.findByListButton(_)
    }
  }

  def save() {
    val filename = applet.selectOutput()
    if (filename == null) return
    val xml = <parameters>
    {
      packs map {
        pack =>
        val p = pack.param
        <parameter>
        <name>{ p.name }</name>
        <hitPoint>{ p.hitPoint }</hitPoint>
        <moveRangePoint>{ p.moveRangePoint }</moveRangePoint>
        <attackPoint>{ p.attackPoint }</attackPoint>
        <attackRangePoint>{ p.attackRangePoint }</attackRangePoint>
        <cost>{ p.cost }</cost>
        <imageFilename>{ pack.imageFilename }</imageFilename>
        </parameter>
      }
    }
    </parameters>

    scala.xml.XML.save(filename, xml, "utf-8")
  }

  def load() {
    val filename = applet.selectInput()
    if (filename == null) return

    clear()
    
    (scala.xml.XML.loadFile(filename) \ "parameter").foreach {
      xml =>
      create(
        Parameter(
          (xml \ "name").text,
          (xml \ "hitPoint").text.toInt,
          (xml \ "moveRangePoint").text.toInt,
          (xml \ "attackPoint").text.toInt,
          (xml \ "attackRangePoint").text.toInt,
          (xml \ "cost").text.toInt
        ),
        (xml \ "imageFilename").text
      )
    }
  }

  def clear() {
    listManager.clear()
    packs.clear()
  }

  def imagePath(filename: String): String =
    IMAGE_DIR + filename + IMAGE_EXT
    
  def loadImage() {
    focus foreach {
      pack =>
      val filename = paramManager.toText('imageFilename)
      pack.imageFilename = filename
      canvas.update(filename)
    }
  }

  def parameter: Parameter = {
    import paramManager.{ toInt, toText }
    Parameter(
      toText('name),
      toInt('hitPoint),
      toInt('moveRangePoint),
      toInt('attackPoint),
      toInt('attackRangePoint),
      toInt('cost)
    )
  }

  def parameter_=(p: Parameter) {
    List(
      ('name, p.name),
      ('hitPoint, p.hitPoint),
      ('moveRangePoint, p.moveRangePoint),
      ('attackPoint, p.attackPoint),
      ('attackRangePoint, p.attackRangePoint),
      ('cost, p.cost)
    ) foreach {
      case (symbol, value) =>
      paramManager(symbol).foreach(_.setText(value.toString))
    }      
  }
  
  override def draw() {
    applet.background(156, 150, 139)
    
    listManager.checkMouse()
    listManager.draw()

    listManager.focus.foreach {
      focus =>
      applet.noStroke()
      applet.fill(255, 255, 0, 120)
      applet.rect(focus.x, focus.y, focus.width, focus.height)
    }

    buttonManager.checkMouse()
    buttonManager.draw()
    
    images.draw()
    canvas.draw()
  }
  
  override def mouseWheelMoved() {
    if (listManager.mouseContains) {
      listManager.scroll += applet.mouseWheelRotation
    }
  }
}
