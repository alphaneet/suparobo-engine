package com.github.alphaneet.suparobo_engine.tools
import com.github.alphaneet.processing.{
  ListManager,
  TextManager,
  ButtonManager
}

object UIEditor extends EditorPApplet {
  def createEditorScene = new UIEditorScene(this)
}

class UIEditorScene(applet: EditorPApplet) extends EditorScene(applet) {
  editor =>
    
  import processing.core.{ PImage, PConstants }

  // TK: MENU_LEFT_X の方がいいかも？
  // 左 or 右のメニューの x 座標という文意なのだが、MENU_LEFT_X だと、
  // とあるメニューの左 or 右の x 座標って勘違いしそうなので
  // LEFT_MENU or RIGHT_MENU にした。
  // MENU という接頭語を常に付けた方が分かりやすさはあると思う。まぁこまけぇこたぁ(ry
  val LEFT_MENU_X   = 30
  val RIGHT_MENU_X  = 1000
  val MENU_TOP      = 8
  val MENU_INTERVAL = 50

  // TK: 紛らわしいので変更する。
  // 美術用語的な方面からぐぐって、キャンバスの下地的な単語に変更する。
  // おそらくサーフェースだと思う（プラモ的に考えると）
  val CANVAS_WIDTH  = 800
  val CANVAS_HEIGHT = 600
  val CANVAS_TOP  = 20
  val CANVAS_LEFT = (applet.width >> 1) - (CANVAS_WIDTH >> 1)
  val CANVAS_RIGHT  = CANVAS_LEFT + CANVAS_WIDTH
  val CANVAS_BOTTOM = CANVAS_TOP  + CANVAS_HEIGHT
  
  val buttonManager = new ButtonManagerEx

  case class Pack(
    private var _name: String,
    private var _x: Int,
    private var _y: Int,
    private var _width: Int,
    private var _height: Int,
    panelButton: panelManager.Button,
    listButton: listManager.Button
  ) {
    def updatePanelButtonImages() {
      panelButton.images = createPanelButtonImages(name, width, height)
    }
    def updateListButtonImages() {
      listButton.images  = createListButtonImages(name)
    }
    
    def name = _name
    def name_=(name: String) {
      _name = name
      updatePanelButtonImages()
      updateListButtonImages()
    }

    def x = _x
    def x_=(x: Int) {
      _x = x
      panelButton.x = x
    }

    def y = _y
    def y_=(y: Int) {
      _y = y
      panelButton.y = y
    }

    def width = _width
    def width_=(width: Int) {
      _width = width
      updatePanelButtonImages()
    }

    def height = _height
    def height_=(height: Int) {
      _height = height
      updatePanelButtonImages()
    }
  }
  
  val packs = new scala.collection.mutable.ArrayBuffer[Pack] {
    def findByPanelButton(panelButton: panelManager.Button): Option[Pack] = {
      find(_.panelButton == panelButton)
    }
    def findByListButton(listButton: listManager.Button): Option[Pack] = {
      find(_.listButton == listButton)
    }
  }
  
  object panelManager extends ButtonManager(applet) {
    class Panel(images: List[PImage]) extends Button(images) {
      override def x = canvas.realX + (super.x * canvas.scale).toInt
      override def y = canvas.realY + (super.y * canvas.scale).toInt
      override def width  = (image.width  * canvas.scale).toInt
      override def height = (image.height * canvas.scale).toInt
      override def draw() {
        applet.image(image, x, y, width, height)
      }
    }
    
    override def createButton(images: List[PImage]) = new Panel(images)
  }
              
  val listManager = new ListManager(applet) {    
    x = RIGHT_MENU_X - 5
    y = (MENU_TOP + MENU_INTERVAL * 5) + 20
    background(110, 300, 0xEEEEEE)
  }
  
  val paramManager = new TextManager(applet) {
    paramManager =>
      
    abstract class CommonField(val symbol: Symbol) extends TextField {
      def validateValue(): Unit
      
      import java.awt.event.{ FocusAdapter, FocusEvent, ActionListener, ActionEvent }
      addFocusListener(new FocusAdapter() {
        override def focusLost(e: FocusEvent): Unit = validateValue()
      })
      addActionListener(new ActionListener() {
        def actionPerformed(e: ActionEvent): Unit = validateValue()
      })
    }
    
    class StringField(symbol: Symbol) extends CommonField(symbol) {
      def validateValue(): Unit = editor.updateStringParam(symbol, getText) 
    }
    
    class IntField(symbol: Symbol, min: Int, max: Int) extends CommonField(symbol) {
      private var _value = 0
      def value = _value
      def value_=(v: Int) {
        _value = rangeOfNumber(v, min, max)
        setText(_value.toString)
      }

      def validateValue() {
        value = try {
          getText.toInt
        } catch {
          case _ => min
        }

        editor.updateIntParam(symbol, value)
      }
      
      import javax.swing.text.{ PlainDocument, AttributeSet }
      setDocument(new PlainDocument() {
        override def insertString(offs: Int, str: String, a: AttributeSet) {
          try {
            str.toInt
            super.insertString(offs, str, a)
          } catch { case _ => return }
        }
      })      
    }

    val intFields: List[IntField] = {
      val createChangeButton = buttonManager.createButtonByBasicColor(20, 20, 15)
      val left  = LEFT_MENU_X
      val right = RIGHT_MENU_X
      val top   = MENU_TOP
      val diffY = MENU_INTERVAL
      
      List(
        ('canvasWidth,  800, 200, 2000, "キャンバスの幅",   left,  top),
        ('canvasHeight, 600, 150, 1500, "キャンバスの高さ", left,  top + diffY),
        ('gridWidth,    32,  5,   500,  "グリッドの幅",     left,  top + diffY * 2),
        ('gridHeight,   32,  5,   500,  "グリッドの高さ",   left,  top + diffY * 3),
        ('panelX,       0,   0,   2000, "パネルの x 座標",  right, top + diffY),
        ('panelY,       0,   0,   1500, "パネルの y 座標",  right, top + diffY * 2),
        ('panelWidth,   100, 5,   1000, "パネルの幅",       right, top + diffY * 3),
        ('panelHeight,  100, 5,   1000, "パネルの高さ",     right, top + diffY * 4)
      ) map {
        case (symbol, default, min, max, text, x, y) =>
        registerMenuLabel(x, y, text)
        new IntField(symbol, min, max) {
          setBounds(x + 30, y + 20, 60, 25)
          value = applet.config(symbol, default)

          List(
            (0x25C0, -10,  -10),
            (0x25C1,  10,  -1),
            (0x25B7,  90,   1),
            (0x25B6,  110,  10)
          ) foreach {
            case (arrow, addX, addValue) =>
            createChangeButton(x + addX, y + 23, arrow.toChar) {
              value += addValue
              updateIntParam(symbol, value)
            } 
          }
        }
      }
    }

    val stringFields: List[StringField] =
      List(
        ('panelName, "パネルの名前", RIGHT_MENU_X, MENU_TOP)
      ) map {
        case (symbol, text, x, y) => 
        registerMenuLabel(x, y, text)
        new StringField(symbol) {
          setBounds(x - 10 , y + 20, 140, 20)
        }
      }
    

    private val emptyIntField = new IntField('emptyIntField, 0, 0)
    private val emptyStringField = new StringField('emptyStringField)

    private def findField[T <: CommonField](symbol: Symbol)(fields: List[T], default: T): T =
      fields.find(_.symbol == symbol).getOrElse(default)
    
    def intField(symbol: Symbol): IntField = findField(symbol)(intFields, emptyIntField)
    def stringField(symbol: Symbol): StringField = findField(symbol)(stringFields, emptyStringField)

    def canvasWidth:  Int = intField('canvasWidth).value
    def canvasHeight: Int = intField('canvasHeight).value
    def panelX: Int = intField('panelX).value
    def panelY: Int = intField('panelY).value
    def panelWidth:  Int = intField('panelWidth).value
    def panelHeight: Int = intField('panelHeight).value
    def panelName: String = stringField('panelName).getText
    
    def canvasWidth_=(value: Int)  { intField('canvasWidth).value  = value }
    def canvasHeight_=(value: Int) { intField('canvasHeight).value = value }
    def panelX_=(value: Int) { intField('panelX).value = value }
    def panelY_=(value: Int) { intField('panelY).value = value }
    def panelWidth_=(value: Int)  { intField('panelWidth).value  = value }
    def panelHeight_=(value: Int) { intField('panelHeight).value = value }
    def panelName_=(value: String) { stringField('panelName).setText(value) }

    def panelParams = (panelName, panelX, panelY, panelWidth, panelHeight)    
    def panelParams_=(params: Tuple5[String, Int, Int, Int, Int]) {
      panelName = params._1
      panelX = params._2
      panelY = params._3
      panelWidth  = params._4
      panelHeight = params._5
    }
    def panelParams_=(pack: Pack) {
      panelParams = (pack.name, pack.x, pack.y, pack.width, pack.height)
    }
  }

  object canvas {
    private var _scale = 1.0f
    def scale = _scale
    def scale_=(scale: Float) {
      val (prevW, prevH) = (realWidth, realHeight)
      _scale = rangeOfNumber(scale, 0.3f, 3.0f)

      x += (prevW - realWidth)  / 2
      y += (prevH - realHeight) / 2 
    }

    private var _x = 0
    def x = _x
    def x_=(x: Int) {
      _x = rangeOfNumber(x, -realWidth + 5, CANVAS_WIDTH - 5)
    }
    def realX = x + CANVAS_LEFT
    
    private var _y = 0
    def y = _y
    def y_=(y: Int) {
      _y = rangeOfNumber(y, -realHeight + 5, CANVAS_HEIGHT - 5)
    }
    def realY = y + CANVAS_TOP
    
    var width = paramManager.canvasWidth
    def realWidth = (width.toFloat * scale).toInt
    
    var height = paramManager.canvasHeight
    def realHeight = (height.toFloat * scale).toInt
  }
  
  // initialize
  {    
    import buttonManager.createButtonByBasicColor
    val createListButton = createButtonByBasicColor(60, 20, 13)

    val right = RIGHT_MENU_X
    val top  = MENU_TOP + MENU_INTERVAL * 5
    registerMenuLabel(right, top, "パネルリスト")
        
    createListButton(right -  5, 590, "new")    { editor.create() }
    createListButton(right + 65, 590, "delete") { editor.remove() }

    listManager.scrollWidth = 20
    listManager.scrollBackground = 0xCCCCCC

    def createScrollButton(arrow: Int): List[PImage] = {
      val text   = arrow.toChar.toString
      val size   = listManager.scrollWidth
      val create = gg.createLabel(text, size, size, 15, _: Int, _: Int)
      List(
        create(0xFFFFFF, 0x555555),
        create(0xAAAAAA, 0x555555),
        create(0x333333, 0x555555)
      )
    }
    listManager.scrollUpButton   = createScrollButton(0x25b2)
    listManager.scrollDownButton = createScrollButton(0x25bc)
    listManager.scrollBarButton  = createScrollButton(0x25a0)
  }

  def registerMenuLabel(x: Int, y: Int, text: String): Image = {
    new Image(
      x, y,
      gg.createLabel(
        text,
        width  = 130,
        height = 20,
        size   = 13,
        frontColor = 0x333333,
        align  = PConstants.LEFT
      )
    )
  }

  def createPanelButtonImages(text: String, width: Int, height: Int): List[PImage] = {
    def createImage(color: Int): PImage = {
      gg.createLabel(
        text,
        width,
        height,
        size = 18,
        frontColor  = color,
        frameWeight = 1,
        frameColor  = color
      )
    }

    List(0x000000, 0xFF3333, 0x000000).map(createImage)
  }

  def createListButtonImages(text: String): List[PImage] = {
    def createImage(color: Int): PImage = {
      gg.createLabel(text, listManager.width, 30, 15, 0xFFFFFF, color)
    }

    List(0x666666, 0x999999, 0x555555).map(createImage)
  }

  var _focus: Option[Pack] = None
  def focus = _focus
  def focus_=(focus: Option[Pack]) {
    _focus = focus
    _focus foreach { paramManager.panelParams = _ }
  }
  
  def create() {
    val (name, x, y, width, height) = paramManager.panelParams

    val pack = Pack(
      name,
      x,
      y,
      width,
      height,
      panelManager.register(createPanelButtonImages(name, width, height), x, y).action
      {
        panelButton =>
        focus = packs.findByPanelButton(panelButton)
      },
      listManager.register(createListButtonImages(name), 0, 0).action {
        listButton =>
        focus = packs.findByListButton(listButton)
      }
    )
    
    if (focus.isEmpty) focus = Option(pack)

    packs += pack  
  }

  def remove() {
    focus foreach {
      pack =>
      panelManager.unregister(pack.panelButton)
      
      listManager.focus = Option(pack.listButton)      
      listManager.remove()

      packs -= pack
    }

    focus = listManager.focus.flatMap { packs.findByListButton }
  }
  
  def updateIntParam(symbol: Symbol, value: Int) {
    symbol match {
      case 'canvasWidth  => canvas.width  = value
      case 'canvasHeight => canvas.height = value
      case 'gridWidth  =>
      case 'gridHeight =>
      case _ =>
    }
    
    focus foreach {
      pack =>
      symbol match {
        case 'panelX => pack.x = value
        case 'panelY => pack.y = value
        case 'panelWidth  => pack.width  = value
        case 'panelHeight => pack.height = value
        case _ =>
      }
    }
  }

  def updateStringParam(symbol: Symbol, value: String) {    
    focus foreach {
      pack =>
      symbol match {
        case 'panelName => pack.name = value
        case _ =>
      }
    }
  }

  override def draw() {
    applet.noStroke()
    applet.fill(200)
    applet.rect(CANVAS_LEFT, CANVAS_TOP, CANVAS_WIDTH, CANVAS_HEIGHT)

    applet.fill(255)
    applet.rect(canvas.realX, canvas.realY, canvas.realWidth, canvas.realHeight)
    
    panelManager.checkMouse()
    panelManager.draw()

    focus foreach {
      pack =>
      val p = pack.panelButton
      applet.noStroke()
      applet.fill(200, 200, 200, 128)
      applet.rect(p.x, p.y, p.width, p.height)
    }

    // 画面の手前と奥の壁
    applet.fill(200, 200, 255)
    applet.rect(0, 0, applet.width, CANVAS_TOP)
    applet.rect(0, CANVAS_BOTTOM, applet.width, applet.height - CANVAS_BOTTOM)
    applet.rect(0, CANVAS_TOP, CANVAS_LEFT, CANVAS_HEIGHT)
    applet.rect(CANVAS_RIGHT, CANVAS_TOP, applet.width - CANVAS_RIGHT, CANVAS_HEIGHT)

    buttonManager.checkMouse()
    buttonManager.draw()
    
    images.draw()
    
    listManager.checkMouse()
    listManager.draw()

    focus foreach {
      pack =>

      val l = pack.listButton
      if (!l.status.isDisabled) {
        applet.noFill()
        applet.stroke(255, 255, 0)
        applet.strokeWeight(2)
        applet.rect(l.x, l.y, l.width, l.height)
      }
    }
  }

  override def keyPressed() {
    if (listManager.mouseContains) {
      if (applet.keyCode == PConstants.UP  ) listManager.scroll += 1
      if (applet.keyCode == PConstants.DOWN) listManager.scroll -= 1      
    }
  }
  
  override def mouseWheelMoved() {
    if (listManager.mouseContains) {
      listManager.scroll += applet.mouseWheelRotation
    }

    if (mouseContains(CANVAS_LEFT, CANVAS_TOP, CANVAS_WIDTH, CANVAS_HEIGHT)) {
      canvas.scale += applet.mouseWheelRotation.toFloat / 100
    }
  }

  override def mouseDragged() {
    if (mouseContains(CANVAS_LEFT, CANVAS_TOP, CANVAS_WIDTH, CANVAS_HEIGHT)) {
      canvas.x += applet.mouseX - applet.pmouseX
      canvas.y += applet.mouseY - applet.pmouseY
    }
  }
}
