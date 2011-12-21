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

  // TK: MENU_(LEFT or RIGHT)_X の方がいいかも？
  val LEFT_MENU_X   = 30
  val RIGHT_MENU_X  = 1000
  val MENU_TOP      = 8
  val MENU_INTERVAL = 50

  val WORKSPACE_WIDTH = 800
  val WORKSPACE_HEIGHT = 600
  val WORKSPACE_TOP = 20
  val WORKSPACE_LEFT = (applet.width >> 1) - (WORKSPACE_WIDTH >> 1)
  val WORKSPACE_RIGHT = WORKSPACE_LEFT + WORKSPACE_WIDTH
  val WORKSPACE_BOTTOM = WORKSPACE_TOP + WORKSPACE_HEIGHT
  
  val buttonManager = new ButtonManagerEx

  var _focus: Option[Pack] = None
  def focus = _focus
  def focus_=(focus: Option[Pack]) {
    _focus = focus
    _focus foreach { paramManager.panelParams = _ }
  }
  
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

    override def mouseClicked(button: Button): Boolean =
      mousePressed && button.status.isOver
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
        ('gridWidth,    50,  5,   500,  "グリッドの幅",     left,  top + diffY * 2),
        ('gridHeight,   50,  5,   500,  "グリッドの高さ",   left,  top + diffY * 3),
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
    def gridWidth: Int  = intField('gridWidth).value
    def gridHeight: Int = intField('gridHeight).value
    
    def panelX: Int = intField('panelX).value
    def panelY: Int = intField('panelY).value
    def panelWidth:  Int = intField('panelWidth).value
    def panelHeight: Int = intField('panelHeight).value
    def panelName: String = stringField('panelName).getText
    
    def canvasWidth_=(value: Int)  { intField('canvasWidth).value  = value }
    def canvasHeight_=(value: Int) { intField('canvasHeight).value = value }
    def gridWidth_=(value: Int)  { intField('gridWidth).value  = value }
    def gridHeight_=(value: Int) { intField('gridHeight).value = value }
    
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
      _x = rangeOfNumber(x, -realWidth + 5, WORKSPACE_WIDTH - 5)
    }
    def realX = x + WORKSPACE_LEFT
    
    private var _y = 0
    def y = _y
    def y_=(y: Int) {
      _y = rangeOfNumber(y, -realHeight + 5, WORKSPACE_HEIGHT - 5)
    }
    def realY = y + WORKSPACE_TOP
    
    var width = paramManager.canvasWidth
    def realWidth = (width.toFloat * scale).toInt
    
    var height = paramManager.canvasHeight
    def realHeight = (height.toFloat * scale).toInt
  }

  object grid {
    var margin = 2
    var isActive = false
    def width  = paramManager.gridWidth
    def height = paramManager.gridHeight

    def fit(value: Int, length: Int): Int = {
      val delay = value + margin
      if ( isActive && (delay % length) <= (margin << 1) ) {
        (delay / length) * length
      } else {
        value
      }
    }
    
    def fitX(x: Int): Int = fit(x, width)
    def fitY(y: Int): Int = fit(y, height)
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
    
    val createMenuButton = createButtonByBasicColor(150, 30, 16)
    createMenuButton(15, 230, "grid") { grid.isActive = !grid.isActive }
    createMenuButton(15, 280, "save") { saveXML() }
    createMenuButton(15, 330, "load") { loadXML() }
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

  def saveXML(filename: String = applet.selectOutput()) {
    if (filename == null) return
    val xml = <layouts>
    {
      packs map {
        pack =>
        <layout>
        <name>{ pack.name }</name>
        <x>{ pack.x }</x>
        <y>{ pack.y }</y>
        <width>{ pack.width }</width>
        <height>{ pack.height }</height>
        </layout>
      }
    }
    </layouts>
    
    scala.xml.XML.save(filename, xml, "utf-8")
  }

  def loadXML(filename: String = applet.selectInput()) {
    if (filename == null) return    
    clear()

    // TK: ちゃんとしたデータが入ってるってあたし信じてるから！！！
    // Modツールとしてユーザーに提供する時はチェック追加しましょうね＾ω＾；
    (xml.XML.loadFile(filename) \ "layout").foreach {
      layout =>
      create(
        (layout \ "name").text,
        (layout \ "x").text.toInt,        
        (layout \ "y").text.toInt,
        (layout \ "width").text.toInt,
        (layout \ "height").text.toInt
      )
    }
  }

  def clear() {
    focus = None
    listManager.clear()
    panelManager.clear()
    packs.clear()
  }
  
  def create(params: Tuple5[String, Int, Int, Int, Int] = paramManager.panelParams) {
    val (name, x, y, width, height) = params

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
    applet.rect(WORKSPACE_LEFT, WORKSPACE_TOP, WORKSPACE_WIDTH, WORKSPACE_HEIGHT)

    applet.fill(255)
    applet.rect(canvas.realX, canvas.realY, canvas.realWidth, canvas.realHeight)

    if (grid.isActive) {
      applet.stroke(100)
      applet.strokeWeight(1)

      val sx = canvas.realX.toFloat
      val ex = (sx + canvas.realWidth).toFloat
      val stepX = grid.width.toFloat * canvas.scale
      (sx to ex by stepX) foreach {
        x =>
        applet.line(x, canvas.realY, x, canvas.realY + canvas.realHeight)
      }

      val sy = canvas.realY.toFloat
      val ey = (sy + canvas.realHeight).toFloat
      val stepY = grid.height.toFloat * canvas.scale
      (sy to ey by stepY) foreach {
        y =>
        applet.line(canvas.realX, y, canvas.realX + canvas.realWidth, y)
      }
      
      applet.noStroke()
    }
    
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
    applet.rect(0, 0, applet.width, WORKSPACE_TOP)
    applet.rect(0, WORKSPACE_BOTTOM, applet.width, applet.height - WORKSPACE_BOTTOM)
    applet.rect(0, WORKSPACE_TOP, WORKSPACE_LEFT, WORKSPACE_HEIGHT)
    applet.rect(WORKSPACE_RIGHT, WORKSPACE_TOP, applet.width - WORKSPACE_RIGHT, WORKSPACE_HEIGHT)

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

    if ( mouseContainsByWorkspace ) {
      canvas.scale += applet.mouseWheelRotation.toFloat / 100
    }
  }

  override def mouseDragged() {
    if ( mouseContainsByWorkspace ) {
      val (diffX, diffY) = (applet.mouseX - applet.pmouseX, applet.mouseY - applet.pmouseY)
      
      if ( panelManager.isLock ) {
        focus foreach {
          pack =>
          pack.x += (diffX / canvas.scale).toInt
          pack.x = grid.fitX(pack.x)
          pack.y += (diffY / canvas.scale).toInt
          pack.y = grid.fitY(pack.y)
          paramManager.panelParams = pack
        }
      } else {
        canvas.x += diffX
        canvas.y += diffY
      }
    }
  }

  def mouseContainsByWorkspace: Boolean =
    mouseContains(
      WORKSPACE_LEFT,
      WORKSPACE_TOP,
      WORKSPACE_WIDTH,
      WORKSPACE_HEIGHT
    )
}
