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

  val LEFT_MENU_X   = 15
  val RIGHT_MENU_X  = 665
  val MENU_TOP      = 8
  val MENU_INTERVAL = 50

  val CANVAS_X = 150
  val CANVAS_Y = 50
  val CANVAS_WIDTH  = 500
  val CANVAS_HEIGHT = 500

  import processing.core.{ PImage, PConstants }
  
  val buttonManager = new ButtonManagerEx

  case class Pack(var name: String, panelButton: panelManager.Button, listButton: listManager.Button)  
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
      override def x_=(x: Int) { super.x = x + CANVAS_X }
      override def y_=(y: Int) { super.y = y + CANVAS_Y }
    }

    override def createButton(images: List[PImage]) = new Panel(images)
  }
              
  val listManager = new ListManager(applet) {    
    x = 660
    y = (MENU_TOP + MENU_INTERVAL * 5) + 20
    background(110, 270, 0xEEEEEE)
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
        ('panelWidth,   50,  5,   1000, "パネルの幅",       right, top + diffY * 3),
        ('panelHeight,  50,  5,   1000, "パネルの高さ",     right, top + diffY * 4)
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

    def panelX: Int = intField('panelX).value
    def panelY: Int = intField('panelY).value
    def panelWidth:  Int = intField('panelWidth).value
    def panelHeight: Int = intField('panelHeight).value
    def panelName: String = stringField('panelName).getText

    def panelX_=(value: Int) { intField('panelX).value = value }
    def panelY_=(value: Int) { intField('panelY).value = value }
    def panelWidth_=(value: Int)  { intField('panelWidth).value  = value }
    def panelHeight_=(value: Int) { intField('panelHeight).value = value }
    def panelName_=(value: String) { stringField('panelName).setText(value) }

    def panelParams = (panelName, panelX, panelY, panelWidth, panelHeight)
    def panelParams_=(params: Tuple5[String, Int, Int, Int, Int]) {
      panelName = params._1
      panelX = params._2 - CANVAS_X
      panelY = params._3 - CANVAS_Y
      panelWidth  = params._4
      panelHeight = params._5
    }
  }

  // initialize
  {
    import buttonManager.createButtonByBasicColor
    val createListButton = createButtonByBasicColor(60, 20, 13)

    val left = RIGHT_MENU_X
    val top  = MENU_TOP + MENU_INTERVAL * 5
    registerMenuLabel(left, top, "パネルリスト")
        
    createListButton(660, 560, "new")    { editor.create() }
    createListButton(730, 560, "delete") { editor.remove() }

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
      gg.createLabel(text, width, height, 12, color, frameWeight = 1, frameColor = color)
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
    _focus foreach {
      pack =>
      val p = pack.panelButton
      paramManager.panelParams = (
        pack.name,
        p.x,
        p.y,
        p.width,
        p.height
      )
    }
  }
  
  def create() {
    val (name, x, y, width, height) = paramManager.panelParams

    val pack = Pack(
      name,
      panelManager.register(createPanelButtonImages(name, width, height), x, y).action {
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

    focus = listManager.focus match {
      case Some(listButton) => packs.findByListButton(listButton)
      case _ => None
    }
  }
  
  def updateIntParam(symbol: Symbol, value: Int) {
    if ( List('panelX, 'panelY, 'panelWidth, 'panelHeight).contains(symbol) ) {
      focus foreach {
        pack =>
        val p = pack.panelButton
        symbol match {
          case 'panelX => p.x = value
          case 'panelY => p.y = value
          case 'panelWidth  => p.images = createPanelButtonImages(pack.name, value, p.height)
          case 'panelHeight => p.images = createPanelButtonImages(pack.name, p.width, value)
        }
      }
    }
  }

  def updateStringParam(symbol: Symbol, value: String) {    
    if ( List('panelName).contains(symbol) ) {
      focus foreach {
        pack =>
        val p = pack.panelButton
        symbol match {
          case 'panelName =>
            pack.name = value
            p.images = createPanelButtonImages(value, p.width, p.height)
            pack.listButton.images = createListButtonImages(value)
        }
      }
    }
  }
  
  override def draw() {
    applet.background(200, 200, 255)
    applet.noStroke()
    applet.fill(250)
    applet.rect(150, 50, 500, 500)

    buttonManager.checkMouse()
    buttonManager.draw()
    
    images.draw()
    
    listManager.checkMouse()
    listManager.draw()

    panelManager.checkMouse()
    panelManager.draw()

    focus foreach {      
      pack =>

      val p = pack.panelButton
      applet.noStroke()
      applet.fill(200, 200, 200, 128)
      applet.rect(p.x, p.y, p.width, p.height)

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
  }
}
