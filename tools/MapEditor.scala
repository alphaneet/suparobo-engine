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

trait EditorPApplet extends PApplet {
  val clazzName = {
    val tmp = getClass.getName
    if (tmp.endsWith("$")) tmp.init else tmp
  }

  val config = new ConfigXML(clazzName + ".xml")
  def createEditorScene: Scene

  override def setup() {    
    size(config('width, 800), config('height, 600))
    frameRate(24)
    scene = createEditorScene
  }
}

class EditorScene(val applet: EditorPApplet) extends Scene {
  editor =>

  import processing.core.{ PImage, PVector }
  
  val gg = new GraphicsGenerator(applet)
  val images= new scala.collection.mutable.ArrayBuffer[Image] {
    def drawAll(): Unit = foreach { _.draw() }
    def update(v: Image, image: PImage) {
      find(_ == v) foreach { _.image = Option(image) }
    }
  }
  
  class ButtonManagerEx extends ButtonManager(applet) {
    def createEasyButton
    (releasedFront: Int, pressedFront: Int, back: Int)
    (w: Int, h: Int, size: Int)
    (x: Int, y: Int, anyText: Any)
    (action: => Unit)
    {
      val text = anyText.toString
      val labels = (
        gg.createLabel(text, w, h, size, releasedFront, back),
        gg.createLabel(text, w, h, size, releasedFront, back),
        gg.createLabel(text, w, h, size, pressedFront,  back)
      )
      register(labels, new processing.core.PVector(x, y))(action)
    }

    val createButtonByBasicColor = createEasyButton(0xFFFFFFF, 0xAAAAAA, 0x333333)_
  }

  class Image(var pos: PVector = new PVector(), _image: PImage = null) {
    def this(x: Int, y: Int, _image: PImage = null) = this(new PVector(x, y), _image)
    
    var image = Option(_image)
    
    editor.images += this
    def draw(): Unit = image foreach { applet.image(_, pos.x, pos.y) }
  }
}

object MapEditor extends EditorPApplet {
  def createEditorScene = new MapEditorScene(this)
}

class MapEditorScene(applet: EditorPApplet)
extends EditorScene(applet)
with Util
{
  editor =>

  import processing.core.{ PImage, PVector, PConstants }
  import applet.config
  
  val CANVAS_LEFT   = 240
  val CANVAS_TOP    = 50
  val CANVAS_WIDTH  = 510
  val CANVAS_HEIGHT = 510
  val CANVAS_BOTTOM = CANVAS_TOP + CANVAS_HEIGHT
  val CANVAS_RIGHT  = CANVAS_LEFT + CANVAS_WIDTH

  val MINIMAP_LEFT   = 10
  val MINIMAP_TOP    = 380
  val MINIMAP_WIDTH  = 200
  val MINIMAP_HEIGHT = 200

  val SCALE_VALUE = config('scale_value, 100.0f)
  val MIN_SCALE   = config('min_scale, 50.0f)
  val MAX_SCALE   = config('max_scale, 200.0f)

  val MIN_COLUMN = config('min_column, 5)
  val MAX_COLUMN = config('max_column, 50)
  val MIN_ROW    = config('min_row, 5)
  val MAX_ROW    = config('max_row, 50)

  private var _canvasX = CANVAS_LEFT
  private var _canvasY = CANVAS_TOP
  var isCanvasDragged = false

  def canvasX = _canvasX
  def canvasY = _canvasY

  def canvasX_=(x: Int) {
    val w = realCanvasWidth
    _canvasX = rangeOfNumber(
      x,
      math.min(CANVAS_LEFT, CANVAS_RIGHT - w),
      math.max(CANVAS_LEFT, CANVAS_LEFT + (CANVAS_WIDTH - w))
    )
  }
  def canvasY_=(y: Int) {
    val h = realCanvasHeight
    _canvasY = rangeOfNumber(
      y,
      math.min(CANVAS_TOP, CANVAS_BOTTOM - h),
      math.max(CANVAS_TOP, CANVAS_TOP + (CANVAS_HEIGHT - h))
    )
  }
  
  private var _column = config('init_column, 20)  
  private var _row    = config('init_row, 20)

  def column = _column  
  def row    = _row
  
  def column_=(column: Int) {
    _column = rangeOfNumber(column, MIN_COLUMN, MAX_COLUMN)
  }
  def row_=(row: Int) {
    _row = rangeOfNumber(row, MIN_ROW, MAX_ROW)
  }
  
  // TK: こういうの標準でありませんか？あったら教えてぴょ
  def rangeOfNumber[T: Ordering](v: T, min: T, max: T): T = {
    val ord = implicitly[Ordering[T]]

    if (ord.lt(v, min)) min
    else if (ord.gt(v, max)) max
    else v
  }
    
  val buttonManager = new ButtonManagerEx
  
  object keys {
    val values = scala.collection.mutable.ArrayBuffer[Key]()  
    val config = new ConfigXML(applet.config.elem \ "keys")
    case class Key(symbol: Symbol, default: Char, text: String)(_action: => Unit) {
      val action = () => {        
        _action
        editor.updateInfo()
      }
      val char = config(symbol, default)
      values += this
    }

    def setFocus(symbol: Symbol) {
      editor.statuses find {
        _.symbol == symbol
      } foreach { _.focus() }
    }
    
    Key('flat,  'q', ": 平地を選択（移動力 -1）") { setFocus('ft) }
    Key('wood,  'w', ": 森を選択（移動力 -2）")   { setFocus('wd) }
    Key('hill,  'a', ": 丘を選択（移動力 -3）")   { setFocus('hl) }
    Key('mount, 's', ": 山を選択（移動不可能）")  { setFocus('mt) }
    
    Key('zoom_in,  'z', ": キャンバスの拡大") { editor.scaleValue += 20 }
    Key('zoom_out, 'x', ": キャンバスの縮小") { editor.scaleValue -= 20 }
    
    Key('left,   'c', ": キャンバスの左端に移動") {
      canvasX = CANVAS_RIGHT + realCanvasWidth
    }
    Key('right,  'b', ": キャンバスの右端に移動") {
      canvasX = CANVAS_LEFT - realCanvasWidth
    }
    Key('top,    'f', ": キャンバスの上端に移動") {
      canvasY = CANVAS_BOTTOM + realCanvasHeight
    }
    Key('bottom, 'v', ": キャンバスの下端に移動") {
      canvasY = CANVAS_TOP - realCanvasHeight
    }
    
    Key('column_plus,  't', ": キャンバスの幅を増やす") { editor.column += 1 }
    Key('column_minus, 'r', ": キャンバスの幅を減らす") { editor.column -= 1 }
    Key('row_plus,     'd', ": キャンバスの高さを増やす")  { editor.row += 1 }
    Key('row_minus,    'e', ": キャンバスの高さを減らす")  { editor.row -= 1 }

    Key('save, 'o', ": 保存画面を開く")     { saveBinary() }
    Key('load, 'l', ": 読み込み画面を開く") { loadBinary() }
    
    Key('help, 'h', ": ヘルプの開閉") {
      if (help.isOpen) help.close() else help.open()
    }
    
    Key('None, ' ', "左クリックでマップチップの配置") {}
    Key('None, ' ', "右ドラッグでキャンバスの移動") {}
    Key('None, ' ', "スクロールで拡大／縮小") {}
  }
  
  val help = new ButtonManagerEx {
    private var _isOpen = false
    def isOpen  = _isOpen

    def open()  { _isOpen = true }
    def close() { _isOpen = false }

    createButtonByBasicColor(120, 30, 20)(340, 500, "閉じる") { close() }

    val labels = keys.values map {
      case key =>
      val char = if (key.symbol == 'None) "" else key.char
      gg.createLabel(
        char + key.text, 250, 40,
        size  = 15,
        front = 60,
        align = PConstants.LEFT
      )
    }
    
    override def draw() {
      applet.strokeWeight(2)
      applet.stroke(30)
      applet.fill(230, 230, 255, 220)
      applet.rect(100, 50, 600, 500)

      labels.zipWithIndex foreach {
        case (label, index) =>
        val x = index / 10
        val y = index % 10
        applet.image(label, 140 + x * 300, 60 + y * 40)
      }
        
      super.draw()      
    }
  }

  val columnImage = new Image(650, 10)
  val rowImage    = new Image(710, 10)

  var divWidth  = 30
  var divHeight = 30
  
  private var _scaleValue = SCALE_VALUE
  def scaleValue = _scaleValue
  def scaleValue_=(value: Float) {
    _scaleValue = rangeOfNumber(value, MIN_SCALE, MAX_SCALE)
  }
  def scale = scaleValue / SCALE_VALUE  

  def realCanvasWidth: Int = ( column * divWidth * scale ).toInt
  def realCanvasHeight: Int = ( row * divHeight * scale ).toInt

  def createInfoLabel(text: String) =
    gg.createLabel(text, text.length * 20, 25, 20, 0x333333)
  
  def updateInfo() {
    List( (columnImage, column), (rowImage, row) ) foreach {
      case (image, value) =>
      editor.images(image) = createInfoLabel(value.toString)
    }
  }
  
  abstract case class Status(id: Int, symbol: Symbol, color: Int, image: PImage)
                extends NotNull { def focus(): Unit }
  
  val statuses: Seq[Status] = {
    val (sx, sy) = (30, 30)
    val (ex, ey) = (120, 120)
    
    List(
      (1, 'ft, 0x99FF99, sx, sy),
      (3, 'wd, 0x009900, ex, sy),
      (4, 'hl, 0x999933, sx, ey),
      (5, 'mt, 0x999999, ex, ey)
    ) map {
      case (id, symbol, color, x, y) =>

      val image = gg.createAndDrawPImage(divWidth, divHeight) {
        g =>
        val c = gg.rgb(color)
        g.stroke(60)
        g.fill(c._1, c._2, c._3)
        g.rect(0, 0, divWidth - 1, divHeight - 1)
      }

      val status = new Status(id, symbol, color, image) {
        def focus() {
          editor.focus.status = this
          editor.focus.pos = new PVector(x, y)
        }
      }
        
      buttonManager.createEasyButton(0x444444, 0, color)(
        60, 60, 18)(x, y, symbol.toString)
      {
        status.focus()
      }
      
      status
    }
  }

  val buffer = Array.fill(MAX_COLUMN * MAX_ROW)(statuses.head)
  
  object focus {
    var pos = new PVector(30, 30)
    var status = statuses.head
    def draw() {
      applet.stroke(255, 60, 60)
      applet.strokeWeight(3)
      applet.noFill()
      applet.rect(pos.x, pos.y, 60, 60)
    }
  }  

  val background = gg.createAndDrawPImage(applet.width, applet.height) {
    g =>
    g.noStroke()
    g.rect(0, 0, applet.width, CANVAS_TOP)
    g.rect(0, 0, CANVAS_LEFT, applet.height)
    g.rect(0, CANVAS_BOTTOM, applet.width,  applet.height - CANVAS_BOTTOM)    
    g.rect(CANVAS_RIGHT, 0, applet.width - CANVAS_RIGHT,  applet.height)
  }

  // initialize
  {
    import buttonManager.createButtonByBasicColor

    val createMenuButton = createButtonByBasicColor(150, 30, 12)

    createMenuButton(30, 210, "Save") { saveBinary() }
    createMenuButton(30, 255, "Load") { loadBinary() }
    createMenuButton(30, 300, "Help") { help.open()  }
    
    val size = 20
    val (top, left) = (CANVAS_TOP - size, CANVAS_LEFT - size)
    val createToolButton = createButtonByBasicColor(size, size, 15)

    List(
      (left + size,       top, 0x25C0, -10, 0),
      (left + (size * 2), top, 0x25C1, -1,  0),
      (left + (size * 3), top, 0x25B7,  1,  0),
      (left + (size * 4), top, 0x25B6, 10,  0),
                 
      (left, top + size,       0x25b2, 0, -10),
      (left, top + (size * 2), 0x25b3, 0,  -1),
      (left, top + (size * 3), 0x25bd, 0,   1),
      (left, top + (size * 4), 0x25bc, 0,  10)
    ) foreach {
      case (x, y, arrow, addColumn, addRow) =>
      createToolButton(x, y, arrow.toChar) {
        editor.column += addColumn
        editor.row    += addRow
        updateInfo()
      }
    }

    List(
      (left + CANVAS_WIDTH - size, top, "-", -20),
      (left + CANVAS_WIDTH,        top, "+",  20)
    ) foreach {
      case (x, y, ch, value) =>
      createToolButton(x, y, ch) {
        editor.scaleValue += value
      }
    }

    applet.addMouseWheelListener(new java.awt.event.MouseWheelListener() {
      def mouseWheelMoved(e: java.awt.event.MouseWheelEvent) {
        editor.scaleValue += e.getWheelRotation()
      }
    })
    
    new Image(690, 10, createInfoLabel("x"))
    updateInfo()
  }

  def isInsideInBuffer(index: Int): Boolean = {
    if (index < 0) false
    
    val x = index % MAX_COLUMN
    val y = index / MAX_COLUMN

    (x < editor.column && y < editor.row)    
  }
  
  def saveBinary(filename: String = applet.selectOutput()) {
    if (filename == null) return
    
    val size = Array[Byte](column.toByte, row.toByte)
    val data: Array[Byte] = buffer.zipWithIndex withFilter {
      case(_, index) => isInsideInBuffer(index)
    } map(_._1.id.toByte)
    
    applet.saveBytes(filename, size ++ data)
  }

  def loadBinary(filename: String = applet.selectInput()) {    
    if (filename == null) return

    val bytes = applet.loadBytes(filename)
    val (size, data) = bytes.splitAt(2)

    editor.column = size(0)
    editor.row = size(1)

    data.zipWithIndex foreach {
      case (id, dataIndex) =>

      val x = dataIndex % editor.column
      val y = dataIndex / editor.column
      val bufferIndex = (x + y * MAX_COLUMN)
        
      if (isInsideInBuffer(bufferIndex)) {
        buffer(bufferIndex) = statuses find(_.id == id) getOrElse(statuses.head)
      }
    }
    
    updateInfo()
  }

  override def draw() {
    import applet.{ rect }
    
    case class MiniMap(x: Float, y: Float, color: Int)
    val miniMapBuffer = scala.collection.mutable.ArrayBuffer[MiniMap]()    
    val miniMapSize = math.max(editor.column, editor.row)
    
    val miniMapChipWidth = MINIMAP_WIDTH.toFloat / miniMapSize
    val miniMapChipHeight = MINIMAP_HEIGHT.toFloat / miniMapSize
    
    val miniMapWidth = editor.column * miniMapChipWidth
    val miniMapHeight = editor.row * miniMapChipHeight
    
    val scaleWidth  = editor.divWidth  * editor.scale
    val scaleHeight = editor.divHeight * editor.scale

    val realCanvasWidth = editor.realCanvasWidth
    val realCanvasHeight = editor.realCanvasHeight

    applet.noStroke()
    applet.fill(255)
    applet.rect(CANVAS_LEFT, CANVAS_TOP, CANVAS_WIDTH, CANVAS_HEIGHT)    
    
    buffer.zipWithIndex foreach {
      case (status, index) =>
                
      val x = index % MAX_COLUMN
      val y = index / MAX_COLUMN

      if (x < editor.column && y < editor.row) {
        val posX = canvasX + (x * scaleWidth)
        val posY = canvasY + (y * scaleHeight)
        if (
          posX + scaleWidth > CANVAS_LEFT &&
          posX < CANVAS_RIGHT &&
          posY + scaleHeight > CANVAS_TOP &&
          posY < CANVAS_BOTTOM
        ) {
          applet.image(status.image, posX, posY, scaleWidth, scaleHeight)
        }

        miniMapBuffer += MiniMap(
          x * miniMapChipWidth,
          y * miniMapChipHeight,
          status.color
        )
      }
    }
    
    applet.image(background, 0, 0)
    applet.noStroke()
    miniMapBuffer foreach {
      miniMap =>
      val c = gg.rgb(miniMap.color)
      applet.fill(c._1, c._2, c._3)
      applet.rect(
        MINIMAP_LEFT + miniMap.x,
        MINIMAP_TOP + miniMap.y,
        miniMapChipWidth,
        miniMapChipHeight
      )
    }

    {      
      applet.stroke(255, 100, 100)
      applet.strokeWeight(1)
      applet.noFill()

      val scaleWidth = (CANVAS_LEFT - canvasX).toFloat / realCanvasWidth
      val scaleHeight = (CANVAS_TOP - canvasY).toFloat / realCanvasHeight

      val x = miniMapWidth * scaleWidth
      val y = miniMapHeight * scaleHeight

      val w = miniMapWidth * (CANVAS_WIDTH.toFloat / realCanvasWidth)
      val h = miniMapHeight * (CANVAS_HEIGHT.toFloat / realCanvasHeight)

      applet.rect(
        MINIMAP_LEFT + math.max(x, 0),
        MINIMAP_TOP + math.max(y, 0),
        math.min(w, MINIMAP_WIDTH),
        math.min(h, MINIMAP_HEIGHT)
      )
    }
    
    {
      val size = 20
      val (top, left) = (CANVAS_TOP - size, CANVAS_LEFT - size)
      val (width, height) = (CANVAS_WIDTH + (size << 1), CANVAS_HEIGHT + (size << 1))
        
      applet.noStroke()
      applet.fill(90)
        
      rect(left, top,  width, size)
      rect(left, top,  size,  height)
      
      rect(left, 560,  width, size)
      rect(750,  top,  size,  height)
    }
    
    if (!help.isOpen) buttonManager.checkMouse()
    buttonManager.draw()

    images.drawAll()
    focus.draw()
  
    if (help.isOpen) {
      help.checkMouse()
      help.draw()
    }
    
    if (
      applet.isMousePressed &&
      applet.mouseButton == PConstants.LEFT &&
      !help.isOpen
    ) {
      val diffX = (applet.mouseX - canvasX)
      val diffY = (applet.mouseY - canvasY)

      val x = (diffX / scaleWidth).toInt
      val y = (diffY / scaleHeight).toInt
      if (diffX > 0 && x < editor.column &&  diffY > 0 && y < editor.row) {
        buffer(x + y * MAX_COLUMN) = focus.status
      }
    }

    if (isCanvasDragged) {
      canvasX += (applet.mouseX - applet.pmouseX)
      canvasY += (applet.mouseY - applet.pmouseY)
    }
  }
  
  override def mousePressed() =
    if (
      applet.mouseButton == PConstants.RIGHT &&
      util.isMouseInside(CANVAS_LEFT, CANVAS_TOP, CANVAS_WIDTH, CANVAS_HEIGHT) &&
      !help.isOpen
    ) isCanvasDragged = true

  override def mouseReleased() = isCanvasDragged = false

  override def keyTyped() = keys.values find {
    key =>
    key.char.toUpper == applet.key || key.char.toLower == applet.key
  } foreach { _.action() }
}
