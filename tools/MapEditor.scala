class ConfigXML(elem: scala.xml.Node) {
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
    try {
      val text = (elem \ name).text      
      (default match {
        case _: Int => java.lang.Integer.decode(text)
        case _: Float => text.toFloat        
        case _: String => text
        case _: Symbol => Symbol(text)        
      }).asInstanceOf[T]
    } catch {
      case ex => {
        Console.err.println("use default: " + default + " --- " + ex)
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
  
  val CANVAS_LEFT = 240
  val CANVAS_TOP = 50
  val CANVAS_WIDTH = 510
  val CANVAS_HEIGHT = 510
  val CANVAS_BOTTOM = CANVAS_TOP + CANVAS_HEIGHT
  val CANVAS_RIGHT = CANVAS_LEFT + CANVAS_WIDTH

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
  val help = new ButtonManagerEx {
    var isView = false

    override def draw() {
      super.draw()
    }
  }

  val columnImage = new Image(650, 10)
  val rowImage    = new Image(710, 10)

  var divWidth  = 30
  var divHeight = 30

  def realCanvasWidth: Int = ( column * divWidth * (scaleValue / SCALE_VALUE) ).toInt
  def realCanvasHeight: Int = ( row * divHeight * (scaleValue / SCALE_VALUE) ).toInt

  def createInfoLabel(text: String) =
    gg.createLabel(text, text.length * 20, 25, 20, 0x333333)
  
  def updateInfo() {
    List( (columnImage, column), (rowImage, row) ) foreach {
      case (image, value) =>
      editor.images(image) = createInfoLabel(value.toString)
    }
  }
  
  case class Status(id: Int, symbol: Symbol, color: Int, image: PImage) extends NotNull
  val statuses: Seq[Status] = {
    (scala.xml.XML.loadFile("AreaStatuses.xml") \ "status") map {
      xml =>
      val config = new ConfigXML(xml)
      val symbol = config('symbol, 'None)
      val color = config('color, 0xFFFFFFF)
      val image = gg.createAndDrawPImage(divWidth, divHeight) {
        g =>
        val c = gg.rgb(color)
        g.stroke(60)
        g.fill(c._1, c._2, c._3)
        g.rect(0, 0, divWidth - 1, divHeight - 1)
      }
      Status(config('id, 0), symbol, color, image)
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
  
  private var _scaleValue = SCALE_VALUE
  def scaleValue = _scaleValue
  def scaleValue_=(value: Float) {
    _scaleValue = rangeOfNumber(value, MIN_SCALE, MAX_SCALE)
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
    import buttonManager.{ createEasyButton, createButtonByBasicColor }

    val (sx, sy) = (30, 30)
    val (ex, ey) = (120, 120)

    val createStatusButton = createEasyButton(0x444444, 0, _: Int)(60, 60, 18)_
    List( (sx, sy), (ex, sy), (sx, ey), (ex, ey) ).zipWithIndex foreach {
      case((x, y), index) =>
      val status = statuses(index)
      createStatusButton(status.color)(x, y, status.symbol.toString) {
        focus.status = status
        focus.pos = new PVector(x, y)
      }
    }

    val createMenuButton = createButtonByBasicColor(150, 40, 15)

    createMenuButton(30, 210, "Save") { saveBinary(applet.selectOutput()) }
    createMenuButton(30, 260, "Load") { loadBinary(applet.selectInput()) }
    
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
  
  def saveBinary(filename: String) {
    if (filename == null) return
    
    val size = Array[Byte](column.toByte, row.toByte)
    val data: Array[Byte] = buffer.zipWithIndex withFilter {
      case(_, index) => isInsideInBuffer(index)
    } map(_._1.id.toByte)
    
    applet.saveBytes(filename, size ++ data)
  }

  def loadBinary(filename: String) {    
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
    
    val scale = editor.scaleValue / SCALE_VALUE
    val scaleWidth  = editor.divWidth  * scale
    val scaleHeight = editor.divHeight * scale

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
        applet.image(status.image, posX, posY, scaleWidth, scaleHeight)
      }
    }
    
    applet.image(background, 0, 0)
   
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
    
    if (!help.isView) buttonManager.checkMouse()
    buttonManager.draw()

    images.drawAll()
    focus.draw()
  
    if (help.isView) {
      help.checkMouse()
      help.draw()
    }

    if (applet.isMousePressed && applet.mouseButton == PConstants.LEFT) {
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
      util.isMouseInside(CANVAS_LEFT, CANVAS_TOP, CANVAS_WIDTH, CANVAS_HEIGHT)
    ) isCanvasDragged = true

  override def mouseReleased() = isCanvasDragged = false
}
