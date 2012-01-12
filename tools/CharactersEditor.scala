package com.github.alphaneet.suparobo

object CharactersEditor extends EditorPApplet {
  def createEditorScene = new CharactersEditorScene(this)
}

class CharactersEditorScene(applet: EditorPApplet) extends EditorScene(applet) {
  editor =>

  import java.awt.{ Rectangle }
  import processing.core.{ PImage, PConstants }

  val CONFIG_PATH = DATA_PATH + "CharactersEditorConfig.xml"
  val config = new ConfigXML(CONFIG_PATH)
  val layout = new LayoutXML(DATA_PATH + "CharactersEditorLayout.xml")
  
  val SCROLL_WIDTH = 20
  val IMAGE_EXT = ".png"

  def imageDir = paramManager.toText('imageDir)
  def dataDir  = paramManager.toText('dataDir)
  def dataFilename = paramManager.toText('dataFilename)
  def dataFilePath = {
    val dir = if (dataDir.isEmpty) "." else dataDir
    dir + java.io.File.separator + dataFilename
  }
  def isDataFilePathEmpty = dataDir.isEmpty && dataFilename.isEmpty

  case class Parameter(
    id: Int,
    hitPoint: Int,
    moveRangePoint: Int,
    attackPoint: Int,
    attackRangePoint: Int,
    guardPoint: Int,
    cost: Int
  ) extends NotNull

  case class Profile(
    id: Int,
    name: String,
    symbol: String
  )

  case class Pack(
    var param: Parameter,
    private var _profile: Profile,
    listButton: listManager.Button
  ) extends NotNull {
    def profile = _profile
    def profile_=(profile: Profile) {
      _profile = profile
      listButton.images = createListButtonImages(profile.name)
      canvas.update(profile.symbol)
    }
  }

  val packs = new scala.collection.mutable.ArrayBuffer[Pack]() {
    def findByListButton(listButton: listManager.Button): Option[Pack] =
      find(_.listButton == listButton)
  }

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
      val path = imageDir + java.io.File.separator + filename + IMAGE_EXT
      image = Option(applet.loadImage(path))
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
  
  object dialog {
    val buttonWidth   = 60
    val buttonHieght  = 20
    val buttonCenterX = (applet.width  >> 1) - (buttonWidth  >> 1)
    val buttonCenterY = (applet.height >> 1) - (buttonHieght >> 1)
      
    private abstract sealed class Mode extends ButtonManagerEx {
      private var _label: Option[PImage] = None
      def label = _label
      def label_=(msg: String) {
        val image = gg.createLabel(
          msg,
          applet.width,
          height = 50,
          size = 12,
          frontColor = 0xFFFFFF
        )
        
        _label = Option(image)
      }
            
      val actionButton: Button
      val createButton = createEasyButton(
        releasedFront = 0x473027,
        pressedFront  = 0xFFFFFF,
        back = 0xD9CEAD
      )(
        width  = buttonWidth,
        height = buttonHieght,
        size   = 15
      )_

      override def draw() {
        applet.fill(163, 109, 92)
        applet.stroke(128, 69, 62)

        val halfW = applet.width  >> 1
        val halfH = applet.height >> 1

        val (w, h) = (applet.width - 20, 100)
        val left = halfW - (w >> 1)
        val top  = halfH - (h >> 1)
        
        applet.rect(left, top, w, h)

        super.draw()

        label foreach { applet.image(_, 0, halfH - 50) }
      }
    }

    private object OK extends Mode {
      val actionButton = createButton(buttonCenterX, buttonCenterY + 20, "OK") {
        close()
      }
    }

    private object OKCancel extends Mode {
      val actionButton = createButton(buttonCenterX - 50, buttonCenterY + 20, "OK") {
        close()
      }
      createButton(buttonCenterX + 50, buttonCenterY + 20, "Cancel") { close() }
    }
    
    private var _isOpen = false
    def isOpen = _isOpen
    private def open(m: Mode, msg: String, action: () => Unit) {
      _isOpen = true
      mode = m
      mode.actionButton.action {
        close()
        action()
      }
      mode.label = msg
    }
    private def close() { _isOpen = false }
    
    private var mode: Mode = OK
    
    def confirm(msg: String)(action: => Unit) {
      open(OKCancel, msg, action _)
    }
    def message(msg: String, action: => Unit = {}) {
      open(OK, msg, action _)
    }

    def checkMouse(): Unit = mode.checkMouse()
    def draw(): Unit = mode.draw()
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
      ('guardPoint, "防御力", 0, 9999),
      ('cost, "コスト", 1, 10)
    ) foreach {
      case (symbol, text, min, max) =>
      val rect = layout.rect(symbol)
      
      registerLabel(text, rect, size = 15, diff = new Rectangle(-25, -25, 50, 0))
      
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

    val emptyFunc = () => {}
    List(
      ('name, "名前", update _),
      ('symbol, "シンボル名", update _),
      ('imageDir, "イメージディレクトリ", loadImage _),
      ('dataDir, "データディレクトリ", emptyFunc),
      ('dataFilename, "ファイル名", emptyFunc)
    ) foreach {
      case (symbol, text, f) =>
      layout(symbol) {
        rect =>
        registerLabel(text, rect, size = 15, diff = new Rectangle(0, -25, 0, 0))
        new paramManager.ValidateField(symbol) {
          override def updateValue(): Unit = f()
          setBounds(rect)
          setText(config(symbol, ""))
        }
      }
    }

    List(
      ('parameter, "パラメーター"),
      ('profile, "プロフィール"),
      ('menu, "メニュー")
    ) foreach {
      case (symbol, text) =>
      layout(symbol) {
        registerLabel(text, _, size = 20)
      }
    }

    if (!isDataFilePathEmpty) {
      try { _load() } catch { case _ => }
    }    
  }

  def registerLabel(
    text: String,
    rect: Rectangle,
    size: Int,
    diff: Rectangle = new Rectangle()
  ) {
    new Image(
      rect.x + diff.x,
      rect.y + diff.y,
      gg.createLabel(
        text,
        rect.width  + diff.width,
        rect.height + diff.height,
        size = size,
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

  def create() { create(parameter, profile) }
  def create(param: Parameter, profile: Profile) {
    def action(listButton: listManager.Button) {
      listManager.focus = Option(listButton)
      packs.findByListButton(listButton).foreach {
        pack =>
        editor.parameter = pack.param
        editor.profile   = pack.profile
        canvas.update(pack.profile.symbol)
      }
    }

    val images = createListButtonImages(profile.name)
    val listButton = listManager.register(images).action {
      action(_)
    }

    packs += Pack(param, profile, listButton)
    if (listManager.focus.isEmpty) { action(listButton) }
  }

  def delete() {
    listManager.remove().foreach {
      packs.findByListButton(_).foreach(packs.-=)
    }
  }

  def update() {
    focus foreach {
      pack =>
      pack.param   = parameter
      pack.profile = profile
    }
  }

  def focus: Option[Pack] = {
    listManager.focus.flatMap {
      packs.findByListButton(_)
    }
  }

  def save(): Unit = {    
    dialog.confirm(dataDir + " に保存しますか？") {
      try {
        _save()
      } catch {
        case ex =>
          Console.err.println(ex)
          dialog.message(ex.toString)
      }
    }
  }
  
  private def _save() {    
    val parametersXML = <parameters>
    {
      packs.zipWithIndex map {
        case (pack, index) =>
        val p = pack.param
        val id = index + 1
        <parameter>
        <id>{ id }</id>
        <hitPoint>{ p.hitPoint }</hitPoint>
        <moveRangePoint>{ p.moveRangePoint }</moveRangePoint>
        <attackPoint>{ p.attackPoint }</attackPoint>
        <attackRangePoint>{ p.attackRangePoint }</attackRangePoint>
        <guardPoint>{ p.guardPoint }</guardPoint>
        <cost>{ p.cost }</cost>
        </parameter>
      }
    }
    </parameters>

    val profilesXML = <profiles>
    {
      packs.zipWithIndex map {
        case (pack, index) =>
        val p = pack.profile
        val id = index + 1
        <profile>
        <id>{ id }</id>
        <name>{ p.name }</name>
        <symbol>{ p.symbol }</symbol>
        </profile>
      }
    }
    </profiles>
      
    val saveXML = scala.xml.XML.save(_: String, _: scala.xml.Elem, "utf-8")
    
    saveXML(dataFilePath + "Parameters.xml", parametersXML)
    saveXML(dataFilePath + "Profiles.xml",   profilesXML)
    saveXML(
      CONFIG_PATH,
      <config>
      <dataDir>{ dataDir }</dataDir>
      <dataFilename>{ dataFilename }</dataFilename>
      <imageDir>{ imageDir }</imageDir>
      </config>
    )
  }

  def load(): Unit = {
    dialog.confirm(dataDir + " から読み込みますか？") {
      try {
        _load()
      } catch {
        case ex =>
          Console.err.println(ex)          
          dialog.message(ex.toString)
      }
    }
  }
  
  private def _load() {
    import scala.xml.Node
    
    clear()

    def loadXML(filename: String) = {
      scala.xml.XML.loadFile(dataFilePath + filename)
    }

    def Node2Str(node: Node)(name: String): String = (node \ name).text
    def Node2Int(node: Node)(name: String): Int = try {
      (node \ name).text.toInt
    } catch {
      case _ => 0
    }      
    
    val parameters = (loadXML("Parameters.xml") \ "parameter").map {
      node =>
      val toInt = Node2Int(node)_
      Parameter(
        toInt("id"),
        toInt("hitPoint"),
        toInt("moveRangePoint"),
        toInt("attackPoint"),
        toInt("attackRangePoint"),
        toInt("guardPoint"),
        toInt("cost")        
      )
    }

    val profiles = (loadXML("Profiles.xml") \ "profile").map {
      node =>
      val toInt = Node2Int(node)_
      val toStr = Node2Str(node)_
      Profile(
        toInt("id"),
        toStr("name"),
        toStr("symbol")
      )
    }

    parameters foreach {
      param =>
      profiles.find(_.id == param.id) foreach {
        create(param, _)
      }
    }
  }

  def clear() {
    listManager.clear()
    packs.clear()
  }

  def loadImage() {
    focus foreach {
      pack =>
      canvas.update(pack.profile.symbol)
    }
  }

  def parameter: Parameter = {
    import paramManager.toInt
    Parameter(
      0,
      toInt('hitPoint),
      toInt('moveRangePoint),
      toInt('attackPoint),
      toInt('attackRangePoint),
      toInt('guardPoint),
      toInt('cost)
    )
  }

  def parameter_=(p: Parameter) {
    List(
      ('hitPoint, p.hitPoint),
      ('moveRangePoint, p.moveRangePoint),
      ('attackPoint, p.attackPoint),
      ('attackRangePoint, p.attackRangePoint),
      ('guardPoint, p.guardPoint),
      ('cost, p.cost)
    ) foreach {
      case (symbol, value) =>
      paramManager(symbol).foreach(_.setText(value.toString))
    }      
  }

  def profile: Profile = {
    import paramManager.toText
    Profile(
      0,
      toText('name),
      toText('symbol)
    )
  }

  def profile_=(p: Profile) = {
    List(
      ('name, p.name),
      ('symbol, p.symbol)
    ) foreach {
      case (symbol, value) =>
      paramManager(symbol).foreach(_.setText(value.toString))
    }
  }
  
  override def draw() {
    applet.background(156, 150, 139)
    
    listManager.draw()

    listManager.focus.foreach {
      focus =>
      applet.noStroke()
      applet.fill(255, 255, 0, 120)
      applet.rect(focus.x, focus.y, focus.width, focus.height)
    }

    buttonManager.draw()
    
    images.draw()
    canvas.draw()

    if (dialog.isOpen) {
      dialog.draw()
      dialog.checkMouse()
    } else {
      buttonManager.checkMouse()
      listManager.checkMouse()
    }
  }
  
  override def mouseWheelMoved() {
    if (listManager.mouseContains) {
      listManager.scroll += applet.mouseWheelRotation
    }
  }
}
