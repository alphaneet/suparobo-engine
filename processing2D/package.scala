package com.github.alphaneet

package object suparobo extends processing.core.PConstants {
  val DATA_PATH       = "data/"
  val IMAGES_EXT      = ".png"    
  val IMAGES_PATH     = DATA_PATH + "images/"
  val LAYOUTS_PATH    = DATA_PATH + "layouts/"
  val CHARACTERS_PATH = DATA_PATH + "characters/"  
  val DECKS_PATH      = DATA_PATH + "decks/"
  val BOARDS_PATH     = DATA_PATH + "boards/"
  val LOCALES_PATH    = DATA_PATH + "locales/"
  
  val MAX_DECK = 3
  val MAX_COST = 13
  def createDeck() = new Deck(MAX_COST)

  // thanks for http://kuler.adobe.com/#themeID/1692819
  
  val C1  = 0xFFE8E8
  val C1R = 255
  val C1G = 232
  val C1B = 232  
  
  val C2  = 0xE8E3D1
  val C2R = 232
  val C2G = 227
  val C2B = 209  
  
  val C3  = 0xC9C2B1
  val C3R = 201
  val C3G = 194
  val C3B = 177
  
  val C4  = 0xA19C8D
  val C4R = 161
  val C4G = 156
  val C4B = 141
  
  val C5  = 0x706666
  val C5R = 112
  val C5G = 102
  val C5B = 102

  val BoardValueColors = Map(
    Board.FLAT  -> 0x99FF99,
    Board.WOOD  -> 0x009900,
    Board.HILL  -> 0x999933,
    Board.MOUNT -> 0x999999
  )
  
  type Rectangle = java.awt.Rectangle

  type PImage     = processing.core.PImage
  
  import com.github.alphaneet._
  type SPApplet          = scala_processing.SPApplet
  type Scene             = scala_processing.Scene  
  type ButtonManager     = scala_processing.ButtonManager
  type ListManager       = scala_processing.ListManager
  type GraphicsGenerator = scala_processing.GraphicsGenerator  
  type LayoutXML         = scala_processing.LayoutXML
  type MyUtil            = scala_processing.MyUtil
  type I18N              = scala_processing.I18N

  def createI18N(locale: String): I18N =
    new I18N(locale, LOCALES_PATH + locale + ".xml")
  
  def t(name: String)(implicit i18n: I18N): String = i18n.t(name)

  object Sprite {
    def empty()(implicit applet: SPApplet) = Sprite(
      applet.createImage(0, 0, processing.core.PConstants.ARGB),
      new Rectangle(0, 0, 0, 0)
    )
  }
  
  // TK: scala-processing にいつか移行するかも？
  case class Sprite(
    img: PImage,
    rect: Rectangle
  ) extends NotNull {
    def this(
      img: PImage,
      x: Int,
      y: Int,
      width: Int,
      height: Int
    ) = this(img, new Rectangle(x, y, width, height))
             
    def this(
      img: PImage,
      x: Int,
      y: Int
    ) = this(img, new Rectangle(x, y, img.width, img.height))
    
    def draw()(implicit applet: SPApplet) {      
      applet.image(img, rect.x, rect.y, rect.width, rect.height)
    }
  }

  def createLabel(
    text: Any,
    symbol: Symbol,
    size: Int,
    diff: Rectangle = new Rectangle(0, 0, 0, 0),
    color: Int = C5
  )(implicit layout: LayoutXML, gg: GraphicsGenerator): Sprite = {
    val rect = layout.rect(symbol)
    
    new Sprite(
      gg.createLabel(
        text.toString,
        rect.width  + diff.width,
        rect.height + diff.height,
        size,
        color
      ),
      rect.x + diff.x,
      rect.y + diff.y
    )
  }
  
  def createButtonImages(
    text: String,
    width: Int,
    height: Int,
    size: Int = 18
  )( implicit gg: GraphicsGenerator): List[PImage] = {
    List(C2, C1, C3).map {
      gg.createLabel(text, width, height, size, _, C5)
    }
  }

  def createBoardSprite(
    board: Board,
    rect: Rectangle
  )(implicit gg: GraphicsGenerator): Sprite = {
    val w = rect.width.toFloat  / board.width
    val h = rect.height.toFloat / board.height
    val image = gg.createAndDrawPImage(rect.width + 1, rect.height + 1) {
      g =>
      board foreach {
        case (pos, status) =>
        val (red, green, blue) = gg.rgb(BoardValueColors(status))
        g.fill(red, green, blue)
        g.rect(pos.x * w, pos.y * h, w, h)
      }
    }
    Sprite(image, rect)
  }

  def registerButtons(
    buttonManager: ButtonManager,
    params: List[Triple[Symbol, String, () => Unit]],
    size: Int = 18
  )(
    implicit layout: LayoutXML, gg: GraphicsGenerator
  ) {
    params foreach {
      case (symbol, text, action) =>
      layout(symbol) {
        rect =>
          
        val images = createButtonImages(text, rect.width, rect.height, size)
        buttonManager.register(images, rect.x, rect.y).action {
          action()
        }
      }
    }
  }
}
