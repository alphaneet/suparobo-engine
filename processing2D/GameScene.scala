package com.github.alphaneet.suparobo

case class GameScene(
  game: Game
)(implicit
  applet: SPApplet,
  i18n: I18N
) extends Scene(applet) with MyUtil {
  scene =>
  
  implicit val gg     = new GraphicsGenerator(applet)

  // TK: 名前変えたほういがいいかも？
  object board {
    board =>
    val fixedWidth  = applet.width
    val fixedHeight = fixedWidth * (game.board.height / game.board.width)

    var chipWidth:  Float = 0.0f
    var chipHeight: Float = 0.0f

    def chipX(i: Int) = x + chipWidth  * i
    def chipY(i: Int) = y + chipHeight * i

    // TK: either(?) を使ったなんちゃらとか合った気ガスるのでぐぐる
    def pos(x: Int, y: Int): Option[Position] = {
      try {
        Option(
          game.board.pos(
            (x - board.x) / chipWidth.toInt,
            (y - board.y) / chipHeight.toInt          
          )
        )
      } catch {
        case _: OutsideBoardException => None
      }
    }
    
    private val sprite: Sprite = createBoardSprite(
      game.board,
      new Rectangle(0, 0, fixedWidth, fixedHeight)
    )
    
    private var _scale = 1.0f
    def scale = _scale
    def scale_=(scale: Float) {
      _scale = rangeOfNumber(scale, 0.375f, 1.0f)
      val halfW = fixedWidth >> 1      
      val prevH = height
        
      width  = (fixedWidth  * _scale).toInt
      height = (fixedHeight * _scale).toInt

      x =  (halfW - _scale * halfW).toInt
      y += (prevH - height) / 2

      chipWidth  = width.toFloat  / game.board.width
      chipHeight = height.toFloat / game.board.height

      // ここに書いてもええんか？（汗）
      // マウスドラッグ中とか動きある時だとまずいことは間違いなく起きる（きり
      scene.characters.foreach(_.sync())
    }

    def x = sprite.x
    private def x_=(x: Int) = sprite.x = x
      
    def y = sprite.y
    def y_=(y: Int) {
      sprite.y = rangeOfNumber(y, -sprite.height + applet.height, 0)
      
      // ここに書いてもええんか？（汗）
      // マウスドラッグ中とか動きある時だとまずいことは間違いなく起きる（きり
      scene.characters.foreach(_.sync())
    }
    
    def width = sprite.width
    private def width_=(width: Int) = sprite.width = width

    def height = sprite.height
    private def height_=(height: Int) = sprite.height = height

    def draw() {
      sprite.draw()
      
      applet.stroke(C5R, C5G, C5B)
      applet.noFill()

      val (sx, ex) = (x, x + width)
      val (sy, ey) = (y, y + height)
        
      (0 to game.board.width) foreach {
        i =>
        val x = chipX(i)
        applet.line(x, sy, x, ey)
      }
      
      (0 to game.board.height) foreach {
        i =>
        val y = chipY(i)
        applet.line(sx, y, ex, y)
      }
    }
  }
    
  val characterImages: Map[Character, PImage] = game.charactersSet map {
    c =>
    c -> applet.loadImage(IMAGES_PATH + c.profile.symbol.name + IMAGES_EXT)
  } toMap

  // TK: 名前を変える
  case class Koma(character: Character) {
    var pos = new PVector()
    val image = characterImages(character)

    def sync() {
      pos.x = board.chipX(character.pos.x)
      pos.y = board.chipY(character.pos.y)
    }
        
    def draw() {
      applet.image(image, pos.x, pos.y, board.chipWidth, board.chipHeight)
    }
  }

  def createKomas(player: Player): scala.collection.mutable.ArrayBuffer[Koma] = {
    val komas = scala.collection.mutable.ArrayBuffer[Koma]()
    player.deck.characters.foreach {
      c =>       
      komas += Koma(c)
    }
    komas
  }  

  val inside  = createKomas(game.inside)
  val outside = createKomas(game.outside)
  def characters = (inside ++ outside).toList
  
  var action: Action = new SetupAction

  abstract class Action(status: Game.Status) extends NotNull {
    if (game.status != status) throw new IllegalGameStatusException(
      "now: " + game.status + " - args: " + status
    )
    
    def draw(): Unit
    def mousePressed(): Unit
  }

  class SetupAction extends Action(Game.SETUP) {
    board.scale = 0

    game.setup()
    characters.foreach(_.sync())
    
    val title = new Sprite(
      gg.createLabel(t("GameScene.setup"), applet.width, 40, 25, C5, C2), 0, 0
    )    

    var catched: Option[Koma] = None    
    var alpha = 0
    var alphaValue = 20
   
    def draw() {
      catched foreach {
        c =>
        c.pos.x = applet.mouseX - (board.chipWidth / 2)
        c.pos.y = applet.mouseY - (board.chipHeight / 2)
      }
      
      applet.background(C2)
      board.draw()

      inside.foreach(_.draw())
      
      alpha += alphaValue
      if (alphaValue < 0 && alpha < -100) alphaValue = 30
      if (alphaValue > 0 && alpha >  160) alphaValue = -5
      
      applet.noStroke()
      applet.fill(200, 200, 30, alpha)

      val halfH = board.height >> 1
      applet.rect(board.x, board.y + halfH, board.width, board.height + halfH)
      
      title.draw()  
    }

    def mousePressed() {
      catched = board.pos(applet.mouseX, applet.mouseY) flatMap {
        pos =>
        game.inside.deck.characters find(_.pos == pos) flatMap {
          c =>
          inside find(_.character == c)
        }        
      }
    }
  }

  override def draw() = action.draw()
  override def mousePressed = action.mousePressed()

  override def mouseWheelMoved() {
    board.scale += applet.mouseWheelRotation.toFloat / 100
  }

  override def mouseDragged() {
    board.y += (applet.mouseY - applet.pmouseY)
  }
}
