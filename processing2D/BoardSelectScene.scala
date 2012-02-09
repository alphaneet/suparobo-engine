package com.github.alphaneet.suparobo

case class BoardSelectScene(
  gameMaker: GameMaker = GameMaker()
)(implicit
  applet: SPApplet,
  i18n: I18N
) extends Scene(applet) {  
  implicit val layout = new LayoutXML(LAYOUTS_PATH + "BoardSelectScene.xml")
  implicit val gg     = new GraphicsGenerator(applet)

  val dialog  = new MyDialog  
  val buttons = new ButtonManager(applet)
  val boards  = new ListManager(applet) {    
    val rect = layout.rect('boards)
    x = rect.x
    y = rect.y
    background(rect.width, rect.height, C5)
  }
  case class BoardPack(board: Board, sprite: Sprite)
  val boardPacks = scala.collection.mutable.Map[boards.Button, BoardPack]() 
  var selectBoard: Option[BoardPack] = None
  
  val title = createLabel(t("BoardSelectScene.title"), 'title, 30)

  // initialize
  {
    registerButtons(
      buttons,      
      List(
        ('select, t("BoardSelectScene.select"), select _),
        ('back,   t("back"), back _)        
      )
    )

    val viewerRect= layout.rect('viewer)

    (new java.io.File(BOARDS_PATH)).list withFilter {
      _.endsWith(".xml")
    } foreach {
      filename =>

      val board = Board.loadXML(BOARDS_PATH + filename)    
      val sprite = createBoardSprite(
        board,
        viewerRect,
        (g, rect) => {
          g.stroke(C5R, C5G, C5B)
          g.noFill()
          g.rect(0, 0, rect.width - 1, rect.height - 1)
        }        
      )
      val buttonImages = List(
        (C2, C5),
        (C1, C4),
        (C3, C4)
      ) map {
        case (f, b) =>
        gg.createLabel(
          board.name,
          boards.width,
          height     = 40,
          size       = 18,
          frontColor = f,
          backColor  = b
        )
      }
      
      val button = boards.register(buttonImages).action {
        button =>
        selectBoard = Option(boardPacks(button))
      }
      
      boardPacks += button -> BoardPack(board, sprite)
    }
  }

  def select() {
    if (selectBoard.isEmpty) {
      dialog.message(t("BoardSelectScene.PleaseSelect"))
      return
    }

    DeckSelectScene(gameMaker.copy(board = selectBoard.map(_.board)))
  }
  
  def back() {
    dialog.confirm(t("TitleScene.back")) {
      TitleScene()
    }    
  }

  override def draw() {
    applet.background(C2)
    title.draw()

    buttons.draw()
    boards.draw()

    selectBoard foreach { _.sprite.draw() }
    
    if (dialog.isOpen) {
      dialog.draw()
      dialog.checkMouse()
    } else {
      buttons.checkMouse()
      boards.checkMouse()
    }
  }
}
