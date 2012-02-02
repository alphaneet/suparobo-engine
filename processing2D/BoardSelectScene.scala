package com.github.alphaneet.suparobo

case class BoardSelectScene(
  game: Game = Game()
)(implicit applet: SPApplet) extends Scene(applet) {  
  implicit val layout = new LayoutXML(LAYOUTS_PATH + "boards.xml")
  implicit val gg = new GraphicsGenerator(applet)

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
  
  val title = createLabel("ステージを選択してください", 'title, 30)

  // initialize
  {
    registerButtons(
      buttons,      
      List(
        ('select, "デッキ選択へ", select _),
        ('back,   "戻る", back _)        
      )
    )

    val viewerRect= layout.rect('viewer)
    
    // TK: とりあえず3固定      
    (1 to 3) foreach {
      index =>

      val board = Board.loadXML(BOARDS_PATH + "stage" + index + ".xml")
      val sprite = createBoardSprite(board, viewerRect)
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
      dialog.message("ステージを選択してください")      
      return
    }

    DeckSelectScene(game.copy(board = selectBoard.map(_.board)))
  }
  
  def back() {
    dialog.confirm("タイトル画面に戻りますか？") {                         
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
