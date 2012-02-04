package com.github.alphaneet.suparobo

case class DeckSelectScene(
  game: Game = Game()
)(implicit
  applet: SPApplet,
  i18n: I18N
) extends DeckScene(LAYOUTS_PATH + "DeckSelectScene.xml") {
  val title = createLabel(t("DeckSelectScene.title"), 'title, 30)
  
  registerButtons(
    menuBtnMgr,
    List(
      ('entry, t("entry"), entry _),
      ('save,  t("clear"), clear _),      
      ('back,  t("back"),  back _)
    )
  )

  val boardNameLabel = createLabel(
    game.board.map(_.name).getOrElse(""),
    'boardName,
    size = 20
  )
  
  val boardViewer: Option[Sprite] = game.board.map {
    createBoardSprite(
      _,
      layout.rect('viewer),
      (g, rect) => {
        g.stroke(C5R, C5G, C5B)
        g.noFill()
        g.rect(0, 0, rect.width - 1, rect.height - 1)
      }
    )
  }
    
  def back() {
    dialog.confirm(t("BoardSelectScene.back")) {
      BoardSelectScene(game)
    }
  }

  def entry() {
    dialog.confirm(nowDeckName + " " + t("DeckSelectScene.entryMessage")) {
      save {
        val self = Option(Player(nowDeck))
        GameScene(game.copy(self = self))
      }
    }
  }

  override def draw() {
    applet.background(C2)   
    title.draw()
    boardNameLabel.draw()    
    boardViewer foreach { _.draw() }
    super.draw()
  }  
}
