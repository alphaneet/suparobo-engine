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
      ('save,  t("save"),  save _),      
      ('back,  t("back"),  back _)
    )
  )

  val boardViewer: Option[Sprite] = game.board.map {
    createBoardSprite(_, layout.rect('viewer))
  }
    
  def back() {
    dialog.confirm(t("BoardSelectScene.back")) {
      BoardSelectScene(game)
    }
  }

  def entry() {
    dialog.confirm(nowDeckName + " " + t("DeckSelectScene.entryMessage")) {
    }
  }

  override def draw() {
    applet.background(C2)
        
    title.draw()
    boardViewer foreach { _.draw() }
    super.draw()
  }  
}
