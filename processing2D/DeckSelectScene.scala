package com.github.alphaneet.suparobo

case class DeckSelectScene(
  gameMaker: GameMaker = GameMaker()
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
    gameMaker.board.map(_.name).getOrElse(""),
    'boardName,
    size = 20
  )
  
  val boardViewer: Option[Sprite] = gameMaker.board.map {
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
      BoardSelectScene(gameMaker)
    }
  }

  def entry() {
    dialog.confirm(nowDeckName + " " + t("DeckSelectScene.entryMessage")) {
      save {
        try {
          val self = Option(Player(nowDeck))
          val game = gameMaker.copy(inside = self).createGame()
          GameScene(game)
        } catch {
          case _: NoSuchInsidePlayerException  =>
            dialog.message(t("NoSuchSelfPlayerException"),  DeckSelectScene(gameMaker))
          case _: NoSuchOutsidePlayerException =>
            dialog.message(t("NoSuchOtherPlayerException"), TitleScene())
          case _: NoSuchBoardException =>
            dialog.message(t("NoSuchBoardException"), BoardSelectScene(gameMaker))
        }
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
