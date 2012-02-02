package com.github.alphaneet.suparobo

case class DeckSelectScene(
  game: Game = Game()
)(implicit applet: SPApplet) extends DeckScene(LAYOUTS_PATH + "DeckSelectScene.xml") {
  val title = createLabel("デッキを選択してください", 'title, 30)

  registerButtons(
    menuBtnMgr,
    List(
      ('entry, "決定", entry _),
      ('save,  "保存", save _),      
      ('back,  "戻る", back _)
    )
  )

  val boardViewer: Option[Sprite] = game.board.map {
    createBoardSprite(_, layout.rect('viewer))
  }
    
  def back() {
    dialog.confirm("ステージ選択に戻りますか？") {
      BoardSelectScene(game)
    }
  }

  def entry() {
    dialog.confirm(nowDeckName + "でよいですか？") {
    }
  }

  override def draw() {
    applet.background(C2)
        
    title.draw()
    boardViewer foreach { _.draw() }
    super.draw()
  }  
}
