package com.github.alphaneet.suparobo

case class DeckMakeScene(implicit applet: SPApplet)
     extends DeckScene(LAYOUTS_PATH + "deck.xml")
{
  val labels = List(
    ('championsLabel, "チャンピオン"),
    ('minionsLabel,   "ミニオン")
  ) map {
    case (symbol, text) =>
    createLabel(text, symbol, size = 25)
  }

  registerButtons(
    menuBtnMgr,
    List(
      ('save, "保存", save _),
      ('clear, "クリア", clear _),
      ('back, "戻る", back _)
    )
  )

  def back() {
    dialog.confirm("タイトル画面に戻りますか？") {                         
      TitleScene()
    }
  }  
  
  override def draw() {
    applet.background(C2)
    
    labels foreach { _.draw() }
  
    super.draw()  
  }
}
