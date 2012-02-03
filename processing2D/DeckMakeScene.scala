package com.github.alphaneet.suparobo

case class DeckMakeScene(implicit applet: SPApplet, i18n: I18N)
     extends DeckScene(LAYOUTS_PATH + "DeckMakeScene.xml")
{
  val labels = List(
    ('championsLabel, t("champion")),
    ('minionsLabel,   t("minion"))
  ) map {
    case (symbol, text) =>
    createLabel(text, symbol, size = 25)
  }

  registerButtons(
    menuBtnMgr,
    List(
      ('save,  t("save"),  save _),
      ('clear, t("clear"), clear _),
      ('back,  t("back"),  back _)
    )
  )

  def back() {
    dialog.confirm(t("TitleScene.back")) {                         
      TitleScene()
    }
  }  
  
  override def draw() {
    applet.background(C2)
    
    labels foreach { _.draw() }
  
    super.draw()  
  }
}
