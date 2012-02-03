package com.github.alphaneet.suparobo

case class TitleScene(implicit applet: SPApplet, i18n: I18N) extends Scene(applet) {
  implicit val layout = new LayoutXML(LAYOUTS_PATH + "TitleScene.xml")
  implicit val gg     = new GraphicsGenerator(applet)  
  val buttonManager   = new ButtonManager(applet)
  
  val title = createLabel( t("title"), 'title, 60 )

  registerButtons(
    buttonManager,
    List(
      ('start,   t("TitleScene.start"),   start _),
      ('network, t("TitleScene.network"), network _),
      ('deck,    t("TitleScene.deck"),    deck _),
      ('replay,  t("TitleScene.replay"),  replay _),
      ('exit,    t("TitleScene.exit"),    exit _)
    )    
  )
  
  registerButtons(
    buttonManager,
    List(
      ('ja, t("ja"), ja _),
      ('en, t("en"), en _)
    ),
    size = 12
  )

  def start() { BoardSelectScene() }
  def network() { println("未実装") }
  def deck() { DeckMakeScene() }
  def replay() { println("未実装") }
  def exit() = applet.exit()

  private def changeLocale(locale: String) {
    val i18n = createI18N(locale)
    applet.title = i18n.t("title")
    TitleScene()(applet, i18n)
  }
  def ja() = changeLocale("ja")
  def en() = changeLocale("en")
    
  override def draw() {
    applet.background(C2)

    buttonManager.checkMouse()
    buttonManager.draw()

    title.draw()
  }
}
