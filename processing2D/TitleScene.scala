package com.github.alphaneet.suparobo

case class TitleScene(implicit applet: SPApplet) extends Scene(applet) {
  val layout = new LayoutXML(LAYOUTS_PATH + "title.xml")
  val buttonManager = new ButtonManager(applet)
  val gg = new GraphicsGenerator(applet)
  object title {
    val rect = layout.rect('title)
    val image = gg.createLabel(
      "スーパー東方大戦",
      rect.width,
      rect.height,
      size = 60,
      frontColor = C5   
    )
    
    def draw() {
      applet.image(image, rect.x, rect.y)
    }
  }

  // initialize
  {
    def createButtonImages(text: String, width: Int, height: Int): List[PImage] = {
      def create(color: Int): PImage =
        gg.createLabel(text, width, height, 18, color, C5)
      
      List(C2, C1, C3).map(create)
    }
    
    List(
      ('start,   "ステージ選択", start _),
      ('network, "ネット対戦",   network _),
      ('deck,    "デッキ作成",   deck _),
      ('replay,  "リプレイ鑑賞", replay _),
      ('exit,    "終了",         exit _)
    ) foreach {
      case (symbol, text, action) =>
      layout(symbol) {
        rect =>
        val images = createButtonImages(text, rect.width, rect.height)
        buttonManager.register(images, rect.x, rect.y).action {
          action()
        }
      }
    }
  }

  def start() { BoardSelectScene() }
  def network() { println("未実装") }
  def deck() { DeckMakeScene() }
  def replay() { println("未実装") }
  def exit(): Unit = applet.exit()
    
  override def draw() {
    applet.background(C2)

    buttonManager.checkMouse()
    buttonManager.draw()

    title.draw()
  }
}
