package com.github.alphaneet.suparobo

case class TitleScene(applet: SPApplet) extends Scene(applet) {
  val layout = new LayoutXML(LayoutsPath + "title.xml")
  val buttonManager = new ButtonManager(applet)
  val gg = new GraphicsGenerator(applet)
  object title {
    val rect = layout.rect('title)
    val image = gg.createLabel(
      "スーパー東方大戦",
      rect.width,
      rect.height,
      50,
      0
    )
    
    def draw() {
      applet.image(image, rect.x, rect.y)
    }
  }

  // initialize
  {
    def createButtonImages(text: String, width: Int, height: Int): List[PImage] = {
      def create(color: Int): PImage =
        gg.createLabel(text, width, height, 18, color, 0x000000)
      
      List(0xFFFFFF, 0xAAAAAA, 0x333333).map(create)
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

  def start() { println("未実装") } 
  def network() { println("未実装") }
  def deck() { DeckMakeScene(applet) }
  def replay() { println("未実装") }
  def exit(): Unit = applet.exit()
    
  override def draw() {
    applet.background(255)

    buttonManager.checkMouse()
    buttonManager.draw()

    title.draw()
  }
}
