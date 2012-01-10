package com.github.alphaneet.suparobo_engine

case class DeckMakeScene(applet: PApplet) extends Scene(applet) {
  val layout = new LayoutXML(LayoutsPath + "deck.xml")
  val buttonManager = new ButtonManager(applet)
  val gg = new GraphicsGenerator(applet)
  val decks = (0 until MAX_DECK).map { Function.const( createDeck ) }
  var focus = decks(0)
  
  val champions = loadCharacterParameters(ParamsPath + "champions.xml")
  val minions = loadCharacterParameters(ParamsPath + "minions.xml")
  val characters = champions ++ minions
  val characterImages = characters map {
    param =>
//    new Image(
//      
//    )
  }
  
  /*
  val championImages = champions map {
    param =>
    new Image(
      applet.loadImage(ImagesPath + champion.name
    )
  }
  */

  class Image(img: PImage, x: Int, y: Int) extends NotNull {
    def draw(): Unit = applet.image(img, x, y)
  }
  
  object labels {    
    val images = List(
      ('championsLabel, "ヒーロー一覧"),
      ('minionsLabel, "キャラクター一覧"),
      ('deckLabel, "デッキ選択")        
    ).map {
      case (symbol, text) =>
      val rect = layout.rect(symbol) 
      new Image(
        gg.createLabel(text, rect.width, rect.height, 18, 0x000000),
        rect.x,
        rect.y
      )
    }

    def draw() {
      images.foreach(_.draw())
    }
  }
  
  
  // initialize
  {
    def createButtonImages(text: String, width: Int, height: Int): List[PImage] = {
      def create(color: Int): PImage =
        gg.createLabel(text, width, height, 18, color, 0x000000)
      
      List(0xFFFFFF, 0xAAAAAA, 0x333333).map(create)
    }

    (1 to MAX_DECK).foreach {      
      index =>
      val text = "デッキ" + index
      val symbol = Symbol("deck0" + index)
      layout(symbol) {
        rect =>
        val images = createButtonImages(text, rect.width, rect.height)
        buttonManager.register(images, rect.x, rect.y).action {
          focus = decks(index)
        }
      }
    }
    
    List(
      ('save, "保存", save _),
      ('clear, "クリア", clear _),
      ('back, "戻る", back _)
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

  def save() {}
  def clear() {}
  def back() {
    TitleScene(applet)
  }
  
  override def draw() {
    applet.background(255)
    buttonManager.checkMouse()
    buttonManager.draw()

    labels.draw()
  }
}
