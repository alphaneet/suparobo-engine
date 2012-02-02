package com.github.alphaneet.suparobo

abstract class DeckScene(
  layoutFilename: String
)(implicit applet: SPApplet) extends Scene(applet) with MyUtil {
  scene =>    
  
  implicit val layout = new LayoutXML(layoutFilename)
  implicit val gg = new GraphicsGenerator(applet)

  val dialog = new MyDialog
  
  val champions: List[Champion] = Champion.loadChampions(
    CHARACTERS_PATH + "championProfiles.xml",
    CHARACTERS_PATH + "championParameters.xml"
  )

  val minions: List[Minion] = Minion.loadMinions(
    CHARACTERS_PATH + "minionProfiles.xml",
    CHARACTERS_PATH + "minionParameters.xml"
  )
  
  val decks: List[Deck] = List.range(0, MAX_DECK) map { i => createDeck() }
  var nowDeck = decks(0)
     
  val menuBtnMgr  = new ButtonManager(applet)
  val deckBtnMgr  = new ButtonManager(applet)
  val charaBtnMgr = new ButtonManager(applet) {
    override def draw() {
      buttons foreach {
        btn =>
        btn.draw(btn.width, btn.height)
      }
    }
  }

  import scala.collection.mutable.LinkedHashMap
  val characterButtons = LinkedHashMap[Character, charaBtnMgr.Button]()
  val deckButtons      = LinkedHashMap[Deck,      deckBtnMgr.Button]()

  
  // TK: 仮に置く、いつか rails like な i18n にする t('hitPoint) みたいな
  val CHARACTER_TEXTS = Map(
    'name -> "",
    'hitPoint -> "HP",
    'moveRangePoint -> "移動力",
    'attackPoint -> "攻撃力",
    'attackRangePoint -> "攻撃範囲",
    'guardPoint -> "防御力",
    'cost -> "コスト"     
  )
  
  object characterLabels {
    private var nowCharacter = Character.empty()

    case class Value(symbol: Symbol) {      
      val title = createLabel(
        CHARACTER_TEXTS(symbol),
        symbol,
        size  = 18,
        diff  = new Rectangle(0, -25, 0, 0)
      )
      
      var field: Option[Sprite] = None
      def update(text: Any) {
        field = Option(
          createLabel(
            text,
            symbol,
            size  = 18
          )
        )
      }
      
      def draw() {
        title.draw()
        field.foreach(_.draw())
      }
    }

    val name             = Value('name)
    val hitPoint         = Value('hitPoint)
    val moveRangePoint   = Value('moveRangePoint)
    val attackPoint      = Value('attackPoint)
    val attackRangePoint = Value('attackRangePoint)
    val guardPoint       = Value('guardPoint)
    val cost             = Value('cost)

    val images = name ::
    hitPoint ::
    moveRangePoint ::
    attackPoint ::
    attackRangePoint ::
    guardPoint ::
    cost ::
    Nil
    
    def update(chara: Character) {
      if (nowCharacter == chara) { return }
      nowCharacter = chara
      
      val p = chara.param

      name             update chara.profile.name
      hitPoint         update p.hitPoint
      moveRangePoint   update p.moveRangePoint
      attackPoint      update p.attackPoint
      attackRangePoint update p.attackRangePoint
      guardPoint       update p.guardPoint
      cost             update p.cost
    }
    
    def draw(): Unit = images.foreach(_.draw())
  }

  object costLabels {
    def createLabels(): List[Sprite] = {
      val color = if (nowDeck.isEntry) C5 else 0xE84167
      List(
        (nowDeck.nowCost, 'nowCost, new Rectangle()),
        ("／",     'nowCost,   new Rectangle(30, 0, 0, 0)),
        ("コスト", 'costLabel, new Rectangle()),
        (MAX_COST, 'maxCost,   new Rectangle())
      ) map {
        case (text, symbol, diff) =>
        createLabel(text, symbol, 23, diff, color)
      }
    }
    
    var labels  = createLabels()

    def update(): Unit = labels = createLabels()    
    def draw(): Unit = labels foreach { _.draw() }
  }

  
  // initialize
  {    
    decks.zipWithIndex foreach {
      case (deck, index) =>
        
      try {
        val filename = DECKS_PATH + "deck" + index + ".xml"        
        deck.loadXML(filename, champions, minions)
      } catch {
        case ex =>
          Console.err.println(ex)
          deck.clear()
      }
    }
    
    List.range(0, MAX_DECK) foreach {      
      index =>
      val id = index + 1
      val text = "デッキ" + id
      val symbol = Symbol("deck0" + id)
      layout(symbol) {
        rect =>
        val images = createButtonImages(text, rect.width, rect.height)
        val button = deckBtnMgr.register(images, rect.x, rect.y).action {
          nowDeck = decks(index)
          costLabels.update()
        }
        deckButtons += decks(index) -> button
      }
    }
    
    def registerCharacter(character: Character)(action: => Unit) {
      val sym    = character.profile.symbol
      val filename = IMAGES_PATH + sym.name + IMAGES_EXT
      val rect   = layout.rect(sym)
      val button = charaBtnMgr.register(
        List(applet.loadImage(filename)),
        rect.x,
        rect.y
      )
      
      button.fixedWidth  = rect.width
      button.fixedHeight = rect.height
      button.action { action }
      
      characterButtons += character -> button
    }

    champions foreach {
      champion =>
      registerCharacter(champion) {
        nowDeck.champion =
          if (nowDeck.existsChampion(champion)) {
            None
          } else {
            Option(champion)
          }

        costLabels.update()
      }
    }

    minions foreach {
      minion =>
      registerCharacter(minion) {
        if (nowDeck.existsMinion(minion)) {
          nowDeck.minions -= minion
        } else {
          nowDeck.minions += minion
        }

        costLabels.update()
      }
    }
  }

  def nowDeckName: String = "デッキ" + (decks.indexOf(nowDeck) + 1)
  
  def save() {    
    dialog.confirm(nowDeckName + "を保存しますか？") {
      try {
        val filename = DECKS_PATH + "deck" + decks.indexOf(nowDeck) + ".xml"
        nowDeck.entry().saveXML(filename)        
        dialog.message("保存しました")
      } catch {
        case _: NoSuchChampionException =>
          dialog.message("チャンピオンを選択してください")
        case _: OverCostException =>
          dialog.message("コストオーバーです")
        case ex =>
          ex.printStackTrace(Console.err)
          dialog.message(ex.getClass.getSimpleName)
      }
    }
  }
  
  def clear() {
    dialog.confirm(nowDeckName + "を初期状態に戻しますか？") {
      nowDeck.clear()
      costLabels.update()
    }
  }
  
  def updateCharacterLabels() {
    characterButtons foreach {
      case (character, button) =>

      if (charaBtnMgr.isOverMouse(button)) {
        characterLabels update character
      }
    }    
  }
  
  override def draw() {
    menuBtnMgr.draw()  
    deckBtnMgr.draw()
    charaBtnMgr.draw()
    
    characterLabels.draw()
    costLabels.draw()

    {
      val btn = deckButtons(nowDeck)
      applet.noStroke()
      applet.fill(252, 196, 95, 64)      
      applet.rect(btn.x, btn.y, btn.width, btn.height)
    }
             
    nowDeck foreach {
      character =>

      val btn = characterButtons(character)
      applet.stroke(C5R, C5B, C5G)
      applet.strokeWeight(2)
      applet.fill(C5R, C5B, C5G, 64)
      applet.rect(btn.x, btn.y, btn.width, btn.height)
    }
    
    if (dialog.isOpen) {
      dialog.draw()
      dialog.checkMouse()
    } else {
      updateCharacterLabels()
      menuBtnMgr.checkMouse()
      deckBtnMgr.checkMouse()
      charaBtnMgr.checkMouse()
    }    
  }  
}
