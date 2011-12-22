package com.github.alphaneet.suparobo_engine.tools

object ParameterEditor extends EditorPApplet {
  def createEditorScene = new ParameterEditorScene(this)
}

class ParameterEditorScene(applet: EditorPApplet) extends EditorScene(applet) {
  editor =>
    
  import processing.core.{ PImage, PConstants }
  import com.github.alphaneet.processing.{
    ListManager,
    TextManager
  }

  case class Parameter(
    name: String,
    hitPoint: Int,
    moveRangePoint: Int,
    attackPoint: Int,
    attackRangePoint: Int
//    imageFilename: String
  )
  
  val layout = new LayoutXML(applet.config.elem)  
  val buttonManager = new ButtonManagerEx
  val paramManager  = new TextManager(applet)
  val listManager   = new ListManager(applet) {
    val rect = layout.rect('list)
    x = rect.x
    y = rect.y
    background(rect.width, rect.height, 0)
  }

//  object canvas {
//    def draw() {
//    }
//  }
  
  // initialize
  {
    val createButton = buttonManager.createEasyButton(0xFFFFFF, 0xAAAAAA, 0x406155)_
    
    List(
      ('create, create _),
      ('delete, delete _),
      ('save, save _),
      ('load, load _),
      ('loadImage, loadImage _)
    ) foreach {
      case (symbol, action) =>
      layout(symbol) {
        rect =>
        createButton(rect.width, rect.height, 18)(rect.x, rect.y, symbol.name) {
          action()
        }
      }
    }

    List(
      ('hitPoint, "HP", 0, 10000),
      ('moveRangePoint, "移動力", 0, 20),
      ('attackPoint, "攻撃力", 0, 10000),
      ('attackRangePoint, "攻撃範囲", 0, 20)       
    ) foreach {
      case (symbol, text, min, max) =>
      val rect = layout.rect(symbol)

      registerMenuLabel(text, rect)
      
      val intField = new paramManager.IntField(symbol, min, max) {
//        override def updateValue(): Unit = canvas.update()
        
        value = min
        setBounds(rect)
      }
      
      List(
        (0x25C0, -40,  -10),
        (0x25C1, -20,  -1),
        (0x25B7,  50,   1),
        (0x25B6,  70,  10)
      ) foreach {
        case (arrow, addX, addValue) =>
        createButton(20, 20, 15)(rect.x + addX, rect.y + 2, arrow.toChar) {
          intField.value += addValue
      //    canvas.update()
        }
      }

      List( ('name, "名前") ) foreach {
        case (symbol, text) =>
        layout(symbol) {
          rect =>
          registerMenuLabel(text, rect)
          new paramManager.ValidateField(symbol) {
            setBounds(rect)
          }
        }
      }
    }
  }

  def registerMenuLabel(text: String, rect: java.awt.Rectangle) {
    new Image(
      rect.x - 25, rect.y - 25,
      gg.createLabel(
        text,
        rect.width + 50,
        rect.height,
        size = 18,
        frontColor = 0xFFFFFF
      )
    )
  }

  def createListButtonImages(text: String): List[PImage] = {
    def createImage(color: Int): PImage = {
      gg.createLabel(text, listManager.width, 30, 15, 0xFFFFFF, color)
    }
    
    List(0x666666, 0x999999, 0x555555).map(createImage)    
  }

  def create() {
    val p = parameter
    listManager.register(createListButtonImages(p.name)).action {
    }
  }

  def delete() {
  }

  def save() {
  }

  def load() {
  }

  def loadImage() {
  }

  def parameter: Parameter = {
    import paramManager.{ toInt, toText }
    Parameter(
      toText('name),
      toInt('hitPoint),
      toInt('moveRangePoint),
      toInt('attackPoint),
      toInt('attackRangePoint)
    )
  }

  def parameter_=(p: Parameter) {
    List(
      ('name, p.name),
      ('hitPoint, p.hitPoint),
      ('moveRangePoint, p.moveRangePoint),
      ('attackPoint, p.attackPoint),
      ('attackRangePoint, p.attackRangePoint)
    ) foreach {
      case (symbol, value) =>
      paramManager(symbol).foreach(_.setText(value.toString))
    }      
  }
  
  override def draw() {
    applet.background(38, 49, 56)
    
    listManager.checkMouse()
    listManager.draw()

    buttonManager.checkMouse()
    buttonManager.draw()

    images.draw()
  }
}
