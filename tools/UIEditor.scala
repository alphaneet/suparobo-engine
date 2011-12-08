package com.github.alphaneet.suparobo_engine.tools
import com.github.alphaneet.processing.{
  ListManager,
  TextManager
}

object UIEditor extends EditorPApplet {
  def createEditorScene = new UIEditorScene(this)
}

class UIEditorScene(applet: EditorPApplet) extends EditorScene(applet) {
  editor =>

  val LEFT_MENU_X   = 15
  val RIGHT_MENU_X  = 665
  val MENU_TOP      = 8
  val MENU_INTERVAL = 50

  import processing.core.PImage
  import processing.core.PConstants
    
  val buttonManager = new ButtonManagerEx
  val listManager   = new ListManager(applet) {
    x = 660
    y = (MENU_TOP + MENU_INTERVAL * 5) + 20
    background(110, 270, 0xEEEEEE)
  }  
  val paramManager  = new TextManager(applet) {
    class TextFieldEx(val symbol: Symbol) extends TextField
    class IntField(symbol: Symbol, min: Int, max: Int) extends TextFieldEx(symbol) {
      private var _value = 0
      def value = _value
      def value_=(v: Int) {
        _value = rangeOfNumber(v, min, max)
        setText(_value.toString)
      }

      def validateValue(default: Int = min) {
        value = try {
          getText.toInt
        } catch {
          case _ => default
        }
      }
      
      import javax.swing.text.{ PlainDocument, AttributeSet }
      setDocument(new PlainDocument() {
        override def insertString(offs: Int, str: String, a: AttributeSet) {
          try {
            str.toInt
            super.insertString(offs, str, a)
          } catch { case _ => return }
        }
      })
      
      import java.awt.event.{ FocusAdapter, FocusEvent, ActionListener, ActionEvent }
      addFocusListener(new FocusAdapter() {
        override def focusLost(e: FocusEvent): Unit = validateValue()
      })
      addActionListener(new ActionListener() {
        def actionPerformed(e: ActionEvent): Unit = validateValue()
      })      
    }

    val fields: List[TextFieldEx] = {
      val createChangeButton = buttonManager.createButtonByBasicColor(20, 20, 15)
      val left  = LEFT_MENU_X
      val right = RIGHT_MENU_X
      val top   = MENU_TOP
      val diffY = MENU_INTERVAL
      
      val intFields = List(
        ('canvasWidth,  800, 200, 2000, "キャンバスの幅",   left,  top),
        ('canvasHeight, 600, 150, 1500, "キャンバスの高さ", left,  top + diffY),
        ('gridWidth,    32,  5,   500,  "グリッドの幅",     left,  top + diffY * 2),
        ('gridHeight,   32,  5,   500,  "グリッドの高さ",   left,  top + diffY * 3),
        ('panelX,       0,   0,   2000, "パネルの x 座標",  right, top + diffY),
        ('panelY,       0,   0,   1500, "パネルの y 座標",  right, top + diffY * 2),
        ('panelWidth,   50,  5,   1000, "パネルの幅",       right, top + diffY * 3),
        ('panelHeight,  50,  5,   1000, "パネルの高さ",     right, top + diffY * 4)
      ) map {
        case (symbol, default, min, max, text, x, y) =>
        registerMenuLabel(x, y, text)
        new IntField(symbol, min, max) {
          setBounds(x + 30, y + 20, 60, 25)
          value = applet.config(symbol, default)

          List(
            (0x25C0, -10,  -10),
            (0x25C1,  10,  -1),
            (0x25B7,  90,   1),
            (0x25B6,  110,  10)
          ) foreach {
            case (arrow, addX, addValue) =>
            createChangeButton(x + addX, y + 23, arrow.toChar) { value += addValue } 
          }
        }
      }

      intFields ::: {
        List(
          ('panelName, "パネルの名前", right, top)
        ) map {
          case (symbol, text, x, y) =>
          registerMenuLabel(x, y, text)
          new TextFieldEx(symbol) {
            setBounds(x - 10 , y + 20, 140, 20)
          }
        }
      }
    }
  }

  // initialize
  {
    import buttonManager.createButtonByBasicColor
    val createListButton = createButtonByBasicColor(60, 20, 13)

    val left = RIGHT_MENU_X
    val top  = MENU_TOP + MENU_INTERVAL * 5
    registerMenuLabel(left, top, "パネルリスト")

    // @delete
    var tmpIndex = 0
    def tmpImages = {
      def r = applet.random(0xFFFFFF).toInt
      tmpIndex += 1
      val text = tmpIndex.toString
      List(
        gg.createLabel(text, listManager.width, 30, 15, r, r),
        gg.createLabel(text, listManager.width, 30, 15, r, r),
        gg.createLabel(text, listManager.width, 30, 15, r, r)
      )
    }
    
    createListButton(660, 560, "new") {
      listManager.register(tmpImages, 0, 0).action {
        listManager.focus = _
      }
    }
    createListButton(730, 560, "delete") { listManager.remove() }

    listManager.scrollWidth = 20
    listManager.scrollBackground = 0xCCCCCC

    def createScrollButton(arrow: Int): List[PImage] = {
      val text   = arrow.toChar.toString
      val size   = listManager.scrollWidth
      val create = gg.createLabel(text, size, size, 15, _: Int, _: Int)
      List(
        create(0xFFFFFF, 0x555555),
        create(0xAAAAAA, 0x555555),
        create(0x333333, 0x555555)
      )
    }
    listManager.scrollUpButton   = createScrollButton(0x25b2)
    listManager.scrollDownButton = createScrollButton(0x25bc)
    listManager.scrollBarButton  = createScrollButton(0x25a0)

    // @delete
    (0 to 40).foreach {
      index =>
      listManager.register(tmpImages, 0, 0).action {
        listManager.focus = _
      }
    }
  }

  def registerMenuLabel(x: Int, y: Int, text: String): Image = {
    new Image(
      x, y,
      gg.createLabel(
        text,
        width  = 130,
        height = 20,
        size   = 13,
        frontColor = 0x333333,
        align  = PConstants.LEFT
      )
    )    
  }

  override def draw() {
    applet.background(200, 200, 255)
    applet.noStroke()
    applet.fill(250)
    applet.rect(150, 50, 500, 500)

    buttonManager.checkMouse()
    buttonManager.draw()
    
    images.draw()
    listManager.checkMouse()
    listManager.draw()
    
    applet.noFill()
    applet.stroke(255, 255, 0)
    applet.strokeWeight(2)
    listManager.focus foreach {
      button =>
      if (!button.status.isDisabled) {
        applet.rect(button.x, button.y, button.width, button.height)
      }
    }
  }

  override def keyPressed() {
    if (listManager.mouseContains) {
      if (applet.keyCode == PConstants.UP  ) listManager.scroll += 1
      if (applet.keyCode == PConstants.DOWN) listManager.scroll -= 1      
    }
  }
  
  override def mouseWheelMoved() {
    if (listManager.mouseContains) {
      listManager.scroll += applet.mouseWheelRotation
    }
  }
}
