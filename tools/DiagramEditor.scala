package com.github.alphaneet.suparobo_engine.tools

object DiagramEditor extends EditorPApplet {
  def createEditorScene = new DiagramEditorScene(this)
}

class DiagramEditorScene(applet: EditorPApplet) extends EditorScene(applet) {
  editor =>
    
  import com.github.alphaneet.processing.{
    TextManager
  }
  import processing.core.{ PApplet, PConstants }

  val layout = new LayoutXML(applet.config.elem)  
  val buttonManager = new ButtonManagerEx
  val paramManager  = new TextManager(applet)

  val diagramTypes  = scala.collection.mutable.ArrayBuffer[DiagramType]()
  
  abstract sealed case class DiagramType(id: Int, symbol: Symbol) extends NotNull {
    diagramTypes += this
  }
  object CircleType   extends DiagramType(1, 'circle)
  object BoxType      extends DiagramType(2, 'box)
  object StickType    extends DiagramType(3, 'stick)
  object TriangleType extends DiagramType(4, 'triangle)

  object canvas {
    val rect = layout.rect('canvas)
    var diagramType: DiagramType = CircleType
    val renderer = applet.createGraphics(rect.width, rect.height, PConstants.JAVA2D)

    update()
    
    def centerX = rect.width  >> 1
    def centerY = rect.height >> 1

    def update() {
      import paramManager.toInt
      
      drawPGraphics(renderer) {
        g =>
          
        def rotate(angle: Int) {
          g.translate(centerX, centerY)
          g.rotate( PApplet.radians(angle) )
        }
          
        val size  = toInt('size)
        val half  = size >> 1
        val angle = toInt('angle)
        
        g.background(0, 0, 0, 0)
        g.noStroke()
        g.fill(toInt('red), toInt('green), toInt('blue))        
        diagramType match {
          case CircleType =>
            g.ellipse(centerX, centerY, size, size)
          case BoxType =>
            rotate(angle)
            g.rect(-half, -half, size, size)
          case StickType =>
            rotate(angle)
            g.rect(-(half >> 1), -half, half, size)
          case TriangleType =>
            rotate(angle)
            val y = half / 2          
            val x = (math.sqrt(3) * y).toFloat
            g.triangle(0, -half, -x, y, x, y)
        }
      }
    }
    
    def draw() {
      applet.fill(255)
      applet.noStroke()
      applet.rect(rect.x, rect.y, rect.width, rect.height)
      applet.image(renderer, rect.x, rect.y)
    }
  }
  
  // initialize
  {
    val createButton = buttonManager.createEasyButton(0xFFFFFF, 0xAAAAAA, 0x406155)_

    List(
      ('circle,   CircleType),
      ('box,      BoxType),
      ('stick,    StickType),
      ('triangle, TriangleType)
    ) foreach {
      case (symbol, diagramType) =>
      layout(symbol) {
        rect =>          
        createButton(rect.width, rect.height, 18)(rect.x, rect.y, symbol.name) {
          canvas.diagramType = diagramType
          canvas.update()
        }
      }
    }
   
    List(
      ('red,   "赤",     0,  255),
      ('green, "緑",     0,  255),
      ('blue,  "青",     0,  255),
//      ('alpha, "透明",   0,  255),
      ('angle, "角度",   0,  360),
      ('size,  "大きさ", 10, 140)
    ) foreach {
      case (symbol, text, min, max) =>

      val rect = layout.rect(symbol)
      new Image(
        rect.x, rect.y - 25,
        gg.createLabel(
          text,
          rect.width,
          rect.height,
          size = 18,
          frontColor = 0xFFFFFF
        )
      )

      val intField = new paramManager.IntField(symbol, min, max) {
        override def updateValue(): Unit = canvas.update()
        
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
          canvas.update()
        }
      }
    }

    List(
      ('export, export _),
      ('save, save _),
      ('load, load _)
    ) foreach {
      case (symbol, action) =>

      layout(symbol) {
        rect =>
        createButton(rect.width, rect.height, 18)(rect.x, rect.y, symbol.name) {
          action()
        }
      }            
    }
  }

  def export() {
    val filename = applet.selectOutput()
    if (filename == null) return
    canvas.renderer.save(filename)
  }
  
  def save() {
    import paramManager.toInt
    
    val filename = applet.selectOutput()
    if (filename == null) return
    val xml = <diagram>
    <type>{  canvas.diagramType.symbol.name }</type>
    <red>{   toInt('red)   }</red>
    <green>{ toInt('green) }</green>
    <blue>{  toInt('blue)  }</blue>
    <angle>{ toInt('angle) }</angle>
    <size>{  toInt('size)  }</size>
    </diagram>

    scala.xml.XML.save(filename, xml, "utf-8")
  }
  
  def load() {
    val filename = applet.selectInput()
    if (filename == null) return

    val xml = scala.xml.XML.loadFile(filename)
    List('red, 'green, 'blue, 'angle, 'size).foreach {
      symbol =>
      val text = (xml \ symbol.name).text
      paramManager(symbol).foreach(_.setText(text))
    }
    
    val typeName = (xml \ "type").text
    diagramTypes.find(_.symbol.name == typeName).foreach {
      canvas.diagramType = _
    }
    
    canvas.update()
  }

  override def draw() {
    applet.background(38, 49, 56)
    buttonManager.checkMouse()
    buttonManager.draw()

    images.draw()

    canvas.draw()
  }
}
