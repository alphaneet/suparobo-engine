//class AlreadyCharaterException

object AreaStatus extends Enumeration {
  type AreaStatus = Value
  val FLAT, GRASS, FOREST, HILL, MOUNT, WATER = Value
}

trait Model {
  self =>

  import scala.collection.mutable.ArrayBuffer
  
  val area: Area
  val characters = ArrayBuffer[Character]()
  val players = ArrayBuffer[Player]()

  case class Position(protected var _x: Int, protected var _y: Int) {
    def x = _x
    def y = _y

    def x_=(x: Int) { _x = x }
    def y_=(y: Int) { _y = y }

    def +=(pos: Position) { x += pos.x; y += pos.y }
    def +=(pos: Pair[Int, Int]) { x += pos._1; y += pos._2 }
    
    def -=(pos: Position) { x -= pos.x; y -= pos.y }
    def -=(pos: Pair[Int, Int]) { x -= pos._1; y -= pos._2 }    
  }
  trait AreaChecker {
    this: Position =>
      
    self.area.checkRectangle(x, y)
    
    override def x_=(x: Int) { self.area.checkWidth(x);  _x = x }    
    override def y_=(y: Int) { self.area.checkHeight(y); _y = y }
  }

  class Area(val width: Int, val height: Int) {
    import AreaStatus._
    
    val size = width * height
    val data = Array.fill(size)(FLAT)

    def apply(x: Int, y: Int): AreaStatus = {
      checkRectangle(x, y)
      data((x - 1) + (y - 1) * width)
    }
    def apply(pos: Position): AreaStatus = apply(pos.x, pos.y)
    def apply(index: Int) = data(index - 1)

    def update(x: Int, y: Int, status: AreaStatus) {
      checkRectangle(x, y)
      data((x - 1) + (y - 1) * width) = status
    }
    def update(pos: Position, status: AreaStatus) { update(pos.x, pos.y, status) }
    def update(index: Int, status: AreaStatus) {
      data(index - 1) = status
    }

    def checkWidth(x: Int) {
      if (x < 1 || x > width) throw new ArrayIndexOutOfBoundsException("x = " + x)
    }
    def checkHeight(y: Int) {
      if (y < 1 || y > height) throw new ArrayIndexOutOfBoundsException("y = " + y)
    }
    def checkRectangle(x: Int, y: Int) { checkWidth(x); checkHeight(y) }
    def checkRectangle(pos: Position)  { checkRectangle(pos.x, pos.y)  }
  }

  class Character(val player: Player, x: Int, y: Int) {    
    self.characters += this
    player.characters += this
    
    var hp = 0
    var mobility = 0
    
    private val _pos = new Position(x, y) with AreaChecker
    def pos = _pos
    def pos_=(pos: Position) {
      _pos.x = pos.x
      _pos.y = pos.y
    }
        
    def destroy {
      self.characters -= this
      player.characters -= this
    }
  }

  case class Player(id: Int) {
    val characters = scala.collection.mutable.ArrayBuffer[Character]()
  }
}
