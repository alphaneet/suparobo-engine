object AreaStatus extends Enumeration {
  type AreaStatus = Value
  val FLAT, GRASS, FOREST, HILL, MOUNT, WATER = Value
}

trait Model extends NotNull {
  self =>
    
  import scala.collection.mutable.ArrayBuffer
  import AreaStatus._

  implicit def pair2Position(pair: Pair[Int, Int]) = Position(pair._1, pair._2)

  // マップの外に出た場合
  type OutsideAreaException = ArrayIndexOutOfBoundsException

  // 移動範囲の外に出た場合
  class OutsideMoveRangeException(msg: String = "") extends Exception(msg)
  
  // その座標に既に誰かいる場合
  class UsedAreaPositionException(msg: String = "") extends Exception(msg)
  
  val area: Area[AreaStatus]
  val characters = ArrayBuffer[Character]()
  val players = ArrayBuffer[Player]()

  case class Position(private var _x: Int, private var _y: Int) extends NotNull {
    self.area.checkRectangle(_x, _y)
    
    def x = _x
    def y = _y

    def x_=(x: Int) { self.area.checkWidth(x);  _x = x }
    def y_=(y: Int) { self.area.checkHeight(y); _y = y }

    def +=(pos: Position) { x += pos.x; y += pos.y }    
    def -=(pos: Position) { x -= pos.x; y -= pos.y }
  }

  class Area[T: ClassManifest](val width: Int, val height: Int, defaultValue: T = FLAT)
  extends NotNull 
  {    
    val size = width * height
    val data = Array.fill(size)(defaultValue)

    def apply(x: Int, y: Int): T = {
      checkRectangle(x, y)
      data(x + y * width)
    }
    def apply(pos: Position): T = apply(pos.x, pos.y)
    def apply(index: Int): T = data(index)

    def update(x: Int, y: Int, value: T) {
      checkRectangle(x, y)
      data(x + y * width) = value
    }
    def update(pos: Position, value: T) { update(pos.x, pos.y, value) }
    def update(index: Int, value: T) { data(index) = value }

    def checkWidth(x: Int) {
      if (x < 0 || x >= width) throw new OutsideAreaException("x = " + x)
    }
    def checkHeight(y: Int) {
      if (y < 0 || y >= height) throw new OutsideAreaException("y = " + y)
    }
    def checkRectangle(x: Int, y: Int) { checkWidth(x); checkHeight(y) }
    def checkRectangle(pos: Position)  { checkRectangle(pos.x, pos.y)  }
  }

  class Character(val player: Player, private var _pos: Position) extends NotNull {
    def this(player: Player, x: Int, y: Int) = this(player, Position(x, y))
    
    self.characters += this
    player.characters += this
    pos = _pos
    
    var hitPoint = 0
    var movePoint = 0

    def pos = _pos
    def pos_=(pos: Position) {
      if (isPositionUsed(pos)) {
        throw new UsedAreaPositionException("x = %d, y = %d".format(pos.x, pos.y))
      }
      _pos = pos
    }

    def isPositionUsed(x: Int, y: Int): Boolean = isPositionUsed(Position(x, y))
    def isPositionUsed(pos: Position): Boolean = {
      self.characters exists {
        other =>
        other != this && other.pos == pos
      }
    }
    
    /**
     * テスト書く時とかのチェックデータ吐く時とかに使います。
     */
    def printMoveRange() { printMoveRange(moveRange) }
    def printMoveRange(flags: Area[Boolean]) {
      val width = self.area.width
      flags.data.zipWithIndex foreach {
        case(flag, i) =>
        
        if (i != 0 && i % width == 0) println
      
        val ch = if (flag == true) '1' else '0'
        val sep = if (i == flags.data.length - 1) "" else ", "
        print(ch + sep)
      }
      println
    }        
    
    def moveRange: Area[Boolean] = {
      def create[T: ClassManifest](v: T) =
        new Area(self.area.width, self.area.height, v)
      
      val flags = create(false)
      val points = create(0)
      val cross = List((0, -1), (-1, 0), (0, 1), (1, 0))

      def check(x: Int, y: Int, movePoint: Int) {
        if (movePoint < 0) return
        if (flags(x, y) == true && points(x, y) >= movePoint) return
        if (isPositionUsed(x, y)) return

        flags(x, y) = true
        points(x, y) = movePoint
        cross foreach {
          case(addX, addY) =>
          val (nextX, nextY) = (x + addX, y + addY)
            
          try {
            self.area.checkRectangle(nextX, nextY)
            check(nextX, nextY, movePoint - 1)            
          } catch {
            case _: OutsideAreaException => 
          }          
        }
      }
      
      check(pos.x, pos.y, movePoint)
      flags
    }

    def move(x: Int, y: Int)(implicit flags: Area[Boolean]) {      
      if (flags(x, y)) this.pos = Position(x, y)
      else throw new OutsideMoveRangeException("(%d, %d)".format(x, y))
    }
    
    def destroy {
      self.characters -= this
      player.characters -= this
    }
  }

  case class Player(id: Int) extends NotNull {
    val characters = scala.collection.mutable.ArrayBuffer[Character]()
  }
}
