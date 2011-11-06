trait Model extends NotNull {
  thismodel =>
    
  implicit def pair2Position(pair: Pair[Int, Int]) = Position(pair._1, pair._2)

  // マップの外に出た場合
  type OutsideAreaException = ArrayIndexOutOfBoundsException

  // 移動範囲の外に出た場合
  class OutsideMoveRangeException(msg: String = "") extends Exception(msg)
  
  // その座標に既に誰かいる場合
  class UsedAreaPositionException(msg: String = "") extends Exception(msg)

  // 既に使用している AreaStatus をもう一度作成しようとした場合
  class UsedAreaStatusException(msg: String = "") extends Exception(msg)
  
  // 存在しない AreaStatus を取得しようとした場合
  class NoSuchAreaStatusException(msg: String = "") extends Exception(msg)

  // basic area statuses
  val FLAT    = AreaStatus(1, 'ft, 1)
//  val GRASS   = AreaStatus(2, 'gs, 1)
  val WOOD    = AreaStatus(3, 'wd, 2)
  val HILL    = AreaStatus(4, 'hl, 3)
  val MOUNT   = AreaStatus(5, 'mt, 0)
//  val WATER   = AreaStatus(6, 'wt, 0)
  
  val area: Area[AreaStatus]
  val characters = scala.collection.mutable.ArrayBuffer[Character]()
  val players = scala.collection.mutable.ArrayBuffer[Player]()
  
  def printAreaStatuses = thismodel.area.printData { _.symbol }    

  object AreaStatus {
    private val values = scala.collection.mutable.Set[AreaStatus]()
    private def find(check: AreaStatus => Boolean, e: String): AreaStatus = {
      values.find {
        check
      } getOrElse {
        throw new NoSuchAreaStatusException(e)
      }
    }
    
    def apply(symbol: Symbol): AreaStatus = {
      find(_.symbol == symbol, "symbol: " + symbol)
    }
    def apply(id: Int): AreaStatus = {
      find(_.id == id, "id: " + id)
    }
  }

  /**
   * @param symbol 制約はないが基本は小文字で二文字
   */
  case class AreaStatus(id: Int, symbol: Symbol, movePoint: Int) {
    if (
      AreaStatus.values.exists {
        status =>
        status.symbol == symbol || status.id == id
      }    
    ) throw new UsedAreaStatusException(symbol.toString)

    AreaStatus.values += this    
    
    val isMove: Boolean = (movePoint > 0)
  }
  
  case class Position(private var _x: Int, private var _y: Int) extends NotNull {
    thismodel.area.checkRectangle(_x, _y)
    
    def x = _x
    def y = _y

    def x_=(x: Int) { thismodel.area.checkWidth(x);  _x = x }
    def y_=(y: Int) { thismodel.area.checkHeight(y); _y = y }

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

    /**
     * テスト書く時とかのチェックデータ吐く時に使う
     */    
    def printData(output: T => Any) {
      data.zipWithIndex foreach {
        case(v, i) =>          
        if (i != 0 && i % width == 0) println
        val sep = if (i == data.length - 1) "" else ", "
        print(output(v) + sep)
      }
      println
    }
  }

  class Character(val player: Player, private var _pos: Position) extends NotNull {
    thischaracter =>

    def this(player: Player, x: Int, y: Int) = this(player, Position(x, y))
    
    thismodel.characters += this
    player.characters += this
    pos = _pos
    
    var hitPoint = 0
    var movePoint = 0

    def pos = _pos
    def pos_=(pos: Position) {
      if (isPositionUsed(pos) || !thismodel.area(pos).isMove) {
        throw new UsedAreaPositionException("x = %d, y = %d".format(pos.x, pos.y))
      }
      _pos = pos
    }

    def isPositionUsed(x: Int, y: Int): Boolean = isPositionUsed(Position(x, y))
    def isPositionUsed(pos: Position): Boolean = {
      thismodel.characters exists {
        other =>
        other != this && other.pos == pos
      }
    }
    
    def printMoveRange() = moveRange.printData { if (_) '1' else '0' }
    
    def moveRange: Area[Boolean] = {
      def create[T: ClassManifest](v: T) =
        new Area(thismodel.area.width, thismodel.area.height, v)
      
      val flags = create(false)
      val points = create(0)
      val cross = List((0, -1), (-1, 0), (0, 1), (1, 0))

      def check(x: Int, y: Int, movePoint: Int) {
        if (!thismodel.area(x, y).isMove) return
        if (movePoint < 0) return
        if (flags(x, y) == true && points(x, y) >= movePoint) return
        if (isPositionUsed(x, y)) return

        flags(x, y) = true
        points(x, y) = movePoint
        cross foreach {
          case(addX, addY) =>
          val (nextX, nextY) = (x + addX, y + addY)
            
          try {
            thismodel.area.checkRectangle(nextX, nextY)
            check(nextX, nextY, movePoint - thismodel.area(nextX, nextY).movePoint)
          } catch {
            case _: OutsideAreaException => 
          }          
        }
      }
      
      check(pos.x, pos.y, thischaracter.movePoint)
      flags
    }

    def move(x: Int, y: Int)(implicit flags: Area[Boolean]) {      
      if (flags(x, y)) this.pos = Position(x, y)
      else throw new OutsideMoveRangeException("(%d, %d)".format(x, y))
    }
    
    def destroy {
      thismodel.characters -= this
      player.characters -= this
    }
  }

  case class Player(id: Int) extends NotNull {
    val characters = scala.collection.mutable.ArrayBuffer[Character]()
  }
}
