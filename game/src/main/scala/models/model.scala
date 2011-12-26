trait Model extends NotNull {
  thismodel =>
    
  implicit def pair2Position(pair: Pair[Int, Int]) = Position(pair._1, pair._2)

  // マップの外に出た場合
  type OutsideAreaException = ArrayIndexOutOfBoundsException

  // 範囲の外に出た場合
  class OutsideRangeException(msg: String = "") extends Exception(msg)
  
  // その座標に既に別の Character がいたり、
  // 移動不可能な地形の座標に Character を配置した場合
  class UsedAreaPositionException(msg: String = "") extends Exception(msg)

  // 既に使用している AreaStatus をもう一度作成しようとした場合
  class UsedAreaStatusException(msg: String = "") extends Exception(msg)
  
  // 存在しない AreaStatus を取得しようとした場合
  class NoSuchAreaStatusException(msg: String = "") extends Exception(msg)

  // 対象のキャラクターが見つからないアクションを起こした場合
  // （例：キャラクターがいない座標に攻撃する）
  class NotFoundCharacterException(msg: String = "") extends Exception(msg)
 
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
  
  def printAreaStatuses() = thismodel.area.printData { _.symbol }

  def createArea[T: ClassManifest](v: T) =
    new Area(thismodel.area.width, thismodel.area.height, v)

  object Util {
    implicit def boolean2Int(b: Boolean) = if (b) 1 else 0
  }
  
  trait Range {    
    def scan(x: Int, y: Int, rangePoint: Int): Area[Boolean]
    def judge(x: Int, y: Int, rangePoint: Int) = true
    def rangeCost(x: Int, y: Int) = 1
  }

  object DiamondRange {
    val CROSS = List( (0, -1), (-1, 0), (0, 1), (1, 0) )    
  }

  trait DiamondRange extends Range {    
    def scan(firstX: Int, firstY: Int, firstRangePoint: Int): Area[Boolean] = {
      // TODO: チェックフラグ用と実際の範囲データを分けて範囲データを戻すようにする。
      val flags = createArea(false)
      val points = createArea(0)
      
      def check(x: Int, y: Int, rangePoint: Int) {
        if (rangePoint < 0) return
        if (flags(x, y) == true && points(x, y) >= rangePoint) return
        if (!judge(x, y, rangePoint)) return
        
        flags(x, y) = true
        points(x, y) = rangePoint
        DiamondRange.CROSS foreach {
          case(addX, addY) =>
          val (nextX, nextY) = (x + addX, y + addY)
            
          try {
            thismodel.area.checkRectangle(nextX, nextY)
            check(nextX, nextY, rangePoint - rangeCost(nextX, nextY))
          } catch {
            case _: OutsideAreaException =>
          }
        }
      }

      check(firstX, firstY, firstRangePoint)
      flags
    }
  }

  object AreaStatus {
    private val values = scala.collection.mutable.Set[AreaStatus]()
    private def find(check: AreaStatus => Boolean, msg: String): AreaStatus = {
      values.find {
        check
      } getOrElse {
        throw new NoSuchAreaStatusException(msg)
      }
    }
    
    def apply(id: Int): AreaStatus = {
      find(_.id == id, "id: " + id)
    }    
    def apply(symbol: Symbol): AreaStatus = {
      find(_.symbol == symbol, "symbol: " + symbol)
    }
  }

  /**
   * @param symbol 制約はないが基本は小文字で二文字
   */
  case class AreaStatus(id: Int, symbol: Symbol, rangeCost: Int) extends NotNull {
    if (
      AreaStatus.values.exists {
        status =>
        status.symbol == symbol || status.id == id
      }    
    ) throw new UsedAreaStatusException("id: " + id + ", symbol: " + symbol)

    AreaStatus.values += this    
    
    val isMove: Boolean = (rangeCost > 0)
  }
  
  case class Position(private var _x: Int, private var _y: Int) extends NotNull {
    thismodel.area.checkRectangle(_x, _y)
    
    def x = _x
    def y = _y

    def x_=(x: Int) { thismodel.area.checkWidth(x);  _x = x }
    def y_=(y: Int) { thismodel.area.checkHeight(y); _y = y }

    def +=(pos: Position) { x += pos.x; y += pos.y }    
    def -=(pos: Position) { x -= pos.x; y -= pos.y }

    def index = x + y * thismodel.area.width
  }

  class Area[T: ClassManifest](val width: Int, val height: Int, defaultValue: T = FLAT)
  extends NotNull 
  {    
    val size = width * height
    val data = Array.fill(size)(defaultValue)

    def apply(pos: Position): T = apply(pos.x, pos.y)    
    def apply(x: Int, y: Int): T = {
      checkRectangle(x, y)
      apply(x + y * width)
    }
    def apply(index: Int): T = data(index)

    def update(pos: Position, value: T) { update(pos.x, pos.y, value) }    
    def update(x: Int, y: Int, value: T) {
      checkRectangle(x, y)
      update(x + y * width, value)
    }
    def update(index: Int, value: T) {
      data(index) = value
    }

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
    var moveRangePoint = 0

    val weapons = scala.collection.mutable.ArrayBuffer[Weapon]()
    val diamondRange = new DiamondRange {
      override def judge(x: Int, y: Int, movePoint: Int): Boolean = 
        thismodel.area(x, y).isMove && !isPositionUsed(x, y)

      override def rangeCost(x: Int, y: Int): Int = thismodel.area(x, y).rangeCost
    }

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
    
    def printMoveRange() = moveRange.printData { Util.boolean2Int(_) }

    def moveRange: Area[Boolean] = diamondRange.scan(pos.x, pos.y, moveRangePoint)

    def move(x: Int, y: Int)(implicit flags: Area[Boolean]) {      
      if (flags(x, y)) this.pos = Position(x, y)
      else throw new OutsideRangeException("(%d, %d)".format(x, y))
    }
    
    def destroy {
      thismodel.characters -= this
      player.characters -= this
    }
  }
  
  class Weapon(val character: Character) {
    character.weapons += this
    
    var attackPoint = 0
    var attackRangePoint = 0

    val diamondRange = new DiamondRange {}
    
    def printAttackRange() = attackRange.printData { Util.boolean2Int(_) }    

    def attackRange: Area[Boolean] = diamondRange.scan(character.pos.x,
                                                       character.pos.y,
                                                       attackRangePoint)
    def attack(x: Int, y: Int)(implicit flags: Area[Boolean]) {
      val msg = "(%d, %d)".format(x, y)
      val pos = Position(x, y)
      if (flags(x, y)) {
        thismodel.characters.find {
          _.pos == pos
        } match {
          case Some(target) => target.hitPoint -= attackPoint
          case None => throw new NotFoundCharacterException(msg)
        }        
      } else throw new OutsideRangeException(msg)
    }
  }

  class Player extends NotNull {
    val id = thismodel.players.size + 1
    thismodel.players += this
    
    val characters = scala.collection.mutable.ArrayBuffer[Character]()
  }
}
