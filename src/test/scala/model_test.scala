import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

trait Helper {
  this: Model =>
    
  val player = new Player
  
  def toInt(flags: Area[Boolean]) =
    flags.data map { Util.boolean2Int(_) } toList

  def createCharacter(x: Int, y: Int) = new Character(player, x, y)

  def createCharacters(positions: Pair[Int, Int]*): List[Character] = {
    positions.zipWithIndex map {
      case (pos, index) => new Character(new Player, pos)
    } toList
  }
  
  def setArea(statuses: List[Symbol]) {
    statuses.zipWithIndex.foreach {
      case (status, i) => area(i) = AreaStatus(status)
    }
  }
}

class AreaStatusSuite extends FunSuite with ShouldMatchers {
  trait Fixture extends Model with Helper {
    val area = new Area(10, 10)
    
    // 絶対存在しなさそうな id と symbol
    def uniqId(id: Int): Int = 98765 + id
    val uniqId: Int = uniqId(0)
      
    def uniqSymbol(id: Int): Symbol = Symbol("oppaiPeropero" + id)
    val uniqSymbol: Symbol = uniqSymbol(0)  
  }

  test("AreaStatus は AreaStatus(id, symbol, rangeCost) で作成し、" +
       "AreaStatus(id or symbol) で取得する")
  {
    new Fixture {
      val createStatus = AreaStatus(uniqId, uniqSymbol, 3)
      val findStatusById = AreaStatus(uniqId)
      val findStatusBySymbol = AreaStatus(uniqSymbol)
      
      createStatus should be (findStatusById)
      createStatus should be (findStatusBySymbol)
      
      findStatusById.id should be (uniqId)
      // findStatusById.symbol should be (uniqSymbol) は失敗する謎がある
      (findStatusById.symbol == uniqSymbol) should be (true)
      findStatusById.rangeCost should be (3)
    }
  }
  
  test("同じ id や symbol の AreaStatus は作成出来ない") {
    new Fixture {
      def check(func: => Unit) =
        evaluating(func) should produce [UsedAreaStatusException]

      check {
        AreaStatus(uniqId, uniqSymbol, 3)
        AreaStatus(uniqId, uniqSymbol(1), 3)
      }

      check {
        AreaStatus(uniqId, uniqSymbol, 3)
        AreaStatus(uniqId(1), uniqSymbol, 3)
      }
    }
  }

  test("存在しない id や symbol を取得しようとすると例外が出る") {
    new Fixture {
      def check(func: => Unit) =
        evaluating(func) should produce [NoSuchAreaStatusException]

      check { AreaStatus(uniqId) }      
      check { AreaStatus(uniqSymbol) }
    }
  }

  test("rangeCost が 0 以下だと isMove は falase になる") {
    new Fixture {
      AreaStatus(uniqId,    uniqSymbol,    3).isMove should be (true)
      AreaStatus(uniqId(1), uniqSymbol(1), 0).isMove should be (false)
    }
  }

  test("既に Character がいる座標には " +
       "AreaStatus#isMove が false の地形は設置できない")
  {
    // Area[Boolean] とか Area[Int] の時には無視して、
    // Area[AreaStatus] の時だけ AreaStatus#isMove を調べるのめんどうだから
    // マップの読み込みの順番とか気をつけてこの状況を起きないようにして対処する（きりっ
    pending
    
    new Fixture {
      createCharacter(0, 0)
      evaluating {
        setArea(List('mt))
      } should produce [UsedAreaPositionException]      
    }
  }
}

class PositionSuite extends FunSuite with ShouldMatchers {
  trait Fixture extends Model {
    val area = new Area(10, 20)
  }

  test("Pair[Int, Int] は Position に暗黙の型変換される") {
    new Fixture {
      val pos: Position = (3, 5)
      pos.x should be (3)
      pos.y should be (5)
      
      pos += (5, 6)
      pos should be (Position(8, 11))

      pos -= (7, 3)
      pos should be (Position(1, 8))
    }
  }
 
  test("Position() は Area の width と height の範囲外を設定したら例外を出す") {
    new Fixture {      
      evaluating { Position(-1, 0) } should produce [OutsideAreaException]
      evaluating { Position(0, -1) } should produce [OutsideAreaException]
      evaluating { Position(10, 0) } should produce [OutsideAreaException]
      evaluating { Position(0, 20) } should produce [OutsideAreaException]
    }
  }

  test("x と y を設定する時も範囲チェックは行なわれている") {
    new Fixture {
      val pos = Position(0, 0)

      def check(func: => Unit) {
        evaluating { func } should produce [OutsideAreaException]
      }

      List((-1, 0), (0, -1), (10, 0), (0, 20)) foreach {
        p =>
        check { pos.x = p._1; pos.y = p._2 }
      }

      List((11, 0), (0, 21)) foreach {
        p =>
        check { pos += p }
        check { pos += Position(p._1, p._2) }
      }

      List((1, 0), (0, 1)) foreach {
        p =>
        check { pos -= p }
        check { pos -= Position(p._1, p._2) }
      }        
    }
  }  

  test("Position#+= は x と y の座標を足すことが出来る") {
    new Fixture {
      val pos = Position(1, 2)
      pos += Position(4, 5)
      pos should be (Position(5, 7))
      pos -= (3, 4)
      pos should be (Position(2, 3))
    }
  }

  test("Position#index は thismodel.area を基準としたマップデータの index を取得") {
    new Fixture {
      val pos = Position(2, 4)
      pos.index should be (42)
    }
  }       
}

class AreaSuite extends FunSuite with ShouldMatchers {
  class Fixture(val width: Int, val height: Int) extends Model {
    val area = new Area(width, height)
  }
  
  test("マップを作成したら、縦x横の領域が確保されて全てFLAT(平地)で初期化されている。")
  {
    new Fixture(10, 20) {
      area.width should be (10)
      area.height should be (20)
      area.size should be (10 * 20)
      area.data foreach { _ should be (FLAT) }
    }
  }

  test("apply() と update() は data(マップデータ) を参照する") {
    new Fixture(5, 10) {
      area(1, 3) = HILL
      area(1, 3) should be (HILL)
      area(16) should be (HILL)
      area(Position(1, 3)) should be(HILL)
      
      area(Position(3, 5)) = WOOD
      area(Position(3, 5)) should be (WOOD)      
        
      area(area.size - 1) = MOUNT
      area(area.size - 1) should be (MOUNT)
      area(4, 9) should be (MOUNT)
      area(Position(4, 9)) should be (MOUNT)
    }  
  }

  test("マップデータのインデックスは0から数える。" +
       "0より小さかったり最大値(width - 1, height - 1, size - 1)より" +
       "大きかった場合は例外を出す")
  {
    new Fixture(5, 5) {
      area(0, 0) should be (FLAT)      
      evaluating { area(-1, -1) } should produce [OutsideAreaException]      

      area(4, 4) should be (FLAT)
      evaluating { area(5, 5) } should produce [OutsideAreaException]

      area(0) should be (FLAT)      
      evaluating { area(-1) } should produce [OutsideAreaException]

      area(area.size - 1) should be (FLAT)      
      evaluating { area(area.size) } should produce [OutsideAreaException]
    }
  }
}

class CharacterSuite extends FunSuite with ShouldMatchers {
  import scala.collection.mutable.ArrayBuffer
  
  class Fixture(width: Int, height: Int) extends Model with Helper {
    val area = new Area(width, height)
  }

  test("Character を生成したら Model.characters と Player.characters に追加され、" +
       "Character#destroy で除かれる。" ) {
    new Fixture(20, 20) {
      self =>

      def check(func: ArrayBuffer[Character] => Unit) =
        List(self.characters, player.characters) foreach func

      check { _.isEmpty should be (true) }         
        
      val c1 = createCharacter(10, 10)
      check {
        buffer =>
        buffer.size should be (1)
        buffer.exists(_ == c1) should be (true)
      }      
      
      val c2 = createCharacter(11, 11)
      check {
        buffer =>
        buffer.size should be (2)
        buffer.exists(_ == c2) should be (true)
      }

      c2.destroy
      check {
        buffer =>
        buffer.size should be (1)
        buffer.exists(_ == c2) should be (false)
      }

      c1.destroy
      check { _.isEmpty should be (true) }
    }
  }

  test("Character#moveRange は移動できる範囲のリストを戻す。" +
       "Character#move はその値を参照して移動を行なう。" +     
       "Area の範囲外は移動できない") {
    
    // 検証データが手打なんであんまりチェックでけてない。
    // とりあえず真ん中とよつはし隅っこだけはチェックした。
    new Fixture(5, 5) {
      val c1 = createCharacter(2, 2)
      c1.moveRangePoint = 2
      implicit var flags: Area[Boolean] = c1.moveRange

      List(
        0, 0, 1, 0, 0,
        0, 1, 1, 1, 0,
        1, 1, 1, 1, 1,
        0, 1, 1, 1, 0,
        0, 0, 1, 0, 0 
      ) should be (toInt(flags))

      List(
                        (2, 0),
                (1, 1), (2, 1), (3, 1),
        (0, 2), (1, 2), (2, 2), (3, 2), (4, 2),
                (1, 3), (2, 3), (3, 3),
                        (2, 4)
      ) foreach {
        case(x, y) =>
        c1.move(x, y)
        c1.pos should be (Position(x, y))
      }
        
      List(
        (0, 0), (1, 0),         (3, 0), (4, 0),
        (0, 1),                         (4, 1),

        (0, 3),                         (4, 3),
        (0, 4), (1, 4),         (3, 4), (4, 4)
      ) foreach {
        case(x, y) =>
        evaluating {
          c1.move(x, y)
        } should produce [OutsideRangeException]
      }

      c1.pos = Position(0, 0)
      List(
        1, 1, 1, 0, 0,
        1, 1, 0, 0, 0,
        1, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0        
      ) should be (toInt(c1.moveRange))

      c1.pos = Position(4, 0)
      List(
        0, 0, 1, 1, 1,
        0, 0, 0, 1, 1,
        0, 0, 0, 0, 1,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0
      ) should be (toInt(c1.moveRange))
      
      c1.pos = Position(0, 4)
      List(
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        1, 0, 0, 0, 0,
        1, 1, 0, 0, 0,
        1, 1, 1, 0, 0        
      ) should be (toInt(c1.moveRange))

      c1.pos = Position(4, 4)
      List(
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 1,
        0, 0, 0, 1, 1,
        0, 0, 1, 1, 1
      ) should be (toInt(c1.moveRange))            
    }

    // ちょっと広めのも一度だけテストしてみる。
    new Fixture(7, 7) {
      val c1 = createCharacter(2, 2)
      c1.moveRangePoint = 5
      List(
        1, 1, 1, 1, 1, 1, 0,
        1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 0,
        1, 1, 1, 1, 1, 0, 0,
        0, 1, 1, 1, 0, 0, 0
      ) should be (toInt(c1.moveRange))
    }
  }
  
  test("Character の生成や移動をする時、" +
       "その座標既に別の Character がいたら移動できない。") {
    new Fixture(8, 8) {
      val list = createCharacters((3, 3), (2, 2), (3, 5), (2, 5))
      val c1 :: c2 :: c3 :: c4 :: Nil = list
      List(2, 3, 4, 5).zipWithIndex foreach {
        case (moveRangePoint, index) =>
        list(index).moveRangePoint = moveRangePoint
      }

      // Position(3, 3), moveRangePoint(2)
      List(
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 1, 0, 0, 0, 0,
        0, 0, 0, 1, 1, 0, 0, 0,
        0, 1, 1, 1, 1, 1, 0, 0,
        0, 0, 1, 1, 1, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0
      ) should be (toInt(c1.moveRange))

      // Position(2, 2), moveRangePoint(3)
      List(
        0, 1, 1, 1, 0, 0, 0, 0,
        1, 1, 1, 1, 1, 0, 0, 0,
        1, 1, 1, 1, 1, 1, 0, 0,
        1, 1, 1, 0, 1, 0, 0, 0,
        0, 1, 1, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0
      ) should be (toInt(c2.moveRange))

      // Position(3, 5), moveRangePoint(4)
      List(
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 1, 0, 0, 0,
        0, 1, 1, 0, 1, 1, 0, 0,
        1, 1, 1, 1, 1, 1, 1, 0,
        0, 1, 0, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 0,
        0, 1, 1, 1, 1, 1, 0, 0
      ) should be (toInt(c3.moveRange))

      // Position(2, 5), moveRangePoint(5)
      List(
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0,
        1, 1, 0, 0, 1, 0, 0, 0,
        1, 1, 1, 0, 1, 1, 0, 0,
        1, 1, 1, 1, 1, 1, 1, 0,
        1, 1, 1, 0, 1, 1, 0, 0,
        1, 1, 1, 1, 1, 1, 1, 0,
        1, 1, 1, 1, 1, 1, 0, 0
      ) should be (toInt(c4.moveRange))
    }    
  }

  test("他の Character が存在する座標を指定した場合、例外を出す") {
    new Fixture(5, 5) {
      val c1 :: c2 :: Nil = createCharacters((1, 1), (2, 3))

      evaluating {
        c2.pos = Position(1, 1)
      } should produce [UsedAreaPositionException]
      
      // インスタンス生成時でも例外を出す。
      evaluating {
        new Character(player, 1, 1)
      } should produce [UsedAreaPositionException]
    }
  }

  test("WOOD  の地点は移動力 -2 になる") {    
    new Fixture(5, 5) {
      setArea {
        List(
          'ft, 'ft, 'ft, 'ft, 'ft,
          'ft, 'wd, 'wd, 'wd, 'wd,
          'ft, 'wd, 'wd, 'ft, 'ft,
          'ft, 'wd, 'ft, 'ft, 'ft,
          'ft, 'ft, 'ft, 'ft, 'ft
        )
      }
      val c = createCharacter(2, 2)
      c.moveRangePoint = 3
      List(
        0, 0, 1, 0, 0,
        0, 0, 1, 1, 0,
        1, 1, 1, 1, 1,
        0, 1, 1, 1, 1,
        0, 1, 1, 1, 0        
      ) should be (toInt(c.moveRange))
    }
  }

  test("HILL  の地点は移動力 -3 になる") {
    new Fixture(5, 5) {
      setArea {
        List(
          'ft, 'ft, 'ft, 'hl, 'hl,
          'ft, 'hl, 'hl, 'ft, 'ft,
          'ft, 'ft, 'ft, 'ft, 'ft,
          'ft, 'ft, 'ft, 'ft, 'ft,
          'ft, 'ft, 'ft, 'ft, 'ft
        )
      }
      val c = createCharacter(2, 0)      
      c.moveRangePoint = 4
      List(
        1, 1, 1, 1, 0,
        1, 1, 1, 1, 0,
        1, 0, 1, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0        
      ) should be (toInt(c.moveRange))
    }    
  }

  test("MOUNT の地点は移動出来ない") {
    new Fixture(5, 5) {
      setArea {
        List(
          'ft, 'mt, 'ft, 'mt, 'ft,
          'ft, 'ft, 'mt, 'ft, 'ft,
          'mt, 'ft, 'ft, 'ft, 'mt,
          'ft, 'mt, 'mt, 'mt, 'ft,
          'ft, 'ft, 'ft, 'ft, 'ft
        )
      }
      val c = createCharacter(2, 2)      
      c.moveRangePoint = 100
      List(
        1, 0, 0, 0, 1,
        1, 1, 0, 1, 1,
        0, 1, 1, 1, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0        
      ) should be (toInt(c.moveRange))
    }           
  }
   
  test("AreaStatus#isMove が false の位置には Character を配置できない。") {
    new Fixture(3, 3) {
      setArea {
        List(
          'mt, 'mt, 'mt,
          'mt, 'mt, 'mt,
          'mt, 'mt, 'mt
        )
      }      
      evaluating {        
        createCharacter(1, 1)
      } should produce [UsedAreaPositionException]
    }
  }    
}

class WeaponSuite extends FunSuite with ShouldMatchers {
  class Fixture(width: Int, height: Int) extends Model with Helper {
    val area = new Area(width, height)
    val p1 = new Player
    val p2 = new Player
    val c1 = new Character(p1, 2, 2)
    val c2 = new Character(p2, 3, 2)
  }

  test("Weapon#attackRange に必要な rangeCost は " +
       "AreaStatus#rangeCost に影響されることなく等しく 1 であり、" +
       "障害物で遮られることがない")
  {   
    new Fixture(5, 5) {
      setArea {
        List(
          'mt, 'mt, 'mt, 'mt, 'mt,
          'mt, 'mt, 'mt, 'hl, 'mt,
          'mt, 'mt, 'ft, 'hl, 'mt,
          'mt, 'hl, 'wd, 'wd, 'mt,
          'mt, 'mt, 'mt, 'mt, 'mt
        )
      }

      val w1 = new Weapon(c1)
      w1.attackRangePoint = 3
      List(
        0, 1, 1, 1, 0,
        1, 1, 1, 1, 1,
        1, 1, 1, 1, 1,
        1, 1, 1, 1, 1,
        0, 1, 1, 1, 0
      ) should be (toInt(w1.attackRange))        
    }
  }

  test("attackRange に Character がいれば攻撃でき、" +
       "存在しない座標に攻撃しようとすると例外を出す")
  {
    new Fixture(5, 5) {
      val w1 = new Weapon(c1)
      w1.attackPoint = 4
      w1.attackRangePoint = 3

      c1.hitPoint = 15
      c2.hitPoint = 10
      implicit val flags = w1.attackRange

      w1.attack(2, 2)
      c1.hitPoint should be (11)
      
      w1.attack(3, 2)
      c2.hitPoint should be (6)

      List( (0, 0), (4, 0), (0, 4), (4, 4) ).foreach {
        case (x, y) =>
        evaluating {
          w1.attack(0, 0)
        } should produce [OutsideRangeException]
      }
    }
  }
}
