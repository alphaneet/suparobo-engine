import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class AreaStatusSuite extends FunSuite with ShouldMatchers {
  trait Fixture extends Model {
    val area = new Area(10, 10)
    
    // 絶対存在しなさそうなシンボル    
    val uniqSymbol  = 'status123456789
    val uniqSymbol2 = 'status987654321
  }

  test("AreaStatus は AreaStatus(symbol, movePoint) で作成し、" +
       "AreaStatus(symbol) で取得する")
  {
    new Fixture {
      val createStatus = AreaStatus(uniqSymbol, 3)
      val getStatus = AreaStatus(uniqSymbol)

      createStatus should be (getStatus)
      
      // getStatus.symbol should be (uniqSymbol) は失敗する謎がある
      (getStatus.symbol == uniqSymbol) should be (true)
      getStatus.movePoint should be (3)      
    }
  }
  
  test("同じシンボルの AreaStatus は作成出来ない") {
    new Fixture {
      evaluating {
        AreaStatus(uniqSymbol, 3)
        AreaStatus(uniqSymbol, 3)
      } should produce [UsedAreaStatusException]      
    }
  }

  test("存在しないシンボルを取得しようとすると例外が出る") {
    new Fixture {      
      evaluating {
        AreaStatus(uniqSymbol)
      } should produce [NoSuchAreaStatusException]
    }
  }

  test("movePoint が 0 以下だと isMove は falase になる") {
    new Fixture {
      AreaStatus(uniqSymbol,  3).isMove should be (true)
      AreaStatus(uniqSymbol2, 0).isMove should be (false)
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

  test("Position#+= メソッドで x と y の座標を足すことが出来る") {
    new Fixture {
      val pos = Position(1, 2)
      pos += Position(4, 5)
      pos should be (Position(5, 7))
      pos -= (3, 4)
      pos should be (Position(2, 3))
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
  
  class Fixture(width: Int, height: Int) extends Model {
    val area = new Area(width, height)
    val player = new Player(1)
    
    def toInt(flags: Area[Boolean]) =
      flags.data map { if (_) 1 else 0 } toList

    def createCharacter(x: Int, y: Int) =
      new Character(player, x, y)

    def createCharacters(positions: Pair[Int, Int]*): List[Character] = {
      positions.zipWithIndex map {
        case (pos, index) =>
        val (x, y) = pos
        new Character(new Player(index), x, y)
      } toList
    }

    def setArea(statuses: List[Symbol]) {
      statuses.zipWithIndex.foreach {
        case (status, i) =>
        area(i) = AreaStatus(status)
      }
    }    
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
       "Area の範囲外は移動できない。") {
    
    // 検証データが手打なんであんまりチェックでけてない。
    // とりあえず真ん中とよつはし隅っこだけはチェックした。
    new Fixture(5, 5) {
      val c1 = createCharacter(2, 2)
      c1.movePoint = 2
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
        } should produce [OutsideMoveRangeException]
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
      c1.movePoint = 5
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
        case (movePoint, index) =>
        list(index).movePoint = movePoint
      }

      // Position(3, 3), movePoint(2)
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

      // Position(2, 2), movePoint(3)
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

      // Position(3, 5), movePoint(4)
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

      // Position(2, 5), movePoint(5)
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
      c.movePoint = 3
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
      c.movePoint = 4
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
      c.movePoint = 100
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
