import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class PositionSuite extends FunSuite with ShouldMatchers {
  trait Fixture extends Model {
    val area = new Area(10, 20)
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

  test("AreaChecker を mix-in していた場合は Area の範囲外には出来ない") {
    new Fixture {
      def position(x: Int, y: Int) = new Position(x, y) with AreaChecker
      
      List((0, 1), (1, 0), (11, 1), (1, 21)) foreach {
        pos =>
        evaluating {
          position(pos._1, pos._2)
        } should produce [ArrayIndexOutOfBoundsException]
      }
      
      val pos = position(1, 1)
      evaluating { pos.x  = 11 } should produce [ArrayIndexOutOfBoundsException]
      evaluating { pos.y += 20 } should produce [ArrayIndexOutOfBoundsException]
      evaluating { pos.x -=  1 } should produce [ArrayIndexOutOfBoundsException]
      evaluating { pos.y  =  0 } should produce [ArrayIndexOutOfBoundsException]      
    }
  }
}

class AreaSuite extends FunSuite with ShouldMatchers {
  import AreaStatus._

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
      area(11) should be (HILL)
      area(Position(1, 3)) should be(HILL)
      
      area(Position(3, 5)) = FOREST
      area(Position(3, 5)) should be (FOREST)      
        
      area(area.size) = GRASS
      area(area.size) should be (GRASS)
      area(5, 10) should be (GRASS)
      area(Position(5, 10)) should be (GRASS)
    }  
  }

  test("マップデータのインデックスは1から数える。" +
       "1より小さかったり最大値より大きかった場合は例外を出す")
  {
    new Fixture(5, 5) {
      area(1, 1) should be (FLAT)      
      evaluating { area(0, 0) } should produce [ArrayIndexOutOfBoundsException]      

      area(5, 5) should be (FLAT)
      evaluating { area(6, 6) } should produce [ArrayIndexOutOfBoundsException]

      area(1) should be (FLAT)      
      evaluating { area(0) } should produce [ArrayIndexOutOfBoundsException]

      area(area.size) should be (FLAT)      
      evaluating { area(area.size + 1) } should produce [ArrayIndexOutOfBoundsException]
    }
  }
}

class CharacterSuite extends FunSuite with ShouldMatchers {
  import scala.collection.mutable.ArrayBuffer
  
  trait Fixture extends Model {
    val area = new Area(20, 20)
    val player = new Player(1)

    def createCharacter(x: Int, y: Int) =
      new Character(player, x, y)
  }

  test("Character を生成したら Model.characters で Player.characters に追加され、" +
       "Character#destroy で除かれる。" ) {
    new Fixture {
      self =>

      def check(func: ArrayBuffer[Character] => Unit) =
        List(self.characters, player.characters) foreach func

      check { _.isEmpty should be (true) }         
        
      val c1 = createCharacter(10, 10)
      check {
        buffer =>
        buffer.size should be (1)
        buffer.exists (_ == c1) should be (true)
      }      
      
      val c2 = createCharacter(10, 10)
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

  test("Character#pos が Area の範囲外だった場合は例外を出す") {
    new Fixture {
      val c1 = createCharacter(10, 10)
      c1.pos should be (Position(10, 10))
      evaluating {
        c1.pos = Position(1, 0)
      } should produce [ArrayIndexOutOfBoundsException]

      evaluating {
        c1.pos += Position(-10, 0)
      } should produce [ArrayIndexOutOfBoundsException]
    }
  }
  test("移動させてみる。") {
    new Fixture {
//      val c1 = new Character(player, Position(10, 10))
//      c1 move (5, 6)

    }
    
 //    c1.
  }
  
  test("Character の生成や移動をする時、" +
       "その座標既に別の Character が居たら例外を出す")
  {
    
  }
  
  test("他のプレイヤーの場所には行けない")(pending)
  
  test("攻撃してみる")(pending)
/*  
  test("Position() は Area の width と height の範囲外を設定したら例外を出す") {
    new Fixture {      
      evaluating { Position(0, 1)  } should produce [ArrayIndexOutOfBoundsException]
      evaluating { Position(1, 0)  } should produce [ArrayIndexOutOfBoundsException]
      evaluating { Position(11, 1) } should produce [ArrayIndexOutOfBoundsException]
      evaluating { Position(1, 21) } should produce [ArrayIndexOutOfBoundsException]
    }
  }

  test("x と y を設定する時も範囲チェックは行なわれている") {
    new Fixture {
      val pos = Position(1, 1)      
      evaluating { pos.x = 0  } should produce [ArrayIndexOutOfBoundsException]
      evaluating { pos.y = 0  } should produce [ArrayIndexOutOfBoundsException]
      evaluating { pos.x = 11 } should produce [ArrayIndexOutOfBoundsException]
      evaluating { pos.y = 21 } should produce [ArrayIndexOutOfBoundsException]
    }
  }
*/  
}

/*
class ManagerSuite extends FunSuite with ShouldMatchers {
  class Fixture() {
    val manager = new Manager() {
      val players = Manager.createPlayers(2)
      val area = new Area(10, 10)
      players foreach {
        _.characters ++= (0 until 5) map {
          Function.const(new Character {})
        }
      }
    }
  }
  
  test("aiueo") {
    new Fixture() {
//      manager.players.isInstanceOf[List[Player]] should be (true)
//      true should be (true)
      println(manager.getClass)
    }
  }
}
*/
