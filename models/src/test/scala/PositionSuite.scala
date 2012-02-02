package com.github.alphaneet.suparobo

class PositionSuite extends FunSuite with ShouldMatchers {
  trait Fixture {
    implicit val board = new Board(10, 20)
  }
  
  test("Position() は Board の width と height の範囲外を設定したら例外を出す") {
    new Fixture {      
      evaluating { Position(-1, 0) } should produce [OutsideBoardException]
      evaluating { Position(0, -1) } should produce [OutsideBoardException]
      evaluating { Position(10, 0) } should produce [OutsideBoardException]
      evaluating { Position(0, 20) } should produce [OutsideBoardException]
    }
  }
  
  test("Position#+= -= は x と y の座標を足したり引いたり出来る") {
    new Fixture {
      val pos = Position(1, 2)
      pos += Position(4, 5)
      pos should be (Position(5, 7))
      
      pos -= Position(3, 4)
      pos should be (Position(2, 3))
    }
  }
  
  
  test("x と y を設定する時も範囲チェックは行なわれている") {
    new Fixture {
      val pos = Position(0, 0)

      def check(func: => Unit) {
        evaluating { func } should produce [OutsideBoardException]
      }

      List((-1, 0), (0, -1), (10, 0), (0, 20)) foreach {
        p =>
        check { pos.x = p._1; pos.y = p._2 }
      }

      List((11, 0), (0, 21)) foreach {
        p =>  
        check { pos += Position(p._1, p._2) }
      }

      List((1, 0), (0, 1)) foreach {
        p =>
        check { pos -= Position(p._1, p._2) }
      }        
    }
  }
  
  test("Position#index は Board を基準としたマップデータの index を取得") {
    new Fixture {
      val pos = Position(2, 4)
      pos.index should be (42)
    }
  }
}
