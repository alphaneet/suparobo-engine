package com.github.alphaneet.suparobo

class BoardSuite extends FunSuite with ShouldMatchers {

  class Fixture(
    val width: Int,
    val height: Int,
    val data: Seq[Board.Status] = Nil    
  ) extends BoardFixture {
    implicit val board = new Board(width, height, defaultData = data)
  }
  
  test("マップを作成したら、縦x横の領域が確保されて全てFLAT(平地)で初期化されている。")
  {
    new Fixture(10, 20) {
      board.width should be (10)
      board.height should be (20)
      board.size should be (10 * 20)
      board.data foreach { _ should be (FLAT) }
    }
  }

  test("defaultData.size と #size が違う場合は FLAT で初期化される") {
    new BoardFixture {
      val data: Array[Board.Status] = Array(
        WOOD,  HILL,
        MOUNT, WOOD,
        HILL,  FLAT
      )

      new Fixture(3, 2, data) {
        board.size        should be (data.size)
        board.data.size   should be (data.size)
        board.data.toList should be (data.toList)
      }

      new Fixture(4, 3, data) {
        board.size        should be (12)
        board.data.size   should be (12)
        board.data.toList should not be (data.toList)
        board.data.toList should be (board.data.map(Function.const(FLAT)).toList)
      }
    }
  }
  
  test("apply() と update() は data(マップデータ) を参照する") {
    new Fixture(5, 10) {
      board(1, 3) = HILL
      board(1, 3) should be (HILL)
      board(16) should be (HILL)
      board(Position(1, 3)) should be(HILL)
      
      board(Position(3, 5)) = WOOD
      board(Position(3, 5)) should be (WOOD)      
        
      board(board.size - 1) = MOUNT
      board(board.size - 1) should be (MOUNT)
      board(4, 9) should be (MOUNT)
      board(Position(4, 9)) should be (MOUNT)
    }
  }
  
  test("マップデータのインデックスは0から数える。" +
       "0より小さかったり最大値(width - 1, height - 1, size - 1)より" +
       "大きかった場合は例外を出す")
  {
    new Fixture(5, 5) {
      board(0, 0) should be (FLAT)      
      evaluating { board(-1, -1)        } should produce [OutsideBoardException]  
      evaluating { board(-1, -1) = FLAT } should produce [OutsideBoardException]  

      board(4, 4) should be (FLAT)
      evaluating { board(5, 5)        } should produce [OutsideBoardException]
      evaluating { board(5, 5) = FLAT } should produce [OutsideBoardException]

      board(0) should be (FLAT)      
      evaluating { board(-1)        } should produce [OutsideBoardException]
      evaluating { board(-1) = FLAT } should produce [OutsideBoardException]

      board(board.size - 1) should be (FLAT)      
      evaluating { board(board.size)        } should produce [OutsideBoardException]
      evaluating { board(board.size) = FLAT } should produce [OutsideBoardException]
    }
  }
  
  // ja: #loadXML は XML から Board 情報を読みこむ
  test("#loadXML reads XML and set") {
    val boardXML = <board>
      <width>3</width>
      <height>2</height>
      <status><id>1</id></status>
      <status><id>3</id></status>
      <status><id>4</id></status>
      <status><id>5</id></status>
      <status><id>4</id></status>
      <status><id>5</id></status>
    </board>

    new BoardFixture {
      val board = Board.loadXML(boardXML)
      board.width  should be (3)
      board.height should be (2)
      board.data   should be (Array(
        FLAT,  WOOD,  HILL,
        MOUNT, HILL,  MOUNT
      ))
    }
  }
}
