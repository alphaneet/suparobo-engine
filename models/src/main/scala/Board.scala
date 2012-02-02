package com.github.alphaneet.suparobo

case class BoardValue(id: Int, symbol: Symbol, rangeCost: Int) {
  val isMove: Boolean = (rangeCost > 0)
}

object Board {
  val FLAT  = BoardValue(1, 'ft, 1)
  val WOOD  = BoardValue(3, 'wd, 2)
  val HILL  = BoardValue(4, 'hl, 3)
  val MOUNT = BoardValue(5, 'mt, 0)

  val statuses: List[BoardValue] = 
    FLAT  ::
    WOOD  ::
    HILL  ::
    MOUNT ::
  Nil
  
  def loadXML(filename: String): Board = loadXML(scala.xml.XML.loadFile(filename))
  def loadXML(elem: scala.xml.Elem): Board = {    
    val to = XML2Value(elem)
    new Board(
      to int 'width,
      to int 'height,
      to str 'name,      
      (elem \ "status") map {
        node =>
        val id = XML2Value(node) int 'id
        statuses.find(_.id == id).getOrElse(FLAT)
      }
    )
  }  
}

case class Board(
  val width: Int,
  val height: Int,
  val name: String = "",  
  defaultData: Seq[BoardValue] = Nil
) extends NotNull {
  val size = width * height
  val data: Array[BoardValue] = if (defaultData.isEmpty || defaultData.size != size) {
    Array.fill(size)(Board.FLAT)    
  } else {
    defaultData.toArray
  }

  def pos(index: Int) = Position(index % width, index / width)(this)
  def index(pos: Position):  Int = index(pos.x, pos.y)
  def index(x: Int, y: Int): Int = x + y * width
  
  def apply(pos: Position):  BoardValue = apply(pos.x, pos.y)
  def apply(x: Int, y: Int): BoardValue = {
    checkRectangle(x, y)
    apply(index(x, y))
  }  
  def apply(index: Int): BoardValue = try {
    data(index)
  } catch {
    case _: ArrayIndexOutOfBoundsException =>
      throw new OutsideBoardException("index: " + index)
  }

  def update(pos: Position, value: BoardValue) { update(pos.x, pos.y, value) }    
  def update(x: Int, y: Int, value: BoardValue) {
    checkRectangle(x, y)
    update(index(x, y), value)
  }  
  def update(index: Int, value: BoardValue): Unit = try {
    data(index) = value
  } catch {
    case _: ArrayIndexOutOfBoundsException =>
      throw new OutsideBoardException("index: " + index)
  }
  
  def checkWidth(x: Int) {
    if (x < 0 || x >= width) throw new OutsideBoardException("x = " + x)
  }
  def checkHeight(y: Int) {
    if (y < 0 || y >= height) throw new OutsideBoardException("y = " + y)
  }
  def checkRectangle(x: Int, y: Int) { checkWidth(x); checkHeight(y) }
  def checkRectangle(pos: Position)  { checkRectangle(pos.x, pos.y)  }

  def foreach(f: Pair[Position, BoardValue] => Unit) {
    data.iterator.zipWithIndex foreach {
      case (status, index) =>
      f(pos(index), status)
    }
  }
  /*
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
  */    
}
