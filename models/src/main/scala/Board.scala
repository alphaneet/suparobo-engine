package com.github.alphaneet.suparobo

object Board {
  sealed case class Status(id: Int, symbol: Symbol, rangeCost: Int) {
    val isMove: Boolean = (rangeCost > 0)    
  }
  
  object FLAT  extends Status(1, 'ft, 1)
  object WOOD  extends Status(3, 'wd, 2)
  object HILL  extends Status(4, 'hl, 3)
  object MOUNT extends Status(5, 'mt, 0)
  
  val statuses: List[Status] = 
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

class Board(
  val width: Int,
  val height: Int,
  val name: String = "",  
  defaultData: Seq[Board.Status] = Nil
) extends NotNull {
  val size = width * height
  val data: Array[Board.Status] = if (defaultData.size != size) {
    Array.fill(size)(Board.FLAT)    
  } else {
    defaultData.toArray
  }

  def pos(index: Int) = Position(index % width, index / width)(this)
  def index(pos: Position):  Int = index(pos.x, pos.y)
  def index(x: Int, y: Int): Int = x + y * width
  
  def apply(pos: Position):  Board.Status = apply(pos.x, pos.y)
  def apply(x: Int, y: Int): Board.Status = {
    checkRectangle(x, y)
    apply(index(x, y))
  }  
  def apply(index: Int): Board.Status = try {
    data(index)
  } catch {
    case _: ArrayIndexOutOfBoundsException =>
      throw new OutsideBoardException("index: " + index)
  }

  def update(pos: Position, status: Board.Status) { update(pos.x, pos.y, status) }    
  def update(x: Int, y: Int, status: Board.Status) {
    checkRectangle(x, y)
    update(index(x, y), status)
  }  
  def update(index: Int, status: Board.Status): Unit = try {
    data(index) = status
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

  def foreach(f: Pair[Position, Board.Status] => Unit) {
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
