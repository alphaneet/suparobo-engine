package com.github.alphaneet.suparobo

// チャンピオンをセットしてない場合
class NoSuchChampionException(msg: String = "") extends Exception(msg)

// コストオーバーの場合
class OverCostException(msg: String = "") extends Exception(msg)

sealed trait MyXMLLoader {
  import scala.xml.Node
  
  private def text(name: String)(implicit node: Node) = (node \ name).text
  def toStr(name: String)(implicit node: Node): String = text(name)
  def toSym(name: String)(implicit node: Node): Symbol = Symbol(text(name))
  def toInt(name: String)(implicit node: Node): Int = try {
    text(name).toInt
  } catch {
    case _ => 0
  }
}

// TK: 一時的に適当に定義。あとで model.scala と混ぜる。
object CharacterProfile extends MyXMLLoader {
  def loadProfiles(filename: String): List[CharacterProfile] =
    loadProfiles(scala.xml.XML.loadFile(filename))
  
  def loadProfiles(elem: scala.xml.Elem): List[CharacterProfile] = {
    (elem \ "profile") map {
      implicit elem: scala.xml.Node =>
      CharacterProfile(
        toInt("id"),
        toStr("name"),
        toSym("symbol")
      )
    } toList
  }

  def empty() = CharacterProfile(0, "", 'None)
}

case class CharacterProfile(
  id: Int,
  name: String,
  symbol: Symbol
) extends NotNull

object CharacterParameter extends MyXMLLoader {
  def loadParameter(id: Int, elem: scala.xml.Elem): CharacterParameter = {
    (elem \ "parameter") find {
      toInt("id")(_) == id
    } map {
      implicit node: scala.xml.Node  =>
      CharacterParameter(
        toInt("hitPoint"),
        toInt("moveRangePoint"),
        toInt("attackPoint"),
        toInt("attackRangePoint"),
        toInt("guardPoint"),
        toInt("cost")
      )
    } getOrElse {
      this.empty()
    }
  }
  
  def empty() = CharacterParameter(
    hitPoint = 0,
    moveRangePoint = 0,
    attackPoint = 0,
    attackRangePoint = 0,
    guardPoint = 0,
    cost = 0
  )
}

case class CharacterParameter(
  var hitPoint: Int,
  var moveRangePoint: Int,
  var attackPoint: Int,
  var attackRangePoint: Int,
  var guardPoint: Int,
  var cost: Int
) extends NotNull {

  // TK: いらんかも？様子見
  /*
  def update(
    hitPoint: Int,
    moveRangePoint: Int,
    attackPoint: Int,
    attackRangePoint: Int,
    guardPoint: Int,
    cost: Int    
  ): Unit = this update CharacterParameter(
    hitPoint,
    moveRangePoint,
    attackPoint,
    attackRangePoint,
    guardPoint,
    cost
  )

  def update(param: CharacterParameter) {
    this.hitPoint = param.hitPoint
    this.moveRangePoint = param.moveRangePoint
    this.attackPoint = param.attackPoint
    this.attackRangePoint = param.attackRangePoint
    this.guardPoint = param.guardPoint
    this.cost = param.cost
  }
  */
}

object Character {
  def empty(): Character = new Character {
    val profile = CharacterProfile.empty()
    val param = CharacterParameter.empty()
  }
}

abstract class Character extends NotNull {  
  // TK: 重複がないかのチェック処理を入れるべきかも
  // その時はChampion と Minion はID領域は別にするかも検証（多分 XML ファイル分かれてるので別にしたほうがいい
  val profile: CharacterProfile
  val param: CharacterParameter
}

object Champion {
  def loadChampions(
    profilesFilename: String,
    parametersFilename: String
  ): List[Champion] = {
    val parametersXML = scala.xml.XML.loadFile(parametersFilename)
    
    CharacterProfile.loadProfiles(profilesFilename) map {
      _profile =>
      new Champion {
        val profile = _profile
        val param = CharacterParameter.loadParameter(
          profile.id,
          parametersXML
        )
      }
    }    
  }
}

abstract class Champion extends Character

object Minion {
  def loadMinions(
    profilesFilename: String,
    parametersFilename: String
  ): List[Minion] = {
    val parametersXML = scala.xml.XML.loadFile(parametersFilename)
    
    CharacterProfile.loadProfiles(profilesFilename) map {
      _profile =>
      new Minion {
        val profile = _profile
        val param = CharacterParameter.loadParameter(
          profile.id,
          parametersXML
        )
      }
    }    
  }
}
abstract class Minion extends Character

// 今回ではタイプによる特別処理はしないのでこれらは使わない。
// パラメーター調整でタイプを表現する。
// 使わないが定義だけはしておく
// 続編（笑）ではこういう風に実装していこう的なメモ変わり
trait Tank
trait Carry
trait Fighter

// ここらへんマクロ実装されたら回したい
abstract class TankChampion extends Character with Tank
abstract class CarryChampion extends Character with Carry
abstract class FighterChampion extends Character with Fighter

abstract class TankMinion extends Minion with Tank
abstract class CarryMinion extends Minion with Carry
abstract class FighterMinion extends Minion with Fighter

class Deck(val maxCost: Int) extends NotNull with MyXMLLoader {
  var champion: Option[Champion] = None
  val minions = scala.collection.mutable.LinkedHashSet[Minion]()

  def foreach(f: Character => Unit) {
    champion foreach { f(_) }
    minions foreach { f(_) }
  }

  def clear() {
    champion = None
    minions.clear()
  }
  
  def existsChampion(other: Champion): Boolean = {
    champion exists(_ == other)
  }

  def existsMinion(other: Minion): Boolean = {
    minions exists(_ == other)
  }
  
  def entry(): this.type = {
    if (champion.isEmpty) throw new NoSuchChampionException
    if (isCostOver) throw new OverCostException
      
    this
  }

  def isEntry: Boolean = try {
    entry()
    true
  } catch {
    case _ => false
  }
 
  def isCostOver: Boolean = nowCost > maxCost

  def nowCost: Int =
    champion.map(_.param.cost).getOrElse(0) + minions.foldLeft(0)(_ + _.param.cost)

  def saveXML(filename: String): this.type = {
    val xml = <deck>
    <champion>
    <id>{ champion.map(_.profile.id).getOrElse(0) }</id>
    </champion>
    
    <minions>
    {
      minions map {
        minion =>
        <minion>
        <id>{ minion.profile.id }</id>
        </minion>
      }
    }
    </minions>
    
    </deck>

    scala.xml.XML.save(filename, xml, "utf-8")
    this
  }

  def loadXML(
    filename: String,
    champions: List[Champion],
    minions: List[Minion]
  ): this.type = loadXML(scala.xml.XML.loadFile(filename), champions, minions)
    
  def loadXML(
    elem: scala.xml.Elem,        
    champions: List[Champion],
    minions: List[Minion]
  ): this.type = {
    this.clear()

    val championID = toInt("id")((elem \ "champion").head)     
    this.champion = champions.find(_.profile.id == championID)

    (elem \ "minions" \ "minion") foreach {
      node =>
      val minionID = toInt("id")(node)
      
      minions.find(_.profile.id == minionID).foreach {
        this.minions += _
      }
    }
    
    this
  }
}
