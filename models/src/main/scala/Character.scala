package com.github.alphaneet.suparobo

object CharacterProfile {
  def loadProfiles(filename: String): List[CharacterProfile] =
    loadProfiles(scala.xml.XML.loadFile(filename))
  
  def loadProfiles(elem: scala.xml.Elem): List[CharacterProfile] = {    
    (elem \ "profile") map {
      node =>
      val to = XML2Value(node)
      CharacterProfile(
        to int 'id,
        to str 'name,
        to sym 'symbol
      )
    } toList
  }
}

case class CharacterProfile(
  id: Int        = 0,
  name: String   = "",
  symbol: Symbol = 'None
) extends NotNull

object CharacterParameter {
  def loadParameter(id: Int, elem: scala.xml.Elem): CharacterParameter = {
    (elem \ "parameter") find {
      XML2Value(_).int('id) == id
    } map {
      node =>
      val to = XML2Value(node)
      CharacterParameter(
        to int 'hitPoint,
        to int 'moveRangePoint,
        to int 'attackPoint,
        to int 'attackRangePoint,
        to int 'guardPoint,
        to int 'cost
      )
    } getOrElse {
      CharacterParameter()
    }
  }
}

case class CharacterParameter(
  var hitPoint: Int         = 0,
  var moveRangePoint: Int   = 0,
  var attackPoint: Int      = 0,
  var attackRangePoint: Int = 0,
  var guardPoint: Int       = 0,
  var cost: Int             = 0
) extends NotNull

object Character {
  def apply() = new Character {
    val profile = CharacterProfile()
    val param   = CharacterParameter()
  }
}

@cloneable trait Character extends NotNull {
  // TK: 重複がないかのチェック処理を入れるべきかも
  // その時はChampion と Minion はID領域は別にするかも検証
  // 多分 XML ファイル分かれてるので別にしたほうがいい
  val profile: CharacterProfile
  val param: CharacterParameter
  
  var pos = Position()

  override def hashCode = profile.hashCode
  override def equals(that: Any): Boolean = that match {
    case x: Character => profile == x.profile
    case _ => false
  }
  override def clone(): this.type = super.clone.asInstanceOf[this.type]
  override def toString = "%s %s %s".format(profile, param, pos)
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

trait Champion extends Character

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

trait Minion extends Character

// 今回ではタイプによる特別処理はしないのでこれらは使わない。
// パラメーター調整でタイプを表現する。
// 使わないが定義だけはしておく
// 続編（笑）ではこういう風に実装していこう的なメモ変わり
trait Tank
trait Carry
trait Fighter

trait TankChampion extends Character with Tank
trait CarryChampion extends Character with Carry
trait FighterChampion extends Character with Fighter

trait TankMinion extends Minion with Tank
trait CarryMinion extends Minion with Carry
trait FighterMinion extends Minion with Fighter
