package com.github.alphaneet.suparobo

// TK: 一時的に適当に定義。あとで model.scala と混ぜる。
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

  def empty() = CharacterProfile(0, "", 'None)
}

case class CharacterProfile(
  id: Int,
  name: String,
  symbol: Symbol
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
      this.empty()
    }
  }
  
  def empty() = CharacterParameter(
    hitPoint         = 0,
    moveRangePoint   = 0,
    attackPoint      = 0,
    attackRangePoint = 0,
    guardPoint       = 0,
    cost             = 0
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
