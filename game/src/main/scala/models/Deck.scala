package com.github.alphaneet.suparobo_engine.game.models

// チャンピオンをセットしてない場合
class NoSuchChampionException(msg: String = "") extends Exception(msg)

// コストオーバーの場合
class OverCostException(msg: String = "") extends Exception(msg)

// TK: 一時的に適当に定義。あとで model.scala と混ぜる。
object CharacterParameter {
  def empty() = CharacterParameter(
    name = "",
    hitPoint = 0,
    moveRangePoint = 0,
    attackPoint = 0,
    attackRangePoint = 0,
    guardPoint = 0,
    cost = 0
  )
}

case class CharacterParameter(
  var name: String,
  var hitPoint: Int,
  var moveRangePoint: Int,
  var attackPoint: Int,
  var attackRangePoint: Int,
  var guardPoint: Int,
  var cost: Int
) extends NotNull {
  
  def update(
    name: String,
    hitPoint: Int,
    moveRangePoint: Int,
    attackPoint: Int,
    attackRangePoint: Int,
    guardPoint: Int,
    cost: Int    
  ): Unit = this update CharacterParameter(
    name,
    hitPoint,
    moveRangePoint,
    attackPoint,
    attackRangePoint,
    guardPoint,
    cost
  )

  def update(param: CharacterParameter) {
    this.name = param.name
    this.hitPoint = param.hitPoint
    this.moveRangePoint = param.moveRangePoint
    this.attackPoint = param.attackPoint
    this.attackRangePoint = param.attackRangePoint
    this.guardPoint = param.guardPoint
    this.cost = param.cost
  }
}

abstract class Character {
  val param = CharacterParameter.empty()
}

class Champion extends Character
class Minion extends Character

// 今回ではタイプによる特別処理はしないのでこれらは使わない。
// パラメーター調整でタイプを表現する。
// 使わないが定義だけはしておく
// 続編（笑）ではこういう風に実装していこう的なメモ変わり
trait Tank
trait Carry
trait Fighter

// ここらへんマクロ実装されたら回したい
class TankChampion extends Character with Tank
class CarryChampion extends Character with Carry
class FighterChampion extends Character with Fighter

class TankMinion extends Minion with Tank
class CarryMinion extends Minion with Carry
class FighterMinion extends Minion with Fighter

class Deck(val maxCost: Int) extends NotNull {
  var champion: Option[Champion] = None
  val minions = scala.collection.mutable.LinkedHashSet[Minion]()

  def entry(): Deck = {
    if (champion.isEmpty) throw new NoSuchChampionException
    if (this.nowCost > maxCost) throw new OverCostException
      
    this
  }

  def nowCost: Int =
    champion.map(_.param.cost).getOrElse(0) + minions.foldLeft(0)(_ + _.param.cost)
}
