package com.github.alphaneet.suparobo_engine.game

package object models {
  type FunSuite = org.scalatest.FunSuite
  type ShouldMatchers = org.scalatest.matchers.ShouldMatchers
  
  trait CharactersFixture {
    val REIMU = new Champion {      
      param.update(
        name = "霊夢",
        hitPoint = 100,
        moveRangePoint = 3,
        attackPoint = 100,
        attackRangePoint = 5,
        guardPoint = 20,
        cost = 4
      )
    }
    
    val MARISA = new Champion {
      param.update(
        name = "魔理沙",
        hitPoint = 100,
        moveRangePoint = 3,
        attackPoint = 100,
        attackRangePoint = 5,
        guardPoint = 20,
        cost = 4
      )
    }
    
    val TANK = new Minion {
      param.update(
        name = "タンク",
        hitPoint = 100,
        moveRangePoint = 3,
        attackPoint = 100,
        attackRangePoint = 5,
        guardPoint = 20,
        cost = 3
      )
    }

    val CARRY = new Minion {
      param.update(
        name = "キャリー",
        hitPoint = 100,
        moveRangePoint = 3,
        attackPoint = 100,
        attackRangePoint = 5,
        guardPoint = 20,
        cost = 2
      )
    }

    val FIGHTER = new Minion {
      param.update(
        name = "ファイター",
        hitPoint = 100,
        moveRangePoint = 3,
        attackPoint = 100,
        attackRangePoint = 5,
        guardPoint = 20,
        cost = 1
      )
    }
  }
}
