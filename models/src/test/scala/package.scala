package com.github.alphaneet

package object suparobo {
  type FunSuite = org.scalatest.FunSuite
  type ShouldMatchers = org.scalatest.matchers.ShouldMatchers
  
  trait CharactersFixture {
    val REIMU = new Champion {      
      val profile = CharacterProfile(
        id = 1,
        name = "霊夢",
        symbol = 'reimu
      )

      val param = CharacterParameter(
        hitPoint = 100,
        moveRangePoint = 3,
        attackPoint = 100,
        attackRangePoint = 5,
        guardPoint = 20,
        cost = 4
      )
    }
    
    val MARISA = new Champion {
      val profile = CharacterProfile(
        id = 2,
        name = "魔理沙",
        symbol = 'marisa
      )

      val param = CharacterParameter(
        hitPoint = 100,
        moveRangePoint = 3,
        attackPoint = 100,
        attackRangePoint = 5,
        guardPoint = 20,
        cost = 4
      )
    }
    
    val TANK = new Minion {
      val profile = CharacterProfile(
        id = 1,
        name = "タンク",
        symbol = 'tank
      )

      val param = CharacterParameter(
        hitPoint = 100,
        moveRangePoint = 3,
        attackPoint = 100,
        attackRangePoint = 5,
        guardPoint = 20,
        cost = 3
      )
    }

    val CARRY = new Minion {
      val profile = CharacterProfile(
        id = 2,
        name = "キャリー",
        symbol = 'carry
      )

      val param = CharacterParameter(
        hitPoint = 100,
        moveRangePoint = 3,
        attackPoint = 100,
        attackRangePoint = 5,
        guardPoint = 20,
        cost = 2
      )
    }

    val FIGHTER = new Minion {
      val profile = CharacterProfile(
        id = 3,
        name = "ファイター",
        symbol = 'fighter
      )

      val param = CharacterParameter(
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
