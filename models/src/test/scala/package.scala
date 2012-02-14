package com.github.alphaneet

package object suparobo {
  type FunSuite = org.scalatest.FunSuite
  type ShouldMatchers = org.scalatest.matchers.ShouldMatchers

  trait BoardFixture {
    val FLAT   = Board.FLAT
    val WOOD   = Board.WOOD
    val HILL   = Board.HILL
    val MOUNT  = Board.MOUNT    
  }  
  
  trait CharactersFixture {    
    def REIMU:  Champion = _REIMU.clone()
    def MARISA: Champion = _MARISA.clone()

    def TANK: Minion     = _TANK.clone()
    def CARRY: Minion    = _CARRY.clone()
    def FIGHTER: Minion  = _FIGHTER.clone()    

    private val _REIMU = new Champion {      
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
    
    private val _MARISA = new Champion {
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
    
    private val _TANK = new Minion {
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

    private val _CARRY = new Minion {
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

    private val _FIGHTER = new Minion {
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
