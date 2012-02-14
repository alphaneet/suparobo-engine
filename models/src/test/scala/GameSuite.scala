package com.github.alphaneet.suparobo

class GameSuite extends FunSuite with ShouldMatchers {
  // ja: #isComplateSetup は手前半分に
  class Fixture(width: Int = 5, height: Int = 10) extends CharactersFixture {
    val game = Game(
      Player(new Deck(10) {
        champion = Option(REIMU)
        minions += TANK
        minions += CARRY
      }),
      Player(new Deck(10) {
        champion = Option(MARISA)
        minions += CARRY          
        minions += FIGHTER
      }),
      new Board(width, height)
    )    
  }

  test("#profiles") {
    new Fixture {
      game.charactersSet should be (
        List(REIMU, MARISA, TANK, CARRY, FIGHTER).toSet        
      )
    }
  }
  
  test("#setup") {
//    new Fi
  }
  
  test("#isComplateSetup") {
  }
}
