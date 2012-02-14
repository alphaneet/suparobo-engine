package com.github.alphaneet.suparobo

class DeckSuite extends FunSuite with ShouldMatchers {
  test("チャンピオンは必ず一人だけセットする必要がある。") {
    new CharactersFixture {
      val deck = new Deck(4)

      evaluating { deck.entry() } should produce [NoSuchChampionException]
      
      deck.champion = Option(REIMU)
      deck.entry()
    }
  }

  test("maxCost 以上にキャラクターをセットした場合はエントリー出来ない。") {
    new CharactersFixture {
      val deck = new Deck(8)
      deck.nowCost should be (0)
      
      deck.champion = Option(MARISA)
      deck.minions += TANK
      deck.minions += CARRY

      (REIMU.param.cost + TANK.param.cost + CARRY.param.cost) should be (deck.nowCost)

      evaluating { deck.entry() } should produce [OverCostException]

      deck.minions -= CARRY
      deck.minions += FIGHTER

      deck.maxCost should be (deck.nowCost)
      deck.entry()
    }
  }
  

  // ja: #characters は champion と minions を足したリストが返ってくる
  test("#characters should return champion add minions List") {
    new CharactersFixture {
      val deck = new Deck(10)

      deck.minions += TANK
      deck.minions += CARRY

      deck.characters.size should be (2)
      deck.characters.exists(_ == TANK)  should be (true)
      deck.characters.exists(_ == CARRY) should be (true)
      
      deck.champion = Option(REIMU)
      deck.characters.size should be (3)
      deck.characters.exists(_ == REIMU)  should be (true)      
    }
  }

  // ja: #loadXML は XML から champions と minions の ID が一致したものをセットしていく
  test("#loadXML reads XML and sets Congruous ID of champions and minions") {
    new CharactersFixture {
      val champions = List(REIMU, MARISA)
      val minions   = List(TANK, CARRY, FIGHTER)
      val deckXML   = <deck>
      <champion><id>{ MARISA.profile.id }</id></champion>
      <minions>
      <minion><id>{ TANK.profile.id }</id></minion>
      <minion><id>{ FIGHTER.profile.id }</id></minion>
      
      </minions>
      </deck>

      val deck = new Deck(10)
      deck.loadXML(deckXML, champions, minions)
      
      deck.champion should be (Option(MARISA))

      deck.minions.size should be (2)
      deck.minions.exists(_ == TANK) should be (true)
      deck.minions.exists(_ == FIGHTER) should be (true)
    }
  }
}
