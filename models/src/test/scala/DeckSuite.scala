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
}
