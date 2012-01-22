package com.github.alphaneet.suparobo

// TK: 一時的にここに書く（@ で移す）
class CharacterSuite2 extends FunSuite with ShouldMatchers {
  test("Character.id(Champion and Minion are another) shoulde be uniq") {
    pending
  }
  
  // ja: profiles と parameters の xml は id によって同期が取れている。
  test("profiles and parameters xml should be sync by id") {  
    val profilesXML = <profiles>
    <profile>
      <id>1</id>
      <name>name1</name>
      <symbol>symbol1</symbol>
    </profile>
    
    <profile>
      <id>2</id>
      <name>name2</name>
      <symbol>symbol2</symbol>    
    </profile>
    
    </profiles>

    val parametersXML = <parameters>
    <parameter>
      <id>1</id>
      <hitPoint>100</hitPoint>
      <moveRangePoint>1</moveRangePoint>
      <attackPoint>10</attackPoint>
      <attackRangePoint>1</attackRangePoint>
      <guardPoint>10</guardPoint>
      <cost>1</cost>
    </parameter>
    
    <parameter>
      <id>2</id>
      <hitPoint>200</hitPoint>
      <moveRangePoint>2</moveRangePoint>
      <attackPoint>20</attackPoint>
      <attackRangePoint>2</attackRangePoint>
      <guardPoint>20</guardPoint>
      <cost>2</cost>
    </parameter>
     
    </parameters>

    val profiles: List[CharacterProfile] =
      CharacterProfile.loadProfiles(profilesXML)

    val minions: List[Minion] = profiles map {
      _profile =>

      new Minion {
        val profile = _profile
        val param = CharacterParameter.loadParameter(
          profile.id,
          parametersXML
        )
      }
    }

    def check(profile: CharacterProfile, param: CharacterParameter) {
      minions find(_.profile.id == profile.id) foreach {
        minion =>
        {
          import minion.profile._

          name should be (profile.name)
          (symbol == profile.symbol) should be (true)
        }

        {
          import minion.param._
          
          hitPoint should be (param.hitPoint)
          moveRangePoint should be (param.moveRangePoint)
          attackPoint should be (param.attackPoint)
          attackRangePoint should be (param.attackRangePoint)
          guardPoint should be (param.guardPoint)
          cost should be (param.cost)
        }
      }
    }

    check(
      CharacterProfile(
        id = 1,
        name = "name1",
        symbol = 'symbol1
      ),
      CharacterParameter(
        hitPoint = 100,
        moveRangePoint = 1,
        attackPoint = 10,
        attackRangePoint = 1,
        guardPoint = 10,        
        cost = 1
      )
    )
    
    check(
      CharacterProfile(
        id = 2,
        name = "name2",
        symbol = 'symbol2
      ),
      CharacterParameter(
        hitPoint = 200,
        moveRangePoint = 2,
        attackPoint = 20,
        attackRangePoint = 2,
        guardPoint = 20,        
        cost = 2
      )
    )
  }
}

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
  
  // ja: #equalsChampion は引数の値と #champion が同じものなら true を返す
  test("#equalsChampion should return true, if together.") {
    new CharactersFixture {
      val deck = new Deck(10)
                  
      deck.equalsChampion(REIMU)  should be (false)    
      deck.equalsChampion(MARISA) should be (false)

      deck.champion = Option(MARISA)
      
      deck.equalsChampion(REIMU)  should be (false)
      deck.equalsChampion(MARISA) should be (true)
    }
  }

  // ja: #existsMinion は引数の値が #minions に含まれているなら true を返す
  test("#existsMinion should return true, if #minions contain arg") {
    new CharactersFixture {
      val deck = new Deck(10)
                        
      deck.existsMinion(TANK)  should be (false)    
      deck.existsMinion(CARRY) should be (false)

      deck.minions += TANK
      
      deck.existsMinion(TANK)  should be (true)    
      deck.existsMinion(CARRY) should be (false)
    }
  }


  // ja: #foreach はチャンピオンとミニオン全てを回す
  test("#foreach should loop champion and minions") {
    new CharactersFixture {
      val deck = new Deck(10)

      deck.champion = Option(REIMU)
      deck.minions += TANK
      deck.minions += CARRY

      var hasReimu = false
      var hasTank  = false
      deck foreach {
        c =>
        if (c == REIMU) hasReimu = true
        if (c == TANK)  hasTank  = true        
      }
      hasReimu should be (true)
      hasTank  should be (true)
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
