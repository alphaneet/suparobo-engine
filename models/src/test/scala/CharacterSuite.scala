package com.github.alphaneet.suparobo

class CharacterSuite extends FunSuite with ShouldMatchers {  
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
