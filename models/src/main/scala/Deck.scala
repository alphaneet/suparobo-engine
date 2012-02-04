package com.github.alphaneet.suparobo

class Deck(val maxCost: Int) extends NotNull {
  var champion: Option[Champion] = None
  val minions = scala.collection.mutable.LinkedHashSet[Minion]()

  def foreach(f: Character => Unit) {
    champion foreach { f(_) }
    minions  foreach { f(_) }
  }

  def clear() {
    champion = None
    minions.clear()
  }
  
  def existsChampion(other: Champion): Boolean = {
    champion exists(_ == other)
  }

  def existsMinion(other: Minion): Boolean = {
    minions exists(_ == other)
  }
  
  def entry(): this.type = {
    if (champion.isEmpty) throw new NoSuchChampionException
    if (isCostOver) throw new OverCostException
      
    this
  }

  def isEntry: Boolean = try {
    entry()
    true
  } catch {
    case _ => false
  }

  def isMaxCost:  Boolean = nowCost == maxCost
  def isCostOver: Boolean = nowCost >  maxCost

  def nowCost: Int =
    champion.map(_.param.cost).getOrElse(0) + minions.foldLeft(0)(_ + _.param.cost)

  def saveXML(filename: String): this.type = {
    val xml = <deck>
    <champion>
    <id>{ champion.map(_.profile.id).getOrElse(0) }</id>
    </champion>
    
    <minions>
    {
      minions map {
        minion =>
        <minion>
        <id>{ minion.profile.id }</id>
        </minion>
      }
    }
    </minions>
    
    </deck>

    scala.xml.XML.save(filename, xml, "utf-8")
    this
  }

  def loadXML(
    filename: String,
    champions: List[Champion],
    minions: List[Minion]
  ): this.type = loadXML(scala.xml.XML.loadFile(filename), champions, minions)
    
  def loadXML(
    elem: scala.xml.Elem,        
    champions: List[Champion],
    minions: List[Minion]
  ): this.type = {
    this.clear()
    
    val node = (elem \ "champion").head
    val championID = XML2Value(node) int 'id
    this.champion = champions.find(_.profile.id == championID)

    (elem \ "minions" \ "minion") foreach {
      node =>
      val minionID = XML2Value(node) int 'id
      
      minions.find(_.profile.id == minionID).foreach {
        this.minions += _
      }
    }
    
    this
  }

  def random(champions: List[Champion], minions: List[Minion]): this.type = {
    import scala.util.Random.{nextInt, shuffle}

    this.clear()    
    this.champion = Option(champions(nextInt(champions.size)))

    shuffle(minions) foreach {
      minion =>
      this.minions += minion
      if (this.isCostOver) this.minions -= minion
      if (this.isMaxCost)  return this
    }

    this
  }
}
