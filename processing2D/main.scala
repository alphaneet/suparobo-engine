package com.github.alphaneet.suparobo

object Main extends SPApplet {
  val screenSize = new Dimension(800, 600)
  override def setup() {
    size(processing.core.PConstants.P2D)
    frameRate(24)
    title = "スーパー東方大戦"
    DeckMakeScene(this)
  }
}
