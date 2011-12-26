package com.github.alphaneet.suparobo_engine.game

object Main extends com.github.alphaneet.processing.PApplet {
  val screenSize = new Dimension(800, 600)
  override def setup() {
    size(processing.core.PConstants.P2D)
    frameRate(24)
    title = "スーパー東方大戦"
    com.github.alphaneet.suparobo_engine.game.scenes.TitleScene(this)
  }
}
