package com.github.alphaneet.suparobo_engine.game.scenes

case class DeckScene(applet: PApplet) extends Scene(applet) {
  override def draw() {
    def r = applet.random(255).toInt
    applet.background(r,r,r)
  }

  override def keyPressed() {
    if (applet.key == 'k') TitleScene(applet)
  }
}
