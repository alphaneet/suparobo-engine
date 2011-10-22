
object Main extends processing.core.PApplet {
  override def setup() {
    size(400, 300)
    background(255, 0, 0)
  }
  override def draw() { }
  override def keyTyped() {
    key match {
      case 'q' | 'Q' => {
        sys.exit()
      }
      case _ => 
    }
  }
  def main(args: Array[String]) = runSketch()
}
