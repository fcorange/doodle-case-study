package doodle.core

import doodle.backend.Canvas


sealed trait Image {
  def on(that: Image): Image = On(this, that)

  def beside(that: Image): Image = Beside(this, that)

  def above(that: Image): Image = Above(this, that)

  def draw(canvas: Canvas, originX: Double, originY: Double): Unit = {
    case Circle(r) => canvas.circle(0.0, 0.0, r)
    case Rectangle(w, h) => canvas.rectangle(-w / 2, h / 2, w, h)
    case On(a, b) => a.draw(canvas, originX, originY); b.draw(canvas, originX, originY)
    case Above(a, b) => {
      val abBox = this.boundingBox
      val aBox = a.boundingBox
      val bBox = b.boundingBox

      val aOriginY = originY + abBox.top - (aBox.height / 2)
      val bOriginY = originY + abBox.bottom + (bBox.height / 2)

      a.draw(canvas, originX, aOriginY)
      b.draw(canvas, originX, bOriginY)
    }
    case Beside(a, b) => {
      val abBox = this.boundingBox
      val aBox = a.boundingBox
      val bBox = b.boundingBox

      val aOriginX = originX + abBox.left + aBox.width / 2
      val bOriginX = originY + abBox.right - bBox.width / 2

      a.draw(canvas, aOriginX, originY)
      b.draw(canvas, bOriginX, originY)
    }
      
  }

  val boundingBox: BoundingBox = {
    case Circle(r) => BoundingBox(-r, r, r, -r)
    case Rectangle(w, h) => BoundingBox(-w / 2, h / 2, w / 2, -h / 2)
    case On(a, b) => a.boundingBox on b.boundingBox
    case Above(a, b) => a.boundingBox above b.boundingBox
    case Beside(a, b) => a.boundingBox beside b.boundingBox
  }


}

final case class Circle(radius: Double) extends Image

final case class Rectangle(width: Double, height: Double) extends Image

final case class On(A: Image, B: Image) extends Image

final case class Above(A: Image, B: Image) extends Image

final case class Beside(A: Image, B: Image) extends Image


final case class BoundingBox(left: Double, top: Double, right: Double, bottom: Double) {

  val height: Double = top - bottom

  val width: Double = right - left

  def above(that: BoundingBox): BoundingBox = BoundingBox(
    math.min(this.left, that.left),
    (this.height + that.height) / 2,
    math.max(this.right, that.right),
    -(this.height + that.height) / 2
  )

  def beside(that: BoundingBox): BoundingBox = BoundingBox(
    -(this.width + that.width) / 2,
    math.max(this.top, that.top),
    (this.width + that.width) / 2,
    math.min(this.bottom, that.bottom)
  )

  def on(that: BoundingBox): BoundingBox = BoundingBox(
    math.min(this.left, that.left),
    math.max(this.top, that.top),
    math.max(this.right, that.right),
    math.min(this.bottom, that.bottom)
  )

}
