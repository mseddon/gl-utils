package scryetek.gl


import org.scalajs.dom.raw.{WebGLRenderingContext => GL}
import org.scalajs.dom.html.Image

class ImageCache(gl: GL, val width: Int, val height: Int) {
  import ImageCache._

  var layers = Seq.empty[Layer]
  var images = Seq.empty[SubImage]
  var currentLayer = 0

  def add(image: Image): SubImage = {
    val img = new SubImage(image, null, 0, 0)
    images :+= img
    img
  }

  def addPacked(image: Image): SubImage = {
    val img = new SubImage(image, null, 0, 0)
    images :+= img
    packImage(img)
    img
  }

  private def findLevel(width: Int, height: Int): Option[Level] = {
    if(width + 2 > this.width || height + 2 > this.height)
      None // texture is too large!
    else
      layers(currentLayer).findLevel(width, height).orElse {
        layers :+= new Layer(gl, this)
        currentLayer += 1
        layers(currentLayer).findLevel(width, height)
      }
  }

  def pack(gl: GL): Unit = {
    layers.foreach { layer => layer.texture.release(gl) }
    layers = Seq(new Layer(gl, this))
    currentLayer = 0

    for (image <- images.sortBy(-_.height))
      packImage(image)
  }

  private def packImage(image: SubImage): Unit =
    findLevel(image.width, image.height).foreach { level =>
      image.x = width - level.remain + 1
      image.y = level.y + 1
      image.layer = level.layer
      level.remain -= image.width + 2
      level.height = level.height max (image.height + 2)
      gl.texSubImage2D(GL.TEXTURE_2D, 0, image.x.toInt, image.y.toInt, GL.RGBA, GL.UNSIGNED_BYTE, image.image)
    }
}

object ImageCache {
  class Layer(gl: GL, val imageCache: ImageCache) {
    var levels = Seq.empty[Level]
    var y = 0
    val texture = new Texture(gl, imageCache.width, imageCache.height)


    def findLevel(width: Int, height: Int): Option[Level] = {
      for (level <- levels.reverseIterator if level.height >= height && level.remain >= width)
        return Some(level)
      y += levels.lastOption.map(_.height).getOrElse(0)
      if (y + height >= imageCache.height)
        return None
      val level = new Level(this, y, height, imageCache.width)
      levels :+= level
      Some(level)
    }
  }
  class Level(val layer: Layer, var y: Int, var height: Int, var remain: Int)

}

class SubImage(var image: Image, var layer: ImageCache.Layer, var x: Float, var y: Float) {
  def width = image.naturalWidth
  def height = image.naturalHeight

  def release = layer.imageCache
}