package scryetek.gl

import org.scalajs.dom.raw.{WebGLRenderingContext => GL}
import org.scalajs.dom.html.Image

/**
 * Created by matt on 16/08/15.
 */
class Texture private (gl: GL) {
  val textureId = gl.createTexture()
  var _width: Int = 0
  var _height: Int = 0
  def this(gl: GL, image: Image) {
    this(gl)
    gl.bindTexture(GL.TEXTURE_2D, textureId)
    gl.texImage2D(GL.TEXTURE_2D, 0, GL.RGBA, GL.RGBA, GL.UNSIGNED_BYTE, image)
    gl.texParameteri(GL.TEXTURE_2D, GL.TEXTURE_WRAP_S, GL.CLAMP_TO_EDGE)
    gl.texParameteri(GL.TEXTURE_2D, GL.TEXTURE_WRAP_T, GL.CLAMP_TO_EDGE)
    gl.texParameteri(GL.TEXTURE_2D, GL.TEXTURE_MIN_FILTER, GL.LINEAR)
    gl.texParameteri(GL.TEXTURE_2D, GL.TEXTURE_MAG_FILTER, GL.LINEAR)
    _width = image.naturalWidth
    _height = image.naturalHeight
  }

  def this(gl: GL, width: Int, height: Int) {
    this(gl)
    gl.bindTexture(GL.TEXTURE_2D, textureId)
    gl.texImage2D(GL.TEXTURE_2D, 0, GL.RGBA, width, height, 0, GL.RGBA, GL.UNSIGNED_BYTE, null)
    gl.texParameteri(GL.TEXTURE_2D, GL.TEXTURE_WRAP_S, GL.CLAMP_TO_EDGE)
    gl.texParameteri(GL.TEXTURE_2D, GL.TEXTURE_WRAP_T, GL.CLAMP_TO_EDGE)
    gl.texParameteri(GL.TEXTURE_2D, GL.TEXTURE_MIN_FILTER, GL.LINEAR)
    gl.texParameteri(GL.TEXTURE_2D, GL.TEXTURE_MAG_FILTER, GL.LINEAR)
    _width = width
    _height = height
  }

  def width = _width
  def height = _height

  def release(gl: GL): Unit =
    gl.deleteTexture(textureId)
}

object Texture {
  def apply(gl: GL, image: Image): Texture =
    new Texture(gl, image)
  }