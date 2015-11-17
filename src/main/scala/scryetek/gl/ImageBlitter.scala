package scryetek.gl

import org.scalajs.dom.raw.{WebGLRenderingContext => GL}
import scryetek.vecmath._

import scala.scalajs.js
import scala.scalajs.js.typedarray.TypedArrayBufferOps._

/**
 * A low-level opengl image blitter.  Streams quads to the video card while attempting to minimize state changes,
 * includes transformation and stencil-buffer/rectangle based clipping.
 *
 * @param gl the GL context to initialize the blitter on
 */
class ImageBlitter(gl: GL) {
  case class TransformedClip(topLeft: Vec2, topRight: Vec2, bottomLeft: Vec2, bottomRight: Vec2, level: Int)

  protected var stack = collection.mutable.Stack[(Mat2d, TransformedClip)]()
  var transform = Mat2d()
  private var currentClip: TransformedClip = null
  private var topClip: TransformedClip = null

  /** Returns true if there is no clipping in effect. */
  def noClipping = stencilLevel == 0

  def reset(): Unit = {
    stack.clear()
    transform = Mat2d()
    currentClip = null
    topClip = null
    stencilLevel = 0
  }

  def translate(t: Vec2): Unit =
    transform.preMultiply(Mat2d.translate2d(t))

  def rotate(angle: Float): Unit =
    transform.preMultiply(Mat2d.rotate2d(angle))

  def scale(s: Vec2): Unit =
    transform.preMultiply(Mat2d.scale2d(s))

  def save(): Unit = {
    stack.push((transform.copy(), topClip))
    topClip = null
  }

  def restore(): Unit = {
    val (oldTrans, oldClip) = stack.pop()
    if(topClip != null)
      resetClip(topClip)
    topClip = oldClip
    transform = oldTrans
  }

  private val shader = {
    val vs = gl.createShader(GL.VERTEX_SHADER)
    gl.shaderSource(vs,
      """attribute vec3 aVertexPosition;
        |attribute vec4 aColorCoord;
        |attribute vec2 aTextureCoord;
        |
        |uniform mat4 modelView;
        |uniform mat4 projection;
        |
        |varying vec2 texCoord;
        |varying vec4 colour;
        |void main(void) {
        |    gl_Position = projection * modelView * vec4(aVertexPosition, 1.0);
        |    texCoord = aTextureCoord;
        |    colour = aColorCoord;
        |}
        |""".stripMargin)
    gl.compileShader(vs)

    val fsSimple = gl.createShader(GL.FRAGMENT_SHADER)
    gl.shaderSource(fsSimple,
      """#ifdef GL_ES
        |precision mediump float;
        |#endif
        |uniform sampler2D texSampler;
        |varying vec4 colour;
        |varying vec2 texCoord;
        |
        |void main(void) {
        |    vec4 col = texture2D(texSampler, texCoord);
        |    gl_FragColor = col*colour;
        |}
        |""".stripMargin)

    // This shader isn't currently used, but implements signed distance field interpolation ala Valve's paper.
    val fs = gl.createShader(GL.FRAGMENT_SHADER)
    gl.shaderSource(fs,
      """#ifdef GL_ES
        |precision mediump float;
        |#endif
        |uniform sampler2D texSampler;
        |varying vec4 colour;
        |varying vec2 texCoord;
        |
        |void main(void) {
        |    vec4 col = texture2D(texSampler, texCoord);
        |    float a = smoothstep(0.4, 0.5, col.x);
        |    gl_FragColor = vec4(colour.rgb, a);
        |}
        |""".stripMargin)

    gl.compileShader(fs)
    gl.compileShader(fsSimple)

    val sp = gl.createProgram()
    gl.attachShader(sp, fsSimple)
    gl.attachShader(sp, vs)
    gl.linkProgram(sp)
    sp
  }

  val glErrors = Map(
    GL.INVALID_VALUE -> "GL_INVALID_VALUE",
    GL.INVALID_ENUM -> "GL_INVALID_ENUM",
    GL.INVALID_OPERATION  -> "GL_INVALID_OPERATION",
    GL.INVALID_FRAMEBUFFER_OPERATION -> "GL_INVALID_FRAMEBUFFER_OPERATION",
    GL.OUT_OF_MEMORY -> "GL_OUT_OF_MEMORY",
    GL.NO_ERROR -> "GL_NO_ERROR")


  private val aVertexPosition = gl.getAttribLocation(shader, "aVertexPosition")
  private val aColorCoord = gl.getAttribLocation(shader, "aColorCoord")
  private val aTextureCoord = gl.getAttribLocation(shader, "aTextureCoord")
  private val modelView = gl.getUniformLocation(shader, "modelView")
  private val projection = gl.getUniformLocation(shader, "projection")
  private val texSampler = gl.getUniformLocation(shader, "texSampler")

  private val buffer = BufferUtils.allocateFloatBuffer(200000)
  private val vbuffer = gl.createBuffer()
  private var currentTexture: Texture = null

  sealed trait RenderOp
  private case class ImageOp(texture: Texture, runStart: Int, count: Int) extends RenderOp
  private case class ClipOp(runStart: Int, count: Int, level: Int) extends RenderOp
  private case class ResetClipOp(runStart: Int, count: Int, level: Int) extends RenderOp

  private var renderList = Seq[RenderOp]()
  private var runStart = 0
  private var count = 0
  private var totalCount = 0

  val white = Vec4(1, 1, 1, 1)
  val identity = Mat2d()

  def drawImage(image: Texture, sx: Float, sy: Float, swidth: Float, sheight: Float, dx: Float, dy: Float, dwidth: Float, dheight: Float, colour: Vec4): Unit =
    addImage(image, sx, sy, swidth, sheight, dx, dy, dwidth, dheight, colour)

  def drawImage(image: Texture, sx: Float, sy: Float, swidth: Float, sheight: Float, dx: Float, dy: Float, dwidth: Float, dheight: Float): Unit =
    addImage(image, sx, sy, swidth, sheight, dx, dy, dwidth, dheight, white)

  def drawImage(image: Texture, dx: Float, dy: Float, dwidth: Float, dheight: Float, colour: Vec4): Unit =
    addImage(image, 0, 0, image.width, image.height, dx, dy, dwidth, dheight, colour)

  def drawImage(image: Texture, dx: Float, dy: Float, dwidth: Float, dheight: Float): Unit =
    addImage(image, 0, 0, image.width, image.height, dx, dy, dwidth, dheight, white)

  def drawImage(image: Texture, x: Float, y: Float): Unit =
    addImage(image, 0, 0, image.width, image.height, x, y, image.width, image.height, white)

  def drawImage(image: Texture, x: Float, y: Float, colour: Vec4): Unit =
    addImage(image, 0, 0, image.width, image.height, x, y, image.width, image.height, colour)

  def drawImageFlipY(image: Texture, x: Float, y: Float): Unit =
    drawImage(image, 0, image.height, image.width, -image.height, x, y, image.width, image.height)

  def drawImage(image: SubImage, dx: Float, dy: Float): Unit =
    drawImage(image, 0, 0, image.width, image.height, dx, dy, image.width, image.height, white)

  def drawImage(image: SubImage, dx: Float, dy: Float, colour: Vec4): Unit =
    drawImage(image, 0, 0, image.width, image.height, dx, dy, image.width, image.height, colour)

  def drawImage(image: SubImage, dx: Float, dy: Float, dwidth: Float, dheight: Float): Unit =
    drawImage(image, 0, 0, image.width, image.height, dx, dy, dwidth, dheight, white)

  def drawImage(image: SubImage, dx: Float, dy: Float, dwidth: Float, dheight: Float, colour: Vec4): Unit =
    drawImage(image, 0, 0, image.width, image.height, dx, dy, dwidth, dheight, colour)

  def drawImage(image: SubImage, sx: Float, sy: Float, swidth: Float, sheight: Float, dx: Float, dy: Float, dwidth: Float, dheight: Float): Unit =
    drawImage(image, sx, sy, swidth, sheight, dx, dy, dwidth, dheight, white)

  def drawImage(image: SubImage, sx: Float, sy: Float, swidth: Float, sheight: Float, dx: Float, dy: Float, dwidth: Float, dheight: Float, colour: Vec4): Unit =
    drawImage(image.layer.texture, image.x+sx, image.y+sy, swidth, sheight, dx, dy, dwidth, dheight, colour)

  /**
   * Undo the clip operation specified
   * @param transformedClip
   */
  private def resetClip(transformedClip: TransformedClip): Unit = {
    if (currentTexture != null)
      renderList :+= ImageOp(currentTexture, runStart, count)
    currentTexture = null
    runStart = totalCount
    count = 0

    def putVertex(v: Vec2): Unit = {
      buffer.put(0); buffer.put(0)
      buffer.put(v.x); buffer.put(v.y)
      buffer.put(0); buffer.put(0); buffer.put(0); buffer.put(1)
      count += 1
      totalCount += 1
    }

    if(count != 0)
      putVertex(transformedClip.topLeft)

    putVertex(transformedClip.topLeft)
    putVertex(transformedClip.topRight)
    putVertex(transformedClip.bottomLeft)
    putVertex(transformedClip.bottomRight)
    putVertex(transformedClip.bottomRight)

    stencilLevel = transformedClip.level
    renderList :+= ResetClipOp(runStart, count, stencilLevel)
    runStart = totalCount
    count = 0
  }

  def clipRect(dx: Float, dy: Float, dwidth: Float, dheight: Float): Unit = {
    if (currentTexture != null)
      renderList :+= ImageOp(currentTexture, runStart, count)
    currentTexture = null
    runStart = totalCount
    count = 0

    val topLeft = Vec2(dx, dy)
    val topRight= Vec2(dx+dwidth, dy)
    val bottomLeft= Vec2(dx, dy+dheight)
    val bottomRight = Vec2(dx+dwidth, dy+dheight)

    def putVertex(v: Vec2): Unit = {
      val out = transform * v
      buffer.put(0); buffer.put(0)
      buffer.put(out.x); buffer.put(out.y)
      buffer.put(0); buffer.put(0); buffer.put(0); buffer.put(1)
      count += 1
      totalCount += 1
    }

    if(count != 0)
      putVertex(topLeft)

    putVertex(topLeft)
    putVertex(topRight)
    putVertex(bottomLeft)
    putVertex(bottomRight)
    putVertex(bottomRight)

    currentClip = TransformedClip(transform * topLeft, transform * topRight, transform * bottomLeft, transform * bottomRight, stencilLevel)
    stencilLevel += 1
    if(topClip == null)
      topClip = currentClip
    renderList :+= ClipOp(runStart, count, stencilLevel)
    runStart = totalCount
    count = 0
  }

  private def addImage(texture: Texture,  sx: Float, sy: Float, swidth: Float, sheight: Float, dx: Float, dy: Float, dwidth: Float, dheight: Float, colour: Vec4): Unit = {
    if(currentTexture != texture) {
      if (currentTexture != null)
        renderList :+= ImageOp(currentTexture, runStart, count)
      currentTexture = texture
      runStart = totalCount
      count = 0
    }
    if(buffer.remaining() < 8*6)
      draw(gl)

    val topLeft = Vec2(dx, dy)
    val topRight= Vec2(dx+dwidth, dy)
    val bottomLeft= Vec2(dx, dy+dheight)
    val bottomRight = Vec2(dx+dwidth, dy+dheight)

    val tx = sx/texture.width
    val ty = sy/texture.height

    val scaleX = swidth/dwidth/texture.width
    val scaleY = sheight/dheight/texture.height

    def putVertex(v: Vec2): Unit = {
      val out = transform * v
      buffer.put((v.x-dx)*scaleX+tx); buffer.put((v.y-dy)*scaleY+ty)
      buffer.put(out.x); buffer.put(out.y)
      buffer.put(colour.x); buffer.put(colour.y); buffer.put(colour.z); buffer.put(colour.w)
      count += 1
      totalCount += 1
    }


    if(count != 0)
      putVertex(topLeft)

    putVertex(topLeft)
    putVertex(topRight)
    putVertex(bottomLeft)
    putVertex(bottomRight)
    putVertex(bottomRight)
  }

  var stencilLevel = 0

  var projectionMatrix = Mat4()
  var modelViewMatrix = Mat4()

  val tempArray = new js.Array[Double](16)

  def getArray(m: Mat4): js.Array[Double] = {
    tempArray(0)  = m.m00; tempArray(1) = m.m10; tempArray(2)  = m.m20; tempArray(3)  = m.m30
    tempArray(4)  = m.m01; tempArray(5) = m.m11; tempArray(6)  = m.m21; tempArray(7)  = m.m31
    tempArray(8)  = m.m02; tempArray(9) = m.m12; tempArray(10) = m.m22; tempArray(11) = m.m32
    tempArray(12) = m.m03; tempArray(13) = m.m13; tempArray(14) = m.m23; tempArray(15) = m.m33
    tempArray
  }

  def draw(gl: GL): Unit = {
    gl.enable(GL.STENCIL_TEST)
    gl.colorMask(true, true, true, true)
    gl.depthMask(true)
    gl.stencilMask(0)

    if(currentTexture != null)
      renderList :+= ImageOp(currentTexture, runStart, count)
    buffer.flip()
    gl.bindBuffer(GL.ARRAY_BUFFER, vbuffer)
    gl.bufferData(GL.ARRAY_BUFFER, buffer.arrayBuffer(), GL.DYNAMIC_DRAW)
    gl.useProgram(shader)
    gl.uniformMatrix4fv(this.projection, false, getArray(projectionMatrix))
    gl.uniformMatrix4fv(this.modelView, false, getArray(modelViewMatrix))
    gl.enableVertexAttribArray(aVertexPosition)
    gl.enableVertexAttribArray(aTextureCoord)
    gl.enableVertexAttribArray(aColorCoord)
    gl.vertexAttribPointer(aVertexPosition, 2, GL.FLOAT, false, 32, 8)
    gl.vertexAttribPointer(aTextureCoord, 2, GL.FLOAT, false, 32, 0)
    gl.vertexAttribPointer(aColorCoord, 4, GL.FLOAT, false, 32, 16)

    for(op <- renderList) op match {
      case ImageOp(texture, runStart, count) =>
        // bind modelView and projection
        gl.stencilMask(0)
        gl.activeTexture(GL.TEXTURE0)
        gl.bindTexture(GL.TEXTURE_2D, texture.textureId)
        gl.uniform1i(texSampler, 0)
        gl.drawArrays(GL.TRIANGLE_STRIP, runStart, count)
      case ClipOp(runStart, count, level) =>
        gl.stencilMask(0xff)
        gl.colorMask(false, false, false, false)
        gl.depthMask(false)
        gl.stencilFunc(GL.EQUAL, level-1, 0xff)
        gl.stencilOp(GL.KEEP, GL.INCR, GL.INCR)
        gl.drawArrays(GL.TRIANGLE_STRIP, runStart, count)

        gl.colorMask(true, true, true, true)
        gl.depthMask(true)

        gl.stencilFunc(GL.EQUAL, level, 0xff)
        gl.stencilMask(0)
      case ResetClipOp(runStart, count, level) =>
        gl.stencilMask(0xff)
        gl.colorMask(false, false, false, false)
        gl.depthMask(false)
        gl.stencilFunc(GL.LESS, level, 0xff)
        gl.stencilOp(GL.KEEP, GL.REPLACE, GL.REPLACE)
        gl.drawArrays(GL.TRIANGLE_STRIP, runStart, count)

        gl.colorMask(true, true, true, true)
        gl.depthMask(true)

        gl.stencilFunc(GL.EQUAL, level, 0xff)
        gl.stencilMask(0)
    }
    runStart = 0
    totalCount = 0
    count = 0
    currentTexture = null
    renderList = Seq.empty
    buffer.clear()
  }
}
