package scryetek.gl

import java.nio._

import scala.scalajs.js.typedarray._
import scala.scalajs.js.JSConverters._

object BufferUtils {
  def allocateByteBuffer(length: Int): ByteBuffer =
    TypedArrayBuffer.wrap(new Int8Array(length))

  def allocateByteBuffer(array: Array[Byte]): ByteBuffer =
    TypedArrayBuffer.wrap(new Int8Array(array.toJSArray))

  def allocateShortBuffer(length: Int): ShortBuffer =
    TypedArrayBuffer.wrap(new Int16Array(length))

  def allocateShortBuffer(array: Array[Short]): ShortBuffer =
    TypedArrayBuffer.wrap(new Int16Array(array.toJSArray))

  def allocateIntBuffer(length: Int): IntBuffer =
    TypedArrayBuffer.wrap(new Int32Array(length))

  def allocateIntBuffer(array: Array[Int]): IntBuffer =
    TypedArrayBuffer.wrap(new Int32Array(array.toJSArray))

  def allocateFloatBuffer(length: Int): FloatBuffer =
    TypedArrayBuffer.wrap(new Float32Array(length))

  def allocateFloatBuffer(array: Array[Float]): FloatBuffer =
    TypedArrayBuffer.wrap(new Float32Array(array.toJSArray))

  def size(buffer: Buffer): Int =
    buffer match {
      case x if x == null => 0
      case _: ByteBuffer => 1
      case _: IntBuffer => 4
      case _: ShortBuffer => 2
      case _: FloatBuffer => 4
      case _: DoubleBuffer => 8
      case _: LongBuffer => 8
      case _: CharBuffer => 2
      case _ => throw new RuntimeException("Unexpected buffer type " + buffer.getClass.getName)
    }
}
