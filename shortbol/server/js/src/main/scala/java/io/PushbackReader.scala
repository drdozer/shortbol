package java.io

/**
  *
  *
  * @author Matthew Pocock
  */
class PushbackReader(in: Reader, size: Int) extends FilterReader(in) {
  def this(in: Reader) { this(in, 1) }

  private val buffer: Array[Char] = Array.ofDim(size)
  private var used: Int = 0

  @throws(classOf[IOException])
  override def mark(readAheadLimit: Int): Unit = throw new IOException("Mark not supported by PushbackReader")

  @throws(classOf[IOException])
  override def markSupported(): Boolean = false

  @throws(classOf[IOException])
  override def reset(): Unit = throw new IOException("Reset not supported by PushbackReader")

  @throws(classOf[IOException])
  override def skip(n: Long): Long = ???

  @throws(classOf[IOException])
  override def close(): Unit = ???

  @throws(classOf[IOException])
  override def ready(): Boolean = ???

  @throws(classOf[IOException])
  override def read(cbuf: Array[Char], off: Int, len: Int): Int = ???

  @throws(classOf[IOException])
  override def read(): Int = ???

  @throws(classOf[IOException])
  def unread(cbuf: Array[Char]): Unit = ???

  @throws(classOf[IOException])
  def unread(c: Int): Unit = ???
}
