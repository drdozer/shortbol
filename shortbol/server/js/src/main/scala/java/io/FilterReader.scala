package java.io
import java.nio.CharBuffer

/**
  *
  *
  * @author Matthew Pocock
  */
abstract class FilterReader(protected val in: Reader) extends Reader {

  if(in == null) throw new NullPointerException("Can not create a FilterReader with null reader")

  override def close() = in.close()

  override def mark(readAheadLimit: Int) = in.mark(readAheadLimit)

  override def skip(n: Long) = in.skip(n)

  override def markSupported() = in.markSupported()

  override def ready() = in.ready()

  override def read() = in.read()

  override def reset() = in.reset()
}
