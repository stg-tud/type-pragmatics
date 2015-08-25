package veritas.benchmarking.util

import scala.reflect.ClassTag

class GrowingArray[T : ClassTag](initialSize: Int) {
  var ar = new Array[T](initialSize)
  var maxindex = -1

  def size = ar.length

  def apply(ix: Int): T =
    if (ix < ar.length)
      ar(ix)
    else
      null.asInstanceOf[T]

  def update(ix: Int, t: T) = {
    if (ix >= ar.length) {
      val newlength = if (ar.length * 2 > 0) ar.length * 2 else Int.MaxValue
      val newar = new Array[T](newlength)
      scala.compat.Platform.arraycopy(ar, 0, newar, 0, ar.length)
      ar = newar
    }

    maxindex = Math.max(ix, maxindex)
    ar(ix) = t
  }

  def finalizedArray =
    if (ar.length - maxindex < 100)
      ar
    else {
      val smaller = new Array[T](maxindex + 1)
      scala.compat.Platform.arraycopy(ar, 0, smaller, 0, maxindex+1)
      ar = smaller
      ar
    }
}
