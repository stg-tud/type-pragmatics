package veritas.benchmarking.util

import java.io.{File, FileInputStream, FileOutputStream}

object FileUtil {
  def copyContentOfFile(readFrom: File, writeTo: File): Unit = {
    var fos: FileOutputStream = null
    var fis: FileInputStream = null
    // TODO: is there a better way in scala? Something like try-with-resource in
    try {
      fos = new FileOutputStream(writeTo)
      fis = new FileInputStream(readFrom)
      fos getChannel() transferFrom(fis getChannel, 0, Long.MaxValue)
    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      try {
        if (fos != null) fos.close
        try {
          if (fis != null) fis.close
        } catch {
          case e: Exception => e.printStackTrace()
        }
      } catch {
        case e: Exception =>  e.printStackTrace()
      }
    }
  }
}
