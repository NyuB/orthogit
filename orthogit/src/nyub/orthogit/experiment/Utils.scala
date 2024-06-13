package nyub.orthogit.experiment

import java.nio.file.Path
import java.io.FileInputStream
import java.util.zip.InflaterInputStream
import scala.collection.immutable.ArraySeq
import java.io.FileOutputStream
import java.util.zip.DeflaterOutputStream

object Zlib:
    def decompressFile(file: Path): Seq[Byte] =
        FileInputStream(file.toFile()).use: fis =>
            InflaterInputStream(fis).use: is =>
                ArraySeq.unsafeWrapArray(is.readAllBytes())

    def compressIntoFile(file: Path, content: Seq[Byte]): Unit =
        FileOutputStream(file.toFile()).use: fos =>
            DeflaterOutputStream(fos).use: os =>
                os.write(content.toArray)
                os.flush()

extension [T <: AutoCloseable](resource: T)
    def use[R](exec: T => R): R =
        try
            exec(resource)
        finally
            resource.close()
