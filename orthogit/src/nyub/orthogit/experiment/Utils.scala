package nyub.orthogit.experiment

import java.nio.file.Path
import java.io.FileInputStream
import java.util.zip.InflaterInputStream
import scala.collection.immutable.ArraySeq

object Zlib:
    def decompressFile(file: Path): Seq[Byte] =
        FileInputStream(file.toFile()).use: fis =>
            InflaterInputStream(fis).use: is =>
                ArraySeq.unsafeWrapArray(is.readAllBytes())

extension [T <: AutoCloseable](resource: T)
    def use[R](exec: T => R): R =
        try
            exec(resource)
        finally
            resource.close()
