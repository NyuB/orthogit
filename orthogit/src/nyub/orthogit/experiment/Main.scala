package nyub.orthogit.experiment

import java.nio.file.Paths
import java.nio.charset.StandardCharsets
import java.io.FileOutputStream

@main def main(args: String*): Unit =
    val cmd = args(0)
    if cmd == "unzip" then
        val origin = args(1)
        val content =
            Zlib.decompressFile(Paths.get(origin)).toArray

        if args.length < 3 then println(String(content, StandardCharsets.UTF_8))
        else
            val target = args(2)
            val fos = FileOutputStream(Paths.get(target).toFile())
            fos.write(content)
            fos.flush()
            fos.close()
    else println(s"Invalid command ${cmd}, supported commands are {unzip}")
