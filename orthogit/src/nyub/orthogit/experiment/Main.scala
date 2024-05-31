package nyub.orthogit.experiment

import java.nio.file.Paths
import java.nio.file.Files
import java.nio.charset.StandardCharsets

@main def main(args: String*): Unit =
    val cmd = args(0)
    if cmd == "unzip" then
        val origin = args(1)
        val content = String(
          Zlib.decompressFile(Paths.get(origin)).toArray,
          StandardCharsets.UTF_8
        )
        if args.length < 3 then println(content)
        else
            val target = args(2)
            println(
              Files.writeString(
                Paths.get(target),
                content,
                StandardCharsets.UTF_8
              )
            )
    else println(s"Invalid command ${cmd}, supported commands are {unzip}")
