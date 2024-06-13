package nyub.orthogit.experiment

import java.nio.file.Paths
import java.nio.charset.StandardCharsets
import java.io.FileOutputStream
import nyub.orthogit.real.RealGit
import nyub.orthogit.id.Sha1.hex

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
    else if cmd == "log" then
        val git = RealGit(Paths.get(".git"))
        git.log
            .map(id => id -> git.getCommit(id).meta)
            .map((id, msg) => s"${id.asId.hex}\n${msg}")
            .foreach(println)
    else println(s"Invalid command ${cmd}, supported commands are {unzip, log}")
