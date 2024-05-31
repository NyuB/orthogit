package nyub.orthogit

import java.nio.file.Paths
import nyub.orthogit.git.StoredObjects

@main def main() =
    val storage = RealGitObjectStorage(Paths.get(".test"))
    val id = storage.store(
      StoredObjects.Blob(Seq("Youpi", "abc"))
    )
    println(s"Stored object with id ${id.hex}")
    // Stored object with id 22289d8940255b2eacb8560a5667f5d77802a3f7
    val back = storage.get(id)

    println(s"Read back:\n'''\n${back
            .map(_.asInstanceOf[StoredObjects.Blob[Seq[String], ?, ?]].obj.mkString)
            .getOrElse("None")}\n'''")
    // Read back:
    // '''
    // blob 9Youpi
    // abc
    // '''
