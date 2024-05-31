package nyub.orthogit

import java.nio.file.Paths
import nyub.orthogit.git.StoredObjects

@main def main() =
    val id = RealGitObjectStorage(Paths.get(".test")).store(
      StoredObjects.Blob(Seq("123", "abc"))
    )
    println(s"Stored object with id ${id.hex}")
