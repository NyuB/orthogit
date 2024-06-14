package nyub.orthogit.experiment

import java.nio.file.Paths
import java.nio.charset.StandardCharsets
import java.io.FileOutputStream
import nyub.orthogit.real.RealGit
import nyub.orthogit.id.Sha1.hex
import nyub.orthogit.reftree.RefNode
import nyub.orthogit.reftree.RefLeaf
import nyub.orthogit.reftree.ValueNode
import nyub.orthogit.reftree.ValueLeaf

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
            .map((id, meta) =>
                s"${id.hex}\n${meta.author}\n${meta.committer}\n${meta.message}"
            )
            .foreach(println)
    else if cmd == "show" then
        val git = RealGit(Paths.get(".git"))
        val treeId = git.getCommit(git.log(0)).treeId
        val tree = git.getTree(treeId)
        prettyPrint(git, tree)
    else println(s"Invalid command ${cmd}, supported commands are {unzip, log}")

def prettyPrint(git: RealGit, tree: git.Tree, tab: String = ""): Unit =
    tree.children.foreach: (p, r) =>
        r match
            case RefNode(id) =>
                val subTree = git.getTree(id)
                println(s"${tab}+ $p/${id.hex}")
                prettyPrint(git, subTree, tab + "--")
            case RefLeaf(id)         => println(s"${tab}> ${p} (${id.hex})")
            case ValueNode(children) => ???
            case ValueLeaf(value)    => ???
