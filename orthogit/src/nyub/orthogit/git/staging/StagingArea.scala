package nyub.orthogit.git.staging

import nyub.orthogit.reftree.RefTree
import scala.collection.mutable.ArrayBuffer

trait StagingArea[NodeId, LeafId, Edge, Leaf]:
    private type StagedTree = RefTree[NodeId, LeafId, Edge, Leaf]
    def add(obj: StagedObject[Edge, StagedTree]): Unit
    def staged: Seq[StagedObject[Edge, StagedTree]]
    def clear(): Unit

object StagingArea:
    class InMemory[NodeId, LeafId, PathElement, Obj](using
        CanEqual[PathElement, PathElement]
    ) extends StagingArea[NodeId, LeafId, PathElement, Obj]:
        private val stagedList: ArrayBuffer[
          StagedObject[PathElement, RefTree[NodeId, LeafId, PathElement, Obj]]
        ] = ArrayBuffer()

        override def add(
            obj: StagedObject[
              PathElement,
              RefTree[NodeId, LeafId, PathElement, Obj]
            ]
        ): Unit =
            stagedList.filterInPlace(o =>
                !obj.path.contains(o.path) && !o.path.contains(obj.path)
            )
            stagedList.addOne(obj)

        override def staged: Seq[
          StagedObject[PathElement, RefTree[NodeId, LeafId, PathElement, Obj]]
        ] = stagedList.toSeq

        override def clear(): Unit = stagedList.clear()
