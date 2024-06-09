package nyub.orthogit.git

import nyub.orthogit.reftree.RefTree
import scala.collection.mutable.ArrayBuffer

final private class InMemoryStagingArea[NodeId, LeafId, PathElement, Obj]
    extends StagingArea[NodeId, LeafId, PathElement, Obj]:
    private val stagedList: ArrayBuffer[
      StagedObject[PathElement, RefTree[NodeId, LeafId, PathElement, Obj]]
    ] = ArrayBuffer()

    override def add(
        obj: StagedObject[
          PathElement,
          RefTree[NodeId, LeafId, PathElement, Obj]
        ]
    ): Unit = stagedList.addOne(obj)

    override def staged: Seq[
      StagedObject[PathElement, RefTree[NodeId, LeafId, PathElement, Obj]]
    ] = stagedList.toSeq

    override def clear(): Unit = stagedList.clear()
