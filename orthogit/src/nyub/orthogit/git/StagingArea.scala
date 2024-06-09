package nyub.orthogit.git

import nyub.orthogit.reftree.RefTree

trait StagingArea[NodeId, LeafId, Edge, Leaf]:
    private type StagedTree = RefTree[NodeId, LeafId, Edge, Leaf]
    def add(obj: StagedObject[Edge, StagedTree]): Unit
    def staged: Seq[StagedObject[Edge, StagedTree]]
    def clear(): Unit
