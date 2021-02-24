package in.ferrl

class MerkleTree[T](val root: Node[T])

object MerkleTree {

	def apply[T](data: Seq[T])(implicit hashFn: (T, Option[T]) => T): MerkleTree[T] = {
		val withLeaves = data.map(Leaf(_))
		build(withLeaves)
	}

	def build[T](nodes: Seq[Node[T]])(implicit hashFn: (T, Option[T]) => T): MerkleTree[T] = {
		if (nodes.length == 1) new MerkleTree[T](nodes.head)
		else {
			val withBranches = nodes.grouped(2).map {
				case Seq(a, b) => Branch(a, Some(b))
				case Seq(a) => Branch(a, None)
			}.toSeq
			build(withBranches)
		}
	}
}

