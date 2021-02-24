trait Node[T] {
	val hash: T
}

case class Leaf[T](data: T)(implicit hashFn: (T, Option[T]) => T) extends Node[T] {
	override val hash: T = hashFn(data, None)
}

case class Branch[T](left: Node[T], right: Option[Node[T]])(implicit hashFn: (T, Option[T]) => T) extends Node[T] {
	override val hash: T = hashFn(left.hash, right.flatMap(r => Some(r.hash)))
}

/**/
trait MerkleTree[T] {
	val root: Node[T]
}

object MerkleTree {

	def apply[T](inputs: Seq[T])(implicit fn: (T, Option[T]) => T): MerkleTree[T] = {
		val withLeaves = inputs.map(Leaf(_))
		build(withLeaves)
	}

	def build[T](nodes: Seq[Node[T]])(implicit fn: (T, Option[T]) => T): MerkleTree[T] = {
		if (nodes.length == 1) new MerkleTree[T] {
			override val root = nodes.head
		}
		else {
			val withBranches = nodes.grouped(2).map {
				case Seq(a, b) => Branch(a, Some(b))
				case Seq(a) => Branch(a, None)
			}.toSeq
			build(withBranches)
		}
	}
}

implicit val hashFn: (Array[Byte], Option[Array[Byte]]) => Array[Byte] = (byteArr, opt) => {
	val cc = opt match {
		case None => byteArr
		case _ => byteArr ++ opt.get
	}
	java.security.MessageDigest.getInstance("SHA-256").digest(cc)
}

case class Transaction(from: String, amount: BigDecimal, to: String) {
	def getBytes(): Array[Byte] = hashFn(from.getBytes ++ to.getBytes, Some(amount.toString.getBytes))
}

object Transaction {
	def bytesToBase64(bytes: Array[Byte]): String =
		java.util.Base64.getEncoder.encodeToString(bytes)
}

val trans1 = Transaction("bob", 10.70, "jim")
val trans2 = Transaction("jim", 20.33, "max")
val trans3 = Transaction("max", 33.33, "dom")
val inputs = Seq(trans1, trans2, trans3).map(_.getBytes)
val inputs2 = Seq(trans1, trans3).map(_.getBytes)

val one = MerkleTree(inputs)
val two = MerkleTree(inputs)

if (one == two) println("One and Two are the same") else println("No they are not")
println(s"one::${Transaction.bytesToBase64(one.root.hash)} == two::${Transaction.bytesToBase64(two.root.hash)}")
if (one.root == two.root) println("One's root and two's root are the same") else println("No the roots are not the same")

val three = MerkleTree(inputs2)
if (one == three) println("One and three are the same") else println("No, they are not")
println(s"one::${Transaction.bytesToBase64(one.root.hash)} == three::${Transaction.bytesToBase64(three.root.hash)}")
if (one.root == three.root) println("One's root and three's are the same") else println("No, the roots are not the same")

