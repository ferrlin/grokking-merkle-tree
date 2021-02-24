package in.ferrl

trait Node[T] {
	val hash: T
}

case class Leaf[T](datum: T)(implicit hashFn: (T, Option[T]) => T) extends Node[T] {
	override val hash: T = hashFn(datum, None)
}

case class Branch[T](left: Node[T], right: Option[Node[T]])(implicit hashFn: (T, Option[T]) => T) extends Node[T] {
	override val hash: T = hashFn(left.hash, right.flatMap(r => Some(r.hash)))
}
