package in.ferrl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MerkleTreeSpec extends AnyFlatSpec with Matchers {

	case class Transaction(from: String, amount: Float, to: String) {
		def getBytes(): Array[Byte] = (from ++ to ++ amount.toString).getBytes
	}

	object Transaction {
		def bytesToBase64(bytes: Array[Byte]): String =
			java.util.Base64.getEncoder.encodeToString(bytes)
	}

	def fixture = {
		new {
			val trans1 = new Transaction("bob", 10.70f, "jim")
			val trans2 = new Transaction("jim", 20.33f, "max")
			val trans3 = new Transaction("max", 33.33f, "dom")
			val inputs = Seq(trans1, trans2, trans3).map(_.getBytes)
			val inputs2 = Seq(trans1, trans3).map(_.getBytes)

			val hashFn: (Array[Byte], Option[Array[Byte]]) => Array[Byte] = (left, opt) => {
				val cc = opt match {
					case None => left
					case _ => left ++ opt.get
				}
				java.security.MessageDigest.getInstance("SHA-256").digest(cc)
			}
		}
	}

	"Trees with the exactly the same inputs" should "produced the same hash output" in {

		implicit val fn  = fixture.hashFn

		val one = MerkleTree(fixture.inputs)
		val two = MerkleTree(fixture.inputs)

		one shouldNot equal(two)
		one.root shouldNot equal (two.root)

		one.root.hash shouldEqual two.root.hash
		Transaction.bytesToBase64(one.root.hash) shouldEqual Transaction.bytesToBase64(two.root.hash)
	}

	"Trees with different inputs" should "produce different hash output" in {

		implicit val fn = fixture.hashFn
		val one = MerkleTree(fixture.inputs)
		val three = MerkleTree(fixture.inputs2)

		one shouldNot equal(three)
		one.root shouldNot equal(three.root)

		one.root.hash shouldNot equal(three.root.hash)
		Transaction.bytesToBase64(one.root.hash) shouldNot equal(Transaction.bytesToBase64(three.root.hash))
	}
}
