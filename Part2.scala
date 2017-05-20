// 1. class A
class A(x: Int) extends Ordered[A] {

	val a = x
	val value = x
	def compare(other: A): Int = value.compareTo(other.value)

	override def toString() = "A<" + a + ">"
}

// 2. class B <: A
class B(x: Int, y: Int) extends A(x) {

	val b = y
	override val value = x + y
	override def toString() = "B<" + a + "," + b + ">"
}

// 3. class C[]
class C[+T](g: Int => T) {

	def apply(x: Int): T = g(x)
}

// 4. abstract class Tree
abstract class Tree[T <: Ordered[T]] {
	def insert(t: T): Tree[T]
} 

// 5. case class Leaf[T], Node[T], Empty[T]
case class Leaf[T <: Ordered[T]](label: T) extends Tree[T] {
	override def insert(t: T): Tree[T] = label.compare(t) match {
		case -1 => new Node(label, Empty(), Leaf(t))
		case 0 => new Node(label, Empty(), Leaf(t))
		case 1 => new Node(label, Leaf(t), Empty())
	}
	override def toString() = label.toString()
}

case class Node[T <: Ordered[T]](label: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
 	override def insert(t: T): Tree[T] = label.compare(t) match {
		case -1 => new Node(label, left, right.insert(t))
		case 0 => new Node(label, left, right.insert(t))
		case 1 => new Node(label, left.insert(t), right)
	}
	override def toString() = "(" + left.toString() + "," + label.toString() + "," + right.toString() + ")"
}

case class Empty[T <: Ordered[T]]() extends Tree[T] {
	override def insert(t: T): Tree[T] = new Leaf(t)
	override def toString() = ""
}

// 6. singleton class Part2
object Part2 {
	def applyTo10(c: C[A]): A = c.apply(10)

	def found[T <: Ordered[T]](y: T, tr: Tree[T]): Boolean = tr match {
		case Empty() => false
		case Leaf(label) => y.compare(label) == 0
		case Node(label, left, Empty()) => found(y, left) | y.compare(label) == 0
		case Node(label, Empty(), right) => found(y, right) | y.compare(label) == 0
		case Node(label, left, right) => found(y, left) | found(y, right) | y.compare(label) == 0
	}

	def test() {
		val c1 = new C((x: Int) => new A(x))
		println(c1.apply(3))

		val c2 = new C((x: Int) => new B(x + 1, x + 2))
		println(c2.apply(3))

		println(applyTo10(c1))
		println(applyTo10(c2))  //relies on covariant subtyping

		var t1: Tree[A] = Empty()

		t1 = t1.insert(new A(4))
		t1 = t1.insert(new A(3))
		t1 = t1.insert(new B(4, 1))
		t1 = t1.insert(new A(2))
		println(t1)

		val a3 = new A(3)
		val a5 = new A(5)
		val b21 = new B(2, 1)
		val b42 = new B(4, 2)

		if(found(a3, t1))
		println(a3 + "is found in " + t1)
		else
		println(a3 + "is not found in " + t1)

		if(found(a5, t1))
		println(a5 + "is found in " + t1)
		else
		println(a5 + "is not found in " + t1)

		if(found(b21, t1))
		println(b21 + "is found in " + t1)
		else
		println(b21 + "is not found in " + t1)

		if(found(b42, t1))
		println(b42 + "is found in " + t1)
		else
		println(b42 + "is not found in " + t1)
	}

	def main(args: Array[String]) = {
		test()
	}
}