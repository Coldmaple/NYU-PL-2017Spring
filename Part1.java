import java.util.*;

// 1. Addable
interface Addable<T> {
	public T plus(T t);
}

// 2. MyList
class MyList<T extends Comparable<T> & Addable<T>> extends ArrayList<T> implements Comparable<MyList<T>>, Addable<MyList<T>> {
	private T sum;

	public MyList(T z) {
		sum = z;
	}

	public MyList<T> plus(MyList<T> other) {
		MyList<T> result = new MyList<T>(get(0));
		result = this;
		result.addAll(other);
		return result;
	}

	public int compareTo(MyList<T> other) {
		for(Integer i = 0; i < size(); i++) {
			sum = sum.plus(get(i)); 
		}
		for(Integer j = 0; j < other.size(); j++) {
			other.sum = other.sum.plus(other.get(j));
		}
		return sum.compareTo(other.sum);
	} 

	// @override toString()
	public String toString() {
		String result = "[ ";
		for(Integer i = 0; i < size(); i++) {
			result += get(i) + " ";
		}
		result += "]";
		return result;
	}
}

// 3. class A 
class A implements Comparable<A>, Addable<A> {
	Integer a;

	public A(Integer x) {
		a = x;
	}

	public A plus(A other) {
		A res = new A(a + other.a);
		return res;
	}

	public int compareTo(A other) {
		return a.compareTo(other.a);
	}

	// @override toString()
	public String toString() {
		return "A" + "<" + a + ">" + " ";
	}
}

// 4. class B
class B extends A {

	public B(Integer y) {
		super(y);
	}

	// @override toString()
	public String toString() {
		return "B" + "<" + a + ">" + " ";
	}
}

// 5. class Part1
class Part1 {
	private static <T> void insertIntoMyList(T z, MyList<? super T> L) {
		L.add(z);
	}

	public static void test() {
		MyList<A> m1 = new MyList<A>(new A(0));
		MyList<A> m2 = new MyList<A>(new A(0));

		for(Integer i = 1; i <= 6; i++) {
			insertIntoMyList(new A(i), m1);
			// insertIntoMyList(new A(2),m2);
			insertIntoMyList(new B(i), m2);
			// insertIntoMyList(new B(3),m1);
		}

		insertIntoMyList(new A(6), m2);
		System.out.println("m1 = " + m1);
		System.out.println("m2 = " + m2);
		int result = m1.compareTo(m2);
		String s = (result < 0) ? "less than" : (result == 0) ? "equal to" : "greater than";
		System.out.println("m1 is " + s + " m2");
		System.out.println("m1 + m2 = " + m1.plus(m2));
	}

	public static void main(String[] args) {
		boolean testf = true;
		if(testf) test();
	}
}