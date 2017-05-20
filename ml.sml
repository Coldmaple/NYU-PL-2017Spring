(* Haoran Ma N18997526 *)
Control.Print.printDepth := 100;
Control.Print.printLength := 100;

(* 1. reverse L *)
fun helper(x, z) = 
	if null(x) then z
	else helper(tl(x), hd(x)::z);

fun reverse [] = []
	| reverse(x) = helper(x, [])

(* 2. new_reverse L *)
fun new_reverse [] = []
	| new_reverse x = 
	let 
		fun helper(x, z) = 
			if null(x) then z
			else helper(tl(x), hd(x)::z)
	in
		helper(x, [])
	end

(* 3. reduce_depth L *)
fun reduce_depth [] = []
	| reduce_depth(x::xs) = x @ reduce_depth(xs)

(* 4. list operations *)

infix elt
fun x elt [] = false
	| x elt (y::ys) = (x = y) orelse x elt ys

fun foo1 (op elt) x L1 = 
if x elt L1 then true else false

infix ++
fun [] ++ [] = []
	| x ++ [] = x
	| [] ++ x = x
	| x ++ (y::ys) = 
	if(y elt x) then x ++ ys
	else y :: (x ++ ys)

fun foo2 (op ++) L1 L2 = 
	L1 ++ L2

infix **
fun [] ** [] = []
	| x ** [] = []
	| [] ** x = []
	| x ** (y::ys) = 
	if(y elt x) then y :: (x ** ys)
	else (x ** ys)

fun foo3 (op **) L1 L2 = 
	L1 ** L2

infix --
fun [] -- [] = []
	| x -- [] = x
	| [] -- x = []
	| (x::xs) -- y =
	if(x elt y) then xs -- y
	else x :: (xs -- y)

fun foo4 (op --) L1 L2 = 
	L1 -- L2

(* 5. define tree datatype *)
datatype tree = leaf of int | node of int * tree * tree

(* 6. polymorphic tree datatype *)
datatype 'a ptree = pleaf of 'a | pnode of 'a * 'a ptree * 'a ptree

(* 7. interior T *)
fun interior (pleaf x) = []
	| interior (pnode(x, left, right)) = interior left @ (x :: interior right)

(* 8. mapTree T *)
fun mapTree f (pleaf x) = pleaf (f x)
	| mapTree f (pnode(x, left, right)) = pnode(f x, mapTree f left, mapTree f right)

(* 9. lexLess *)
fun lexLess (op <) [] [] = false
	| lexLess (op <) x [] = false
	| lexLess (op <) [] x = true
	| lexLess (op <) (x::xs) (y::ys) = 
	if x < y then true
	else if y < x then false
	else lexLess (op <) xs ys

(* 10. ptreeLess *)
fun ptreeLess (op <) (pleaf x) (pleaf y) = x < y
	| ptreeLess (op <) (pnode(x, left, right)) (pleaf y) = false
	| ptreeLess (op <) (pleaf x) (pnode(y, left, right)) = true
	| ptreeLess (op <) (pnode(x, l1, r1)) (pnode(y, l2, r2)) =
	if ptreeLess (op <) l1 l2 then true
	else if ptreeLess (op <) l2 l1 then false
	else if x < y then true
	else if y < x then false
	else ptreeLess (op <) r1 r2

(* test1 *)
fun foo (op >) x (y,z) =
	let fun bar a = if x > y then z else a
	in bar [1,2,3]
	end

(* test2 *)
fun compose f g x = hd(g(hd(f x)))

(* test3 *)
fun f x = f x

(* mylist datatype *)
datatype 'a mylist = nil | cons of 'a * 'a mylist

fun max (op >) (cons(x, nil)) = x 
	| max (op >) (cons(x, xs)) = if x > max (op >) xs then x else max (op >) xs

(* merge *)
fun merge [] [] = []
	| merge L1 [] = L1
	| merge [] L1 = L1
	| merge (x::xs) (y::ys) =
	if x < y then x :: (y :: merge xs ys)
	else y :: (x :: merge xs ys)

(* split *)
fun split [] = ([], [])
	| split x = ([x], [])
	| split (x::xs) = (x::tl(split xs), hd(split xs))

