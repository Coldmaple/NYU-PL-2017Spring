;;; Scheme Assignment

;;; 1. 
;;; Base case: L contains only one element, return the first element of L, (car L).
;;; Assumption: find-min works on (cdr L), returning the smallest element of (cdr L).
;;; Step: Let x be the result of calling (find-min (cdr L)). If (car L) is less
;;; than x, return (car L). Otherwise, return x.

(define (find-min L)
  (cond ((null? (cdr L)) (car L))
        (else (cond ((< (car L) (find-min (cdr L))) (car L))
              (else (find-min (cdr L)))))))

;;; test case: (4 1 7 2 9 10)
(find-min `(4 1 7 2 9 10))

;;; 2.
;;; Base Case: If L has one element, return a list containing (car L) and ’().
;;; Assumption: find-min-rest works on (cdr L), returning a list which contains
;;; the smallest element and the rest of the list (also a list).
;;; Step: Let x be the result of calling find-min-rest (cdr L). If (car L) is less
;;; than (car x), connect (car L) with (cdr L) and return it. Otherwise, put 
;;; (car x) into the first place and connect others, and return.

(define (find-min-rest L)
  (cond ((null? (cdr L)) (list (car L) `()))
        (else (let* ((x (find-min-rest (cdr L))) (y (cadr x))) 
                (cond ((< (car L) (car x)) (list (car L) (cdr L)))
                (else (list (car x) (cons (car L) y))))))))

;;; test case: (4 1 7 2 9 10)
;;; Note: my program maintains the order of original list when selecting the smallest.
(find-min-rest `(4 1 7 2 9 10))

;;; 3.
;;; Base Case: If L is an empty list, return null
;;; Assumption: After we have chosen the smallest element of the original
;;; list, sort works on the rest of the list.
;;; Step: Connect the already sorted list with the unsorted list.

(define (sort L)
  (cond ((null? L) `())
        (else (let ((x (find-min-rest L)))
              (cons (car x) (sort (car (cdr x))))))))

;;; test case: (3 4 1 2 6 9)
(sort `(3 4 1 2 6 9))

;;; 4.
;;; Base Case: If L is empty, return 0; Or it is not a list, return itself.
;;; Assumption: Assume (sum-list (car L)) returns the sum of the elements
;;; in possibly nested list (car L) and (sum-list (cdr L)) returns the
;;; sum of the elements in (cdr L)
;;; Step: (sum-list L) = (+ (sum-list (car L)) (sum-list (cdr L)))

(define (sum-list L)
  (cond ((null? L) 0)
        ((not (list? L)) L)
        (else (+ (sum-list (car L)) (sum-list (cdr L))))))

;;; test case: (2 3 (4 5) (6 (7 8)) 9)
(sum-list `(2 3 (4 5) (6 (7 8)) 9))

;;; 5.
;;; Base Case: L1 and L2 are both empty lists, return `()
;;; Assumption: (map2 f L1 L2) works on (cdr L1) and (cdr L2) and return the results of
;;; applying f to every element of (cdr L1) and (cdr L2).
;;; Step: Put the result of (f (car L1) (car L2)) in front of the result of
;;; (map2 f (cdr L1) (cdr L2))

(define (map2 f L1 L2)
  (cond ((null? L1) `())
        (else (cons (f (car L1) (car L2)) (map2 f (cdr L1) (cdr L2))))))

;;; test case: (lambda (x y) (+ x y)) ’(1 2 3 4 5) ’(10 20 30 40 50)
(map2 (lambda (x y) (+ x y)) `(1 2 3 4 5) `(10 20 30 40 50))

;;; 6.
;;; Base Case: n = m, only one element, return list (n) (cons n `())
;;; Assumption: Assume (num-from n+1 m) return a list containing number from n+1 to m
;;; Step: (cons n (num-from n+1 m))

(define (nums-from n m)
  (cond ((= n m) (cons n `()))
        (else (cons n (nums-from (+ 1 n) m)))))

;;; test case: n = 5, m = 15
(nums-from 5 15)
;;; Note: This program only considers situation m >= n.

;;; 7.
;;; Base Case: L is an empty list, return null.
;;; Assumption: Assume (remove-mults n (cdr L)) works on (cdr L) and remove all the
;;; multiples of 3 in (cdr L).
;;; Step: Let x be the result of (remove-mults n (cdr L)). If (car L) is a multiple of 3,
;;; connect it to x and return the result. Otherwise, return x.

(define (remove-mults n L)
  (cond ((null? L) `())
        (else (let ((x (remove-mults n (cdr L))))
                (cond ((= 0 (modulo (car L) n)) x)
                      (else (cons (car L) x)))))))

;;; test case1: 3 `(1 2 3 4 5 6 7 8 9)
;;; test case2: 2 (nums-from 2 20)
(remove-mults 3 `(1 2 3 4 5 6 7 8 9))
(remove-mults 2 (nums-from 2 20))

;;; 8.
;;; Base Case: L is an empty list, return null.
;;; Assumption: Assume (sieve (cdr L)) works on (cdr L) and returns the list of elements
;;; of L that are not multiples of each other.
;;; Step: Let x be the result of (sieve (cdr L)), implement (remove-mults (car L) x),
;;; let result be y. And return (cons (car L) y).

(define (sieve L)
  (cond ((null? L) `())
        (else (let* ((x (sieve (cdr L))) (y (remove-mults (car L) x)))
                (cons (car L) y)))))

;;; test case: (4 5 6 7 8 9 10 11 12 13 14 15 16 17)
(sieve `(4 5 6 7 8 9 10 11 12 13 14 15 16 17))

;;; 9.
(define (primes n)
  (sieve (nums-from 2 n)))

;;; test case: n = 30
(primes 30)

;;; 10.
;;; Define a new function named f, which gets a parameter x and inserts y+x, y+x+1,...,
;;; y+n. 
;;; Base Case: When x reaches n+1, we have already inserted all functions in the list, return
;;; empty list.
;;; Assumption: f works on x+1, and it inserted y+x+1, y+x+2,..., y+n into list. n is only
;;; an index, which shows that you inserted from x+1 to n.
;;; Step: put new function y+x in front of the list, using (cons (lambda(y) (+ y x)) list).
;;; Finally we initialize (f 1) to get x+1, x+2, ..., x+n

(define (gen-fn-list n)
  (letrec ((f (lambda (x) (if (= x (+ n 1)) `() (cons (lambda (y) (+ y x)) (f (+ x 1)))))))
    (f 1)))

;;; test case:  
(define fs (gen-fn-list 3))
((car fs) 10)
((cadr fs) 10)
(map (lambda (f) (f 10)) (gen-fn-list 6))