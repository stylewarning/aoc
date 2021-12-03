;;;; 1.lisp
;;;;
;;;; https://adventofcode.com/2021/day/1

(cl:in-package #:aoc2021-cl)

;;; Look ma, no Lisp!

(cl:in-package #:aoc2021)

(coalton-toplevel
  ;; Part I

  (define aoc1-input-file (aoc-relative "2021/1/1.input"))


  (declare read-aoc1-data (Unit -> (List Integer)))
  (define (read-aoc1-data _)
    "Read the data for problem 1, which is a list of integers."
    (let ((data (fromSome "Couldn't read AOC1 data."
                          (read-file-into-string aoc1-input-file))))
      (map parse-int-or-fail
           (filter (/= "") (split-string #\Newline data)))))


  (declare changes (Ord :a => ((List :a) -> (List Ord))))
  (define (changes things)
    "Return a list of neighboring numerical comparisons found in THINGS."
    (match things
      ((Nil) Nil)
      ((Cons _ tail)
       (zipWith <=> things tail))))


  (declare count-increasing-pairs ((List Integer) -> Integer))
  (define (count-increasing-pairs ints)
    "How many times does the list of INTS increase?"
    (countBy (== LT) (changes ints)))


  (declare aoc1.1-solution (Unit -> Integer))
  (define (aoc1.1-solution _)
    (count-increasing-pairs (read-aoc1-data)))


  ;; Part II
  (declare cleave (Integer -> (List :a) -> (Tuple (List :a) (List :a))))
  (define (cleave n xs)
    "Chop XS into a two lists, a head (of length N or less) and a tail (the remainder)."
    (let ((rec (fn (n xs ys)
                 (if (== 0 n)
                     (Tuple (reverse ys) xs)
                     (match xs
                       ((Cons x xs) (rec (- n 1) xs (Cons x ys)))
                       ((Nil) (rec 0 xs ys)))))))
      (rec n xs Nil)))


  (declare windows (Integer -> (List :a) -> (List (List :a))))
  (define (windows n xs)
    "Create a list of sliding windows of exactly length N from the list XS."
    (let ((rec (fn (xs windows)
                 (let ((head (take n xs)))
                   (if (== n (length head))
                       (rec (cdr xs) (cons head windows))
                       (reverse windows))))))
      (rec xs Nil)))


  (declare aoc1.2-solution (Unit -> Integer))
  (define (aoc1.2-solution _)
    (count-increasing-pairs (map sum (windows 3 (read-aoc1-data)))))

)                                       ; COALTON-TOPLEVEL

;;; Pain points with Coalton:
;;;
;;; - List printing is awful and nearly unusable.
;;;
;;; - A lot of utilities had to call directly into Lisp.
;;;
;;; - Sloppy error handling.
;;;
;;; - a FILTER bug bit me (now fixed)
;;;
;;; - annoying to always type COALTON in the REPL
;;;
;;; - lots of missing list utilities
;;;
;;; - trailing whitespace from COALTON-TOPLEVEL
