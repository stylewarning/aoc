;;;; 1.lisp
;;;;
;;;; https://adventofcode.com/2021/day/1

(cl:in-package #:aoc2021-cl)

;;; Look ma, no Lisp!

(cl:in-package #:aoc2021)

(coalton-toplevel
  ;; Part I
  (define aoc1-input-file
    (lisp String ()
      (cl:namestring
       (asdf:system-relative-pathname ':aoc "2021/1/1.input"))))

  (declare read-file-into-string (String -> (Optional String)))
  (define (read-file-into-string filename)
    "Read the file named FILENAME into a string."
    (lisp (Optional String) (filename)
      (cl:let ((contents (cl:ignore-errors
                          (alexandria:read-file-into-string filename))))
        (cl:if (cl:null contents)
               None
               (Some contents)))))

  (declare split-string (Char -> String -> (List String)))
  (define (split-string c s)
    "Split the string S into substrings that are delineated by C."
    (lisp (List String) (c s)
      (cl-list-to-coalton
       (split-sequence:split-sequence c s))))

  (declare parse-int-or-fail (String -> Integer))
  (define (parse-int-or-fail s)
    (fromSome "Failed to parse integer." (parse-int s)))

  (declare read-aoc-data (Unit -> (List Integer)))
  (define (read-aoc-data _)
    (let ((data (fromSome "Couldn't read AOC1 data."
                          (read-file-into-string aoc1-input-file))))
      (map parse-int-or-fail
           (filter (/= "") (split-string #\Newline data)))))

  (define-instance (Eq Ord)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (LT) (LT)) True)
        ((Tuple (EQ) (EQ)) True)
        ((Tuple (GT) (GT)) True)
        (_             False)))
    (define (/= a b)
      (not (== a b))))

  (declare countBy ((:a -> Boolean) -> (List :a) -> Integer))
  (define (countby f things)
    (let ((rec (fn (things count)
                 (match things
                   ((Nil) count)
                   ((Cons x xs) (if (f x)
                                    (rec xs (+ 1 count))
                                    (rec xs count)))))))
      (rec things 0)))

  (declare changes (Ord :a => ((List :a) -> (List Ord))))
  (define (changes things)
    (match things
      ((Nil) Nil)
      ((Cons _ tail)
       (zipWith <=> things tail))))

  (declare count-increasing-pairs ((List Integer) -> Integer))
  (define (count-increasing-pairs ints)
    (countby (== LT) (changes ints)))

  (declare aoc1.1-solution (Unit -> Integer))
  (define (aoc1.1-solution _)
    (count-increasing-pairs (read-aoc-data)))

  ;; Part II
  (declare cleave (Integer -> (List :a) -> (Tuple (List :a) (List :a))))
  (define (cleave n xs)
    (let ((rec (fn (n xs ys)
                 (if (== 0 n)
                     (Tuple (reverse ys) xs)
                     (match xs
                       ((Cons x xs) (rec (- n 1) xs (Cons x ys)))
                       ((Nil) (rec 0 xs ys)))))))
      (rec n xs Nil)))

  (declare take (Integer -> (List :a) -> (List :a)))
  (define (take n xs)
    (fst (cleave n xs)))

  (declare cdr ((List :a) -> (List :a)))
  (define (cdr xs)
    (match xs
      ((Cons _ xs) xs)
      ((Nil) Nil)))

  (declare windows (Integer -> (List :a) -> (List (List :a))))
  (define (windows n xs)
    (let ((rec (fn (xs windows)
                 (let ((head (take n xs)))
                   (if (== n (length head))
                       (rec (cdr xs) (cons head windows))
                       (reverse windows))))))
      (rec xs Nil)))

  (declare aoc1.2-solution (Unit -> Integer))
  (define (aoc1.2-solution _)
    (count-increasing-pairs (map sum (windows 3 (read-aoc-data)))))

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

