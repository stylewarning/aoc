;;;; 3.lisp
;;;;
;;;; https://adventofcode.com/2021/day/3

(cl:in-package #:aoc2021-cl)

;;; Look ma, no Lisp!

(cl:in-package #:aoc2021)

(coalton-toplevel
  ;; Part I

  (define aoc3-input-file (aoc-relative "2021/3/3.input"))


  (declare read-aoc3-data (Unit -> (List (List Integer))))
  (define (read-aoc3-data _)
    (let ((data (fromSome "Couldn't read AOC3 data."
                          (read-file-into-string aoc3-input-file)))
          (char->int (fn (c)
                       (match c
                         (#\0 0)
                         (#\1 1)
                         (_   (error "Invalid binary digit character."))))))
      (map (compose (map char->int) unpack-string)
           (filter (/= "") (string-lines data)))))


  (declare count-01 ((List Integer) -> (Tuple Integer Integer)))
  (define (count-01 ints)
    (Tuple (countBy (== 0) ints)
           (countBy (== 1) ints)))


  (declare gamma-bits ((List (List Integer)) -> (List Integer)))
  (define (gamma-bits numbers)
    (let ((gamma-bit (fn (zeros-ones)
                       (match zeros-ones
                         ((Tuple zeros ones) (if (> zeros ones) 0 1))))))
      (map (compose gamma-bit count-01) (transpose numbers))))


  (declare epsilon-bits-from-gamma-bits ((List Integer) -> (List Integer)))
  (define epsilon-bits-from-gamma-bits (map (- 1)))


  (declare bits->int ((List Integer) -> Integer))
  (define bits->int (fold (fn (b sum) (pipe sum (* 2) (+ b))) 0))


  (declare aoc3.1-solution (Unit -> Integer))
  (define (aoc3.1-solution _)
    (let ((gamma   (gamma-bits (read-aoc3-data)))
          (epsilon (epsilon-bits-from-gamma-bits gamma)))
      (* (bits->int gamma)
         (bits->int epsilon))))


  ;; Part II

  (declare life-support-rating ((Integer -> Integer -> Boolean)
                                -> (List (List Integer))
                                -> (List Integer)))
  (define (life-support-rating f data)
    (let ((n (length (nth 0 data)))
          (rec (fn (i data)
                 (if (or (singleton-list? data)
                         (== n i))
                     data
                     (match (count-01 (map (nth i) data))
                       ((Tuple zeros ones)
                        (if (f zeros ones)
                            (rec (+ 1 i) (filter (compose (== 0) (nth i)) data))
                            (rec (+ 1 i) (filter (compose (== 1) (nth i)) data)))))))))
      (match (rec 0 data)
        ((Nil)               (error "Didn't find a life-support rating"))
        ((Cons rating (Nil)) rating)
        (_                   (error "Ambiguous life-support rating")))))


  (define oxygen-generator-rating (life-support-rating >))
  (define co2-generator-rating    (life-support-rating <=))


  (declare aoc3.2-solution (Unit -> Integer))
    (define (aoc3.2-solution _)
    (let ((data   (read-aoc3-data)))
      (* (bits->int (oxygen-generator-rating data))
         (bits->int (co2-generator-rating data)))))
)
