;;;; 7.lisp
;;;;
;;;; https://adventofcode.com/2021/day/7

(cl:in-package #:aoc2021-cl)

;;; Look ma, no Lisp!

(cl:in-package #:aoc2021)

(coalton-toplevel
  ;; Part I

  (define aoc7-input-file (aoc-relative "2021/7/7.input"))

  (define (read-aoc7-data _)
    (pipe
     (read-file-into-string aoc7-input-file)
     (fromSome "oof")
     string-lines
     car                                ; just the first line suffices
     (split #\,)
     (map parse-int-or-fail)))

  (define (middle-element l)            ; not a median technically
    (index l (floor/ (length l) 2)))

  (define (aoc7.1-solution _)
    (let ((data (sort (read-aoc7-data)))
          (middle (pipe
                   data
                   middle-element
                   (fromSome "empty list"))))
      (sum (map (compose abs (- middle)) data))))


  ;; Part II
  ;;
  ;; I used a bunch of calculus here, starting with:
  ;;
  ;; tot(l) = sum(i = 1 to n) cost(c_i - l)
  ;;        = sum |c_i - l| (|c_i - l| + 1) / 2
  ;;
  ;; After differentiating, you'll get an ugly sgn function, but you
  ;; can bound its value easily enough, to finally get:
  ;;
  ;; mean(c_i) - 1/2 <= l <= mean(c_i) + 1/2

  (define (mean l)
    (exact/ (sum l) (length l)))

  (define (triangle-number n)
    (floor/ (* n (+ n 1)) 2))

  (define (crab-entourage-cost loc crabs)
    (pipe
     crabs
     (map (fn (crab) (triangle-number (abs (- crab loc)))))
     sum))

  (define (aoc7.2-solution _)
    (let ((crabs (read-aoc7-data))
          (m (mean crabs))
          (lower-bound (floor   (- m (/ 1 2))))
          (upper-bound (ceiling (+ m (/ 1 2)))))
      (pipe
       (range lower-bound upper-bound)
       (map (fn (x) (Tuple x (crab-entourage-cost x crabs))))
       (optimumBy (fn (x y) (< (snd x) (snd y))))
       (fromSome "no minimum")
       snd))))
