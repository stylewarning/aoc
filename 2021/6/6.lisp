;;;; 6.lisp
;;;;
;;;; https://adventofcode.com/2021/day/6

(cl:in-package #:aoc2021-cl)

;;; Look ma, no Lisp!

(cl:in-package #:aoc2021)

(coalton-toplevel
  ;; Part I

  (define (population-state pop)
    (map (fn (x) (countby (== x) pop)) (range 0 8)))

  (define aoc6-input-file (aoc-relative "2021/6/6.input"))

  (define (read-aoc6-data _)
    (pipe
     (read-file-into-string aoc6-input-file)
     (fromSome "oof")
     string-lines
     car                                ; just the first line suffices
     (split #\,)
     (map parse-int-or-fail)
     population-state))

  (define (evolve-population-once state)
    (match state
      ((Cons d0 (Cons d1 (Cons d2 (Cons d3 (Cons d4 (Cons d5 (Cons d6 (Cons d7 (Cons d8 (Nil))))))))))
       (make-list d1                    ; 0
                  d2                    ; 1
                  d3                    ; 2
                  d4                    ; 3
                  d5                    ; 4
                  d6                    ; 5
                  (+ d7 d0)             ; 6
                  d8                    ; 7
                  d0))                  ; 8
      (_
       (error "invalid state"))))

  (define (evolve-population n state)
    (if (<= n 0)
        state
        (evolve-population (- n 1) (evolve-population-once state))))

  (define (aoc6.1-solution _)
    (pipe
     (read-aoc6-data)
     (evolve-population 80)
     sum))


  ;; Part II

  (define (aoc6.2-solution _)
    (pipe
     (read-aoc6-data)
     (evolve-population 256)
     sum)))
