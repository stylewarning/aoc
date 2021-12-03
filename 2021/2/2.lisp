;;;; 2.lisp
;;;;
;;;; https://adventofcode.com/2021/day/1

(cl:in-package #:aoc2021-cl)

;;; Look ma, no Lisp!

(cl:in-package #:aoc2021)

(coalton-toplevel
  ;; Part I

  (define aoc2-input-file (aoc-relative "2021/2/2.input"))

  
  (define-type Sub-Instruction
    (Sub-Forward Integer)
    (Sub-Up Integer)
    (Sub-Down Integer))

  
  (declare parse-aoc2-line (String -> Sub-Instruction))
  (define (parse-aoc2-line str)
    (match (string-tokens str)
      ((Cons isn (Cons amt (Nil)))
       (match isn
         ("forward" (Sub-Forward (parse-int-or-fail amt)))
         ("up"      (Sub-Up      (parse-int-or-fail amt)))
         ("down"    (Sub-Down    (parse-int-or-fail amt)))
         (_         (error "Malformed sub instruction."))))
      (_
       (error "Malformed line."))))

  
  (declare read-aoc2-data (Unit -> (List Sub-Instruction)))
  (define (read-aoc2-data _)
    (let ((data (fromSome "Couldn't read AOC2 data."
                          (read-file-into-string aoc2-input-file))))
      (map parse-aoc2-line
           (filter (/= "") (string-lines data)))))

  
  (declare execute-sub-instructions ((List Sub-Instruction) -> (Tuple Integer Integer)))
  (define (execute-sub-instructions isns)
    (let ((exec (fn (isns x y)
                  (match isns
                    ((Nil)
                     (Tuple x y))
                    ((Cons isn isns)
                     (match isn
                       ((Sub-Forward n) (exec isns (+ x n) y))
                       ((Sub-Up n)      (exec isns x       (- y n)))
                       ((Sub-Down n)    (exec isns x       (+ y n)))))))))
      (exec isns 0 0)))

  
  (declare aoc2.1-solution (Unit -> Integer))
  (define (aoc2.1-solution _)
    (match (execute-sub-instructions (read-aoc2-data))
      ((Tuple x y) (* x y))))


  ;; Part II
  )
