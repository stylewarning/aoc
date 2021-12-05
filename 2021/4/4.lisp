;;;; 4.lisp
;;;;
;;;; https://adventofcode.com/2021/day/4

(cl:in-package #:aoc2021-cl)

(defun %read-aoc4-file (filename)
  (with-open-file (s filename :direction ':input
                              :if-does-not-exist ':error)
    (let ((numbers (read-line s))
          (boards  nil))

      (ignore-errors
       (loop :do
         (assert (string= "" (read-line s)))
         (push (loop :repeat 5 :collect (read-line s)) boards)))
      (coalton-library:Tuple numbers boards))))

(cl:in-package #:aoc2021)

(coalton-toplevel
  ;; Part I

  ;; Parsing
  (define aoc4-input-file (aoc-relative "2021/4/4.input"))


  (declare parse-call-numbers (String -> (List Integer)))
  (define parse-call-numbers (compose (map parse-int-or-fail) (split #\,)))

  (declare parse-row-numbers (String -> (List Integer)))
  (define (parse-row-numbers str)
    (pipe
     str
     (split #\Space)
     (filter (/= ""))
     (map parse-int-or-fail)))

  (declare %read-aoc4-file (Unit -> (Tuple String (List (List String)))))
  (define (%read-aoc4-file _)
    (lisp (Tuple String (List (List String))) ()
      (aoc2021-cl::%read-aoc4-file aoc4-input-file)))

  (declare read-aoc4-data (Unit -> (Tuple (List Integer)
                                          (List (List (List Integer))))))
  (define (read-aoc4-data _)
    (match (%read-aoc4-file)
      ((Tuple row-string board-strings)
       (Tuple (parse-call-numbers row-string)
              (map (map parse-row-numbers) board-strings)))))

  ;; Finding winning calls
  (declare bingo-call-position-function ((List Integer)
                                         -> (Integer -> (Optional Integer))))
  (define (bingo-call-position-function calls)
    "Return an efficient function F such that F(n) returns the 1-based position of n in the list CALLS. (n may or may not exist.)"
    (let ((lowest  (fromSome "no min" (minimum calls)))
          (highest (fromSome "no max" (maximum calls)))
          (positions
            (the (Vector (Optional Integer))
                 (into (map (fn (call) (elemIndex call calls))
                            (range 0 highest))))))

      (fn (call)
        (do (candidate <- (vector-index call positions))
            (position <- candidate)
          (pure (+ 1 position))))))

  (declare rowcol-winning-call ((Integer -> (Optional Integer))
                                -> (List Integer)
                                -> (Optional Integer)))
  (define (rowcol-winning-call posfun rowcall)
    "Return the winning call for a single rowcol."
    (let ((rec (fn (numbers biggest-item biggest-position)
                 (match numbers
                   ((Nil) (Some biggest-item))
                   ((Cons x xs)
                    (do (xpos <- (posfun x))
                      (if (< biggest-position xpos)
                          (rec xs x xpos)
                          (rec xs biggest-item biggest-position))))))))
      (if (null rowcall)
          None
          (rec rowcall -1 -1))))

  (declare board-winning-call ((Integer -> (Optional Integer))
                               -> (List (List Integer))
                               -> (Optional Integer)))
  (define (board-winning-call posfun board)
    "Return the winning call of a board."
    (let ((rowcols (append board (transpose board)))
          (rowcol-win (rowcol-winning-call posfun)))
      (let ((rec (fn (rowcols smallest-call smallest-position)
                   (match rowcols
                     ((Nil) smallest-call)
                     ((Cons rowcol rest-rowcols)
                      (match (rowcol-win rowcol)
                        ((None)
                         (rec rest-rowcols smallest-call smallest-position))
                        ((Some win-call)
                         (let ((win-pos (fromSome "oof" (posfun win-call))))
                           (if (< win-pos smallest-position)
                               (rec rest-rowcols (Some win-call) win-pos)
                               (rec rest-rowcols smallest-call smallest-position))))))))))
        (rec rowcols None 10000))))

  (declare earliest-winning-board ((Integer -> (Optional Integer))
                                   -> (List (List (List Integer)))
                                   -> (Optional (Tuple Integer
                                                       (List (List Integer))))))
  (define (earliest-winning-board posfun boards)
    (let ((rec (fn (boards winning-board winning-call winning-position)
                 (match boards
                   ((Nil)
                    (match winning-call
                      ((None) None)
                      ((Some call)
                       (Some (Tuple call winning-board)))))
                   ((Cons board boards)
                    (match (board-winning-call posfun board)
                      ((None)
                       (rec boards winning-board winning-call winning-position))
                      ((Some call)
                       (let ((callpos (fromSome "oof" (posfun call))))
                         (if (< callpos winning-position)
                             (rec boards board (Some call) callpos)
                             (rec boards winning-board winning-call winning-position))))))))))
      (rec boards Nil None 10000)))

  (declare posfun-cheat ((Integer -> (Optional Integer))
                         -> Integer -> Integer))
  (define (posfun-cheat f x)
    (match (f x)
      ((None) 10000)
      ((Some x) x)))

  (declare unmarked-numbers-after-position ((Integer -> (Optional Integer))
                                            -> (List (List Integer))
                                            -> Integer
                                            -> (List Integer)))
  (define (unmarked-numbers-after-position posfun board pos)
    (filter
     (compose (< pos)
              (posfun-cheat posfun))
     (concat board)))

  (declare aoc4.1-solution (Unit -> (Optional Integer)))
  (define (aoc4.1-solution _)
    (match (read-aoc4-data)
      ((Tuple calls boards)
       (let ((posfun (bingo-call-position-function calls)))
         (match (earliest-winning-board posfun boards)
           ((None) None)
           ((Some (Tuple call board))
            (Some (* call (sum (unmarked-numbers-after-position posfun board (fromSome "oof" (posfun call))))))))))))
  ;; Part II

)

;;; really need minimumBy
