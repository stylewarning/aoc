;;;; 5.lisp
;;;;
;;;; https://adventofcode.com/2021/day/5

(cl:in-package #:aoc2021-cl)

;;; Look ma, no Lisp!

(cl:in-package #:aoc2021)

(coalton-toplevel
  ;; Part I

  (define-type Point2d
    (Point2d Integer Integer))

  (define (point->list p)
    (match p
      ((Point2d x y) (make-list x y))))

  (define-type Line2d
    (Line2d Point2d Point2d))

  (define (make-line x1 y1 x2 y2)
    (Line2d (Point2d x1 y1)
            (Point2d x2 y2)))

    (define (horizontal-line line)
    (match line
      ((Line2d (Point2d _ y1) (Point2d _ y2))
       (== y1 y2))))

  (define (vertical-line line)
    (match line
      ((Line2d (Point2d x1 _) (Point2d x2 _))
       (== x1 x2))))

  (define (rasterize l)
    (match l
      ((Line2d (Point2d x1 y1)
               (Point2d x2 y2))
       (cond
         ((vertical-line   l) (map (Point2d x1)        (range y1 y2)))
         ((horizontal-line l) (map ((flip Point2d) y1) (range x1 x2)))
         (True                (zipWith Point2d
                                       (range x1 x2)
                                       (range y1 y2)))))))



  (define aoc5-input-file (aoc-relative "2021/5/5.input"))

  (define (read-aoc5-data _)
    (let ((to-points (fn (xyxy)
                       (match xyxy
                         ((Cons x1 (Cons y1 (Cons x2 (Cons y2 (Nil)))))
                          (make-line x1 y1 x2 y2))
                         (_ (error "Invalid point pair!"))))))
      (pipe
       (read-file-into-string aoc5-input-file)
       (fromSome "oof")
       string-lines
       (filter (/= ""))
       (map (wedge-string " -> "))
       (map (concatMap (split-string #\,)))
       (map (map parse-int-or-fail))
       (map to-points))))

  (declare make-occ-table (Unit -> (Hashtable (List Integer) Integer)))
  (define (make-occ-table _)
    (make-hashtable))

  (declare hashtable-inc (:key -> Integer -> (Hashtable :key Integer) -> Unit))
  (define (hashtable-inc k delta table)
    (match (hashtable-get k table)
      ((Some v) (hashtable-set k (+ v delta) table))
      ((None)   (hashtable-set k delta table))))

  (declare add-point-to-table ((Hashtable (List Integer) Integer) -> Point2d -> Unit))
  (define (add-point-to-table tab pt)
    (hashtable-inc (point->list pt) 1 tab))

  (declare add-line-to-table ((Hashtable (List Integer) Integer) -> Line2d -> Unit))
  (define (add-line-to-table tab line)
    (list-foreach (add-point-to-table tab) (rasterize line)))

  (define (count-intersections lines)
    (let ((tab (make-occ-table)))
      (progn
        (list-foreach (add-line-to-table tab) lines)
        (match tab
          ((Hashtable tab)
           (lisp Integer (tab)
             (cl:loop
                :for v :being :the :hash-values :of tab
                :when (cl:> v 1)
                  :count 1)))))))

  (define (aoc5.1-solution _)
    (pipe
     (read-aoc5-data)
     (filter (disjoin horizontal-line vertical-line))
     count-intersections))


  ;; Part II

  (define (aoc5.2-solution _)
    (pipe
     (read-aoc5-data)
     count-intersections)))
