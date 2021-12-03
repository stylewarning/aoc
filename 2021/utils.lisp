;;; utils.lisp

(cl:in-package #:aoc2021-cl)

(cl:in-package #:aoc2021)

(coalton-toplevel
  (declare aoc-relative (String -> String))
  (define (aoc-relative rel-path)
    (lisp String (rel-path)
      (cl:namestring
       (asdf:system-relative-pathname ':aoc rel-path))))
 
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

  (declare string-lines (String -> (List String)))
  (define string-lines (split-string #\Newline))

  (declare string-tokens (String -> (List String)))
  (define string-tokens (split-string #\Space))

  (declare parse-int-or-fail (String -> Integer))
  (define (parse-int-or-fail s)
    "Parse the string S as an integer, or error."
    (fromSome "Failed to parse integer." (parse-int s)))

  (declare cdr ((List :a) -> (List :a)))
  (define (cdr xs)                      ; should be in stdlib
    "Return the traditional cdr of a list XS."
    (match xs
      ((Cons _ xs) xs)
      ((Nil) Nil)))



  )
