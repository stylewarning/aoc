;;; utils.lisp

(cl:in-package #:aoc2021-cl)

(defun optionalify (x)
  (if (null x)
      coalton-library:None
      (coalton-library:Some x)))

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

  (declare string-length (String -> Integer))
  (define (string-length str)
    "The length of a string STR."
    (lisp Integer (str)
      (cl:length str)))

  (define (conjoin f g x)
    (and (f x) (g x)))

  (define (disjoin f g x)
    (or (f x) (g x)))

  (define (complement f x)
    (not (f x)))

  (define (list-foreach f l)
    (match l
      ((Nil) Unit)
      ((Cons x xs) (progn
                     (f x)
                     (list-foreach f xs)))))

  (declare substring (String -> Integer -> Integer -> String))
  (define (substring str start end)
    (let ((real-start (max 0 (min start end)))
          (real-end (min (string-length str) (max start end))))
      (lisp String (real-start real-end str)
        (cl:subseq str real-start real-end))))

  (declare split-string (Char -> String -> (List String)))
  (define (split-string c s)
    "Split the string S into substrings that are delineated by C."
    (lisp (List String) (c s)
      (cl:values (split-sequence:split-sequence c s))))

  (declare string-lines (String -> (List String)))
  (define string-lines (split-string #\Newline))

  (declare string-tokens (String -> (List String)))
  (define string-tokens (split-string #\Space))

  (declare string-search-at (Integer -> String -> String -> (Optional Integer)))
  (define (string-search-at start needle haystack)
    (lisp (Optional Integer) (needle start haystack)
      (aoc2021-cl::optionalify
       (cl:search needle haystack :start2 start :test #'cl:char=))))

  (declare string-search (String -> String -> (Optional Integer)))
  (define string-search (string-search-at 0))

  (declare wedge-string (String -> String -> (List String)))
  (define (wedge-string wedge string)
    "Like STRING-SPLIT, but works with strings and not characters."
    (let ((wedge-size (string-length wedge))
          (string-size (string-length string)))
      (let ((rec (fn (start found-strings)
                   (match (string-search-at start wedge string)
                     ((None)
                      (if (== start (string-length string))
                          (reverse found-strings)
                          (reverse (Cons (substring string start string-size)
                                         found-strings))))
                     ((Some location)
                      (rec (+ location wedge-size)
                           (Cons (substring string start location)
                                 found-strings)))))))
        (rec 0 Nil))))

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

  (declare nth (Integer -> (List :t) -> :t))
  (define (nth n l)
    (fromSome "There is no NTH" (index l n)))

  (declare singleton-list? ((List :t) -> Boolean))
  (define (singleton-list? x)
    (match x
      ((Nil)          False)
      ((Cons _ (Nil)) True)
      (_              False))))
