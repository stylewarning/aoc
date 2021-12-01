;;;; aoc.asd

(asdf:defsystem #:aoc
  :description "Advent of Code"
  :author "Robert Smith <robert@stylewarning.com>"
  :license "MIT"
  :depends-on (#:coalton
               #:alexandria
               #:uiop)
  :serial t
  :components ((:module "2021"
                :serial t
                :components ((:file "package")
                             (:file "1")))))
