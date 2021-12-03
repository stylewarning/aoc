;;;; aoc.asd

(asdf:defsystem #:aoc
  :description "Advent of Code"
  :author "Robert Smith <robert@stylewarning.com>"
  :license "MIT"
  :depends-on (#:coalton
               #:alexandria
               #:split-sequence
               #:uiop)
  :serial t
  :components ((:module "2021"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:module "1"
                              :serial t
                              :components ((:static-file "1.input")
                                           (:file "1")))
                             (:module "2"
                              :serial t
                              :components ((:static-file "2.input")
                                           (:file "2")))
                             (:module "3"
                              :serial t
                              :components ((:static-file "3.input")
                                           (:file "3")))))))
