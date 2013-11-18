(in-package :cl)
(defpackage sudoku-solver-asd (:use :cl :asdf))
(in-package :sudoku-solver-asd)

(defsystem sudoku-solver
  :version "0.1"
  :author "Oleg Sivokon"
  :license "MIT"
  :depends-on (:alexandria :split-sequence :iterate)
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "solver"))))
  :description "A toy project for solving Sudoku 9x9 puzzles"
  :long-description
  #.(with-open-file
        (stream (merge-pathnames
                 #p"README.org" (or *load-pathname* *compile-file-pathname*))
                :if-does-not-exist nil :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))
