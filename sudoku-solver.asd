(in-package :cl)
(defpackage sudoku-solver-asd
  (:use :cl :asdf)
  (:export :doc-op))
(in-package :sudoku-solver-asd)

(defclass doc-op (asdf:operation) ())

(defmethod perform ((this doc-op) system)
    (format t "~%documenting system: ~s" system))

(defsystem sudoku-solver
  :version "0.1"
  :author "Oleg Sivokon <olegsivokon@gmail.com>"
  :license "MIT"
  :depends-on (:alexandria :split-sequence :iterate)
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "solver" :depends-on ("package")))))
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
          (setf (fill-pointer seq) (read-sequence seq stream)) seq)))
  :in-order-to ((test-op (load-op :sudoku-solver-test))
                (sudoku-solver-asd:doc-op (load-op :sudoku-solver-doc)))
  :perform (test-op :after (op c)
                    (funcall (intern (string '#:run!) :sudoku-solver.test)
                             :sudoku-solver.test)))

(defsystem :sudoku-solver-test
  :author "Oleg Sivokon <olegsivokon@gmail.com>"
  :description "Minimal test suite for testing sudoku-solver"
  :license "MIT"
  :depends-on (:sudoku-solver :fiveam)
  :components ((:module "tests"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "suite" :depends-on ("package"))
                         (:file "test-solver" :depends-on ("suite"))))))

(defsystem :sudoku-solver-doc
  :author "Oleg Sivokon <olegsivokon@gmail.com>"
  :description "Documentation for sudoku-solver package"
  :license "MIT"
  :depends-on (:sudoku-solver :atdoc :cl-fad)
  :components ((:module "doc"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "gendoc" :depends-on ("package"))))))
