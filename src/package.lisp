(in-package :cl)
(defpackage sudoku-solver
  (:use :cl :alexandria :split-sequence :iterate)
  (:export :try-solve))
