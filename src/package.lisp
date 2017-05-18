(in-package :cl)
(defpackage sudoku-solver
  (:use :cl :alexandria :split-sequence :iterate)
  (:export :solve)
  (:documentation
   "@a[https://github.com/wvxvw/sudoku-solver]{sudoku-solver}
    Provides simple backtracking sudoku solver.
    @begin[Functions]{section}
    The library provides these functions
    @aboutfun{solve}
    To run the solver.
    @end{section}
    @begin[Example]{section}
    @begin{pre}
\(in-package :sudoku-solver)
\(defparameter *test-board*
  (make-array
   '(9 9)
   :initial-contents
   '((0 0 0 1 0 5 0 6 8)
     (0 0 0 0 0 0 7 0 1)
     (9 0 1 0 0 0 0 3 0)
     (0 0 7 0 2 6 0 0 0)
     (5 0 0 0 0 0 0 0 3)
     (0 0 0 8 7 0 4 0 0)
     (0 3 0 0 0 0 8 0 5)
     (1 0 5 0 0 0 0 0 0)
     (7 9 0 4 0 1 0 0 0))))

\(solve *test-board*) =>
#2A((4 7 3 1 9 5 2 6 8)
    (8 5 6 3 4 2 7 9 1)
    (9 2 1 6 8 7 5 3 4)
    (3 4 7 5 2 6 1 8 9)
    (5 8 2 9 1 4 6 7 3)
    (6 1 9 8 7 3 4 5 2)
    (2 3 4 7 6 9 8 1 5)
    (1 6 5 2 3 8 9 4 7)
    (7 9 8 4 5 1 3 2 6))
"))
