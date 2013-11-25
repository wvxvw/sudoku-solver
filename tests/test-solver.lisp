(in-package :sudoku-solver.test)

(in-suite :sudoku-solver.test)

(def-suite test-suite
    :description "Minimal testing suite for sudoku-solver project.")

(defparameter *test-board*
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

(test test-solve-simple
  "Tries to solve a simple (classic) sudoku board"
  (is
   (iter
     (with result := (sudoku-solver:solve *test-board*))
     (for i :from 0 :below 9)
     (iter
       (for j :from 0 :below 9)
       (when (zerop (aref result i j))
         (return)))
     (finally (return t)))))
