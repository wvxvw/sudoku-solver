(in-package :sudoku-solver.test)

(in-suite :sudoku-solver.test)

(def-suite test-suite
    :description "Minimal testing suite for sudoku-solver project.")

(defparameter *simple-board*
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

(defparameter *medium-board*
  (make-array
   '(9 9)
   :initial-contents
   '((0 0 0 2 6 0 7 0 1)
     (6 8 0 0 7 0 0 9 0)
     (1 9 0 0 0 4 5 0 0)
     (8 2 0 1 0 0 0 4 0)
     (0 0 4 6 0 2 9 0 0)
     (0 5 0 0 0 3 0 2 8)
     (0 0 9 3 0 0 0 7 4)
     (0 4 0 3 0 0 0 3 6)
     (7 0 3 0 1 8 0 0 0))))

(test test-solve-simple
  "Tries to solve a simple (classic) sudoku board"
  (is
   (iter
     (with result := (sudoku-solver:solve *simple-board*))
     (for i :from 0 :below 9)
     (iter
       (for j :from 0 :below 9)
       (when (zerop (aref result i j))
         (return)))
     (finally (return t)))))

(test test-solve-medium
  "Tries to solve a medium
   (Arizona Daily Wildcat: Wednesday, Jan 18th 2006) sudoku board"
  (is
   (iter
     (with result := (sudoku-solver:solve *medium-board*))
     (for i :from 0 :below 9)
     (iter
       (for j :from 0 :below 9)
       (when (zerop (aref result i j))
         (return)))
     (finally (return t)))))
