(in-package :sudoku-solver.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (get-test :sudoku-solver)
    (def-suite :sudoku-solver)))

(def-suite :sudoku-solver.test :in :sudoku-solver)
