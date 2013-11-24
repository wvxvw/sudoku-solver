(in-package :sudoku-solver)

(defparameter *score-board* (make-array '(9 9) :initial-element 0))

(defstruct solving-state next position options board)

(defun board->hash (board)
  (iter
    (with result := 0)
    (for row :from 0 :below 9)
    (iter (for column :from 0 :below 9)
          (setf result (+ (* result 10) (aref board row column))))
    (finally (return result))))

(defun eliminate-horizontal (board row column options)
  (declare (ignore column))
  (iter
    (for i :from 0 :below 9)
    (setf options (delete (aref board row i) options)))
  options)

(defun eliminate-vertical (board row column options)
  (declare (ignore row))
  (iter
    (for i :from 0 :below 9)
    (setf options (delete (aref board i column) options)))
  options)

(defun eliminate-square (board row column options)
  (setf row (- row (mod row 3)) column (- column (mod column 3)))
  (iter
    (for i :from row :below (+ 3 row))
    (iter
      (for j :from column :below (+ 3 column))
      (setf options (delete (aref board i j) options))))
  options)

(defun eliminate-ltrb (board row column options)
  (declare (ignore row column))
  (iter
    (for i :from 0 :below 9)
    (setf options (delete (aref board i i) options)))
  options)

(defun eliminate-rtlb (board row column options)
  (declare (ignore row column))
  (iter
    (for i :from 0 :below 9)
    (setf options (delete (aref board (- 8 i) i) options)))
  options)

(defun best-options (cell board)
  (iter
    (with row := (second cell))
    (with column := (third cell))
    (initially (setq result (list 0 1 2 3 4 5 6 7 8 9)))
    (for func :in '(eliminate-horizontal
                    eliminate-vertical
                    eliminate-square
                    ;; eliminate-ltrb eliminate-rtlb
                    ))
    (for result :next (funcall func board row column result))
    ;; (format t "~&options so far: ~s" result)
    (while result)
    (finally (return result))))

(defun vertical-up (score-board i)
  (iter
    (for j :from 0 :below 9)
    (incf (aref score-board i j))))

(defun horizontal-up (score-board j)
  (iter
    (for i :from 0 :below 9)
    (incf (aref score-board i j))))

(defun square-up (score-board i j)
  (setf i (- i (mod i 3)) j (- j (mod j 3)))
  (iter
    (for ii :from i :below (+ i 3))
    (iter
      (for jj :from j :below (+ j 3))
      (incf (aref score-board ii jj)))))

(defun diagonal-up-ltr (score-board)
  (iter
    (for i :from 0 :below 9)
    (incf (aref score-board i i))))

(defun diagonal-up-rtl (score-board)
  (iter
    (for i :from 0 :below 9)
    (incf (aref score-board (- 8 i) i))))

(defun assign-scores (board score-board)
  (iter
    (for i :from 0 :below 9)
    (iter
      (for j :from 0 :below 9)
      (when (> (aref board i j) 0)
        (vertical-up score-board i)
        (horizontal-up score-board j)
        (square-up score-board i j)
        ;; (when (= i j) (diagonal-up-ltr score-board))
        ;; (when (= (- 8 i) j) (diagonal-up-rtl score-board))
        ))))

(defun best-pos (board score-board)
  (iter
    (with best-score := 0)
    (with result := nil)
    (for i :from 0 :below 9)
    (iter
      (for j :from 0 :below 9)
      (for score := (aref score-board i j))
      (when (and (= (aref board i j) 0) (> score best-score))
        (setf best-score score result (list score i j))))
    (finally (return result))))

(defun whipe-board (board value)
  (iter
    (for i :from 0 :below 9)
    (iter
      (for j :from 0 :below 9)
      (setf (aref board i j) value))) board)

(defun place-on-board (board cell fit)
  (iter
    (with new-board := (make-array '(9 9) :initial-element 0))
    (for i :from 0 :below 9)
    (iter
      (for j :from 0 :below 9)
      (setf (aref new-board i j) (aref board i j)))
    (finally
     (return
       (progn
         (setf (aref new-board (second cell) (third cell)) fit)
         new-board)))))

(defun try-solve (state)
  (let ((board (solving-state-board state)))
    (assign-scores board (whipe-board *score-board* 0))
    (let* ((pos (or (solving-state-position state)
                      (best-pos board *score-board*)))
           (options (or (solving-state-options state)
                        (best-options pos board)))
           (fit (car options)))
      (when fit
        (setf (solving-state-position state) pos
              (solving-state-options state) (cdr options)
              (solving-state-board state)
              (place-on-board board pos fit))
        state))))

(defun number-of-steps (board)
  (iter
    (for i :from 0 :below 9)
    (summing
     (iter
       (for j :from 0 :below 9)
       (when (= 0 (aref board i j))
         (summing 1))))))

(defun solve (board)
  "Employs best-first match algorithm for solving the sudoku 9x9
puzzle. The algorithm works as follows:
1. Assign cost values to every cell on the board, the cost is
   calculated using this formula: number of non-empty cells in
   the row plus number of non-empty cells in the column plus
   number of non-empty cells in the sub-board (a 3x3 matrix,
   one of the 9 parts together forming the 9x9 board)
2. Find the cell such that it has the highest score and is empty.
3. If there is no such cell, the algorithm finished, return the
   solved board.
4. Find all possible numbers that could be used to fill the cell.
5. If there isn't such number, go to step 7.
6. Place the number on the board. Record the state of the algorithm.
   The state must store all other possible options for filling
   the empty cell. Go to step 1.
7. Retrieve the last state of the algorithm, if there is no
   previous state, the game has no solutions. If the state has
   one or more options to fill the empty cell, restore this state
   and go to step 6. Otherwise, repeat step 7."
  (assign-scores board (whipe-board *score-board* 0))
  (iter
    (with step := 0)
    (with pos := (best-pos board *score-board*))
    (with options := nil)
    (with previous-state :=
          (make-solving-state
           :next nil
           :position pos
           :options (best-options pos board)
           :board board))
    
    (while (< step (number-of-steps board)))
    
    (for next-state :=
         (make-solving-state
          :next previous-state
          :position nil
          :options options
          :board (solving-state-board previous-state)))
    (setf options nil)
    (for attempt := (try-solve next-state))
    (if attempt
        (setf step (1+ step) previous-state next-state)
        (iter
          (until (solving-state-options next-state))
          (setf next-state (solving-state-next next-state)
                previous-state (solving-state-next next-state)
                step (1- step))
          (unless next-state (return "no solutions"))
          (finally (setf options (solving-state-options next-state)))))
    (finally (return (solving-state-board next-state)))))
