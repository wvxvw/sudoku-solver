(in-package :sudoku-solver)

(defparameter *test-board* (make-array '(9 9) :initial-element 0))

(defparameter *score-board* (make-array '(9 9) :initial-element 0))

(defparameter *max-restarts* 9)

(defstruct solving-state next restarts board)

(defun board->hash (board)
  (iter
    (with result := 0)
    (for row :from 0 :below 9)
    (iter (for column :from 0 :below 9)
          (setf result (+ (* result 10) (aref board row column))))
    (finally (return result))))

(defun eliminate-horizontal (board row options)
  (iter
    (for i :from 0 :below 9)
    (setf options (delete (aref board row i) options)))
  options)

(defun eliminate-vertical (board column options)
  (iter
    (for i :from 0 :below 9)
    (setf options (delete (aref board i column) options)))
  options)

(defun eliminate-square (board row column options)
  (setf row (floor row 3) column (floor column 3))
  (iter
    (for i :from row :below (+ 3 row))
    (iter
      (for j :from column :below (+ 3 column))
      (setf options (delete (aref board i j) options))))
  options)

(defun best-options (cell board)
  (iter
    (with result := (list 0 1 2 3 4 5 6 7 8 9))
    (for (x . y) :in-vector group)
    (setf result (delete (aref board x y) result))
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
  (setf i (floor i 3) j (floor i j))
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
      (when (> (aref board i j) -1)
        (vertical-up score-board i)
        (horizontal-up score-board j)
        (square-up score-board i j)
        (when (= i j) (diagonal-up-ltr score-board))
        (when (= (- 8 i) j) (diagonal-up-rtl score-board))))))

(defun best-cells (board score-board)
  (sort 
   (iter
     (for i :from 0 :below 9)
     (nconcing
      (iter
        (for j :from 0 :below 9)
        (when (= (aref board i j) -1)
          (collect (list (aref score-board i j) i j))))))
   #'> :key #'car))

(defun whipe-board (board value)
  (iter
    (for i :from 0 :below 9)
    (iter
      (for j :from 0 :below 9)
      (setf (aref board i j) value))) board)

(defun place-on-board (board cell fit)
  (iter
    (with new-board := (make-array '(9 9) :initial-element -1))
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
    (let ((cells (best-cells board *score-board*)))
      (let ((fit (try-fit (car cells) board (best-options (car cell) board))))
        (when fit
        (make-solving-state
         :next state
         :restarts (subseq (cdr cells) 0 *max-restarts*)
         :board (place-on-board board (car cells) fit)))))))
