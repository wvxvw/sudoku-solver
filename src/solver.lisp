(in-package :sudoku-solver)

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

(defparameter *score-board* (make-array '(9 9) :initial-element 0))

(defstruct solving-state next restarts options board)

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

(defun best-cells (board score-board)
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

(defun try-fit (where board options)
  (iter
    (with row := (second where))
    (with column := (third where))
    (with top := (- row (mod row 3)))
    (with left := (- column (mod column 3)))
    (for i :from 0 :below 9)
    (while options)
    (iter
      (for option :in options)
      (when (or (= (aref board row i) option)
                (= (aref board i column) option)
                (= (aref board (+ top (mod i 3))
                         (+ left (floor i 3))) option))
        (setf options (delete option options))))
    (finally (return (car options)))))

(defun try-solve (state)
  (let ((board (solving-state-board state)))
    (assign-scores board (whipe-board *score-board* 0))
    (format t "~&options in: ~s" (solving-state-options state))
    (let* ((cells (or (solving-state-restarts state)
                      (best-cells board *score-board*)))
           (options (or (solving-state-options state)
                        (best-options cells board)))
           ;; (fit (try-fit cells board options))
           (fit (car options)))
      (format t "~&fitting: ~s, options: ~s, fit: ~d, board:~% ~s"
              cells options fit board)
      (when fit
        (setf (solving-state-restarts state) cells
              (solving-state-options state) (cdr options)
              (solving-state-board state)
              (place-on-board board cells fit))
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
  (assign-scores board (whipe-board *score-board* 0))
  (iter
    (with step := 0)
    (with cells := (best-cells board *score-board*))
    (with previous-state :=
          (make-solving-state
           :next nil
           :restarts cells
           :options (best-options cells board)
           :board board))
    
    (while (< step (number-of-steps board)))
    
    (for next-state :=
         (make-solving-state
          :next previous-state
          :restarts nil
          :options nil
          :board (solving-state-board previous-state)))
    (for attempt := (try-solve next-state))
    (format t "~&----------step: ~d" step)
    (if attempt
        (setf step (1+ step) previous-state next-state)
        (iter
          (until (solving-state-options next-state))
          (setf next-state (solving-state-next next-state)
                previous-state (solving-state-next next-state)
                step (1- step))
          (unless next-state (return "no solutions"))
          (summing 1 :into steps)
          (finally
           (let* ((board (solving-state-board next-state))
                  (cells (solving-state-restarts next-state))
                  (x (second cells))
                  (y (third cells)))
             (format t "~&stepped back: ~d" steps)
             (setf (aref board x y) 0)))))))
