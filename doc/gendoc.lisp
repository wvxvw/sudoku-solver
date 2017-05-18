(in-package :sudoku-sovler.doc)

(defun generate-htdoc (path)
  (let ((dest (cl-fad:merge-pathnames-as-file path #p"./doc/html/"))) 
    (ensure-directories-exist dest)
    (atdoc:generate-html-documentation
     (list :sudoku-solver)
     dest
     :index-title "sudoku-solver package API reference"
     :heading "sudoku-solver bindings")))
