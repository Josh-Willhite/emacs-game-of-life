;; create a new buffer with randomly initialized gol board
(defconst size-of-board 3)
(defconst board-array (make-vector size-of-board nil))

(defun initialize-square () (if (= 1 (random 2))
                                (char-from-name "BLACK LARGE SQUARE")
                              (char-from-name "WHITE LARGE SQUARE")
                              )
       )

(defun initialize-board ()
  (dotimes (i size-of-board)
    (setf (aref board-array i) (make-vector size-of-board 0))
    (dotimes (j size-of-board)
      (setf (elt (elt board-array i) j) (initialize-square))
      (insert (elt (elt board-array i) j))
      )
    (insert "\n")
    )
  )

(defun get-neighbors (column row)
  (let ((ops `(
               [column (- row 1)]
               [(- column 1) (- row 1)]
               [(- column 1) row]
               [(- column 1) (- row 1)]
               [column (+ row 1)]
               [(+ column 1) (+ row 1)]
               [(+ column 1) row]
               [(+ column 1) (+ row 1)]
               )
             )
        (neighbors `())
        )
    (dolist (i ops)
      (let ((c (eval (elt i 0))) (r (eval (elt i 1))))
        (if (not (or (< c 0) (< r 0) (> c (- size-of-board 1)) (> r (- size-of-board 1))))
            (if (eq (length neighbors) 0)
                (setq neighbors (list `(,c ,r)))
              (push `(,c ,r) (cdr (last neighbors)))
              )
          )
        )
      )

    neighbors
    )
  )

(defun neighbor-count (column row)
  (let ((neighbors (get-neighbors column row)) (count 0))
    (dolist (n neighbors)
      (if (eq (elt (elt board-array (car (cdr n))) (car n)) (char-from-name "BLACK LARGE SQUARE"))
          (setq count (+ count 1))
        )
      )
    count
    )
  )

;; Any live cell with two or three live neighbours survives.
;; Any dead cell with three live neighbours becomes a live cell.
;; All other live cells die in the next generation. Similarly, all other dead cells stay dead.

(defun next-generation()
  (let ((new-board-array (make-vector size-of-board nil)))
    (dotimes (i size-of-board)
      (setf (aref new-board-array i) (make-vector size-of-board 0))
      (dotimes (j size-of-board)
        (let (
              (count (neighbor-count j i))
              (live (if (eq (elt (elt board-array i) j) (char-from-name "BLACK LARGE SQUARE")) t nil))
              )
          (cond
           ((and live (or (eq count 2) (eq count 3)))
            (setf (elt (elt new-board-array i) j) (char-from-name "BLACK LARGE SQUARE")))
           ((and (not live) (eq count 3))
            (setf (elt (elt new-board-array i) j) (char-from-name "BLACK LARGE SQUARE")))
           (t
            (setf (elt (elt new-board-array i) j) (char-from-name "WHITE LARGE SQUARE"))) ;; default caseA
           )
          )
        (insert (elt (elt new-board-array i) j))
        )
      (insert "\n")
      )
    (setq board-array new-board-array)
    )
  )

(switch-to-buffer (get-buffer-create "gol"))
(initialize-board)
(next-generation)
(next-generation)
