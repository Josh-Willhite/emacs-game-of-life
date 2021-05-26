;; create a new buffer with randomly initialized gol board
(defconst size-of-board 64)
(defconst board-array (make-vector size-of-board nil))
(defconst alive (char-from-name "BLACK LARGE SQUARE"))
(defconst dead (char-from-name "WHITE LARGE SQUARE"))

(defun gol-initialize-square () (if (= 1 (random 2)) alive dead))

(defun gol-initialize-board ()
  (dotimes (i size-of-board)
    (setf (aref board-array i) (make-vector size-of-board 0))
    (dotimes (j size-of-board)
      (setf (elt (elt board-array i) j) (gol-initialize-square))
      (insert (elt (elt board-array i) j))
      )
    (insert "\n")
    )
  )

(defun gol-get-neighbors (column row)
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

(defun gol-neighbor-count (column row)
  (let ((neighbors (gol-get-neighbors column row)) (count 0))
    (dolist (n neighbors)
      (if (eq (elt (elt board-array (car (cdr n))) (car n)) alive)
          (setq count (+ count 1))
        )
      )
    count
    )
  )

(defun gol-next-generation()
  (erase-buffer)
  (let ((new-board-array (make-vector size-of-board nil)))
    (dotimes (i size-of-board)
      (setf (aref new-board-array i) (make-vector size-of-board 0))
      (dotimes (j size-of-board)
        (let (
              (count (gol-neighbor-count j i))
              (live (if (eq (elt (elt board-array i) j) alive) t nil))
              )
          ;; Any live cell with two or three live neighbours survives.
          ;; Any dead cell with three live neighbours becomes a live cell.
          ;; All other live cells die in the next generation. Similarly, all other dead cells stay dead.
          (cond
           ((and live (or (eq count 2) (eq count 3)))
            (setf (elt (elt new-board-array i) j) alive))
           ((and (not live) (eq count 3))
            (setf (elt (elt new-board-array i) j) alive))
           (t ;; default case
            (setf (elt (elt new-board-array i) j) dead))
           )
          )
        (insert (elt (elt new-board-array i) j))
        )
      (insert "\n")
      )
    (setq board-array new-board-array)
    )
  )

(defun gol-start ()
  "Start a game of life."
  (interactive)
  (switch-to-buffer (get-buffer-create "gol"))
  (gol-initialize-board)
  (setq gol-timer (run-with-timer 0.5 0.5 `gol-next-generation))
  )

(defun gol-stop ()
  "Stop a game of life."
  (interactive)
  (cancel-timer gol-timer)
  )

(provide `gol)
;;; gol.el ends here
