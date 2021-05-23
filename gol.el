;; create a new buffer with randomly initialized gol board

(defvar size-of-board 3)
(defvar board-array (make-vector size-of-board nil))

(defun initialize-square ()
  (if (= 1 (random 2))
      (char-from-name "BLACK SQUARE")
    (char-from-name "WHITE SQUARE")
    )
  )

(dotimes (i size-of-board)
  (setf (aref board-array i) (make-vector size-of-board 0))
  (dotimes (j size-of-board)
     (setf (elt (elt board-array i) j) (initialize-square))
  )
)
(switch-to-buffer (get-buffer-create "gol"))

(dotimes (i size-of-board)
  (dotimes (j size-of-board)
     (insert (elt (elt board-array i) j))
     )
  (insert "\n")
)

;; (print board-array)
