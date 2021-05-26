;;; gol.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Josh Willhite
;;
;; Author: Josh Willhite <https://github.com/josh-willhite>
;; Maintainer: Josh Willhite <me@joshwillhite.com>
;; Created: May 25, 2021
;; Modified: May 25, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/josh-willhite/emacs-game-of-life
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Implementation of Conway's game of life for fun and learning.
;;
;;  ref: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
;;
;;; Code:

(defconst gol-size-of-board 64)
(defconst gol-board-array (make-vector gol-size-of-board nil))
(defconst gol-alive (char-from-name "BLACK LARGE SQUARE"))
(defconst gol-dead (char-from-name "WHITE LARGE SQUARE"))
(defconst gol-timer nil)

(defun gol-initialize-square ()
  "Randomly initialize a square to gol-alive or gol-dead."
  (if (= 1 (random 2)) gol-alive gol-dead)
  )

(defun gol-initialize-board ()
  "Initialize all squares on board."
  (dotimes (i gol-size-of-board)
    (setf (aref gol-board-array i) (make-vector gol-size-of-board 0))
    (dotimes (j gol-size-of-board)
      (setf (elt (elt gol-board-array i) j) (gol-initialize-square))
      (insert (elt (elt gol-board-array i) j))
      )
    (insert "\n")
    )
  )

(defun gol-get-neighbors (column row)
  "Get coordinates of squares adjacent to COLUMN, ROW."
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
        (if (not (or (< c 0) (< r 0) (> c (- gol-size-of-board 1)) (> r (- gol-size-of-board 1))))
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
  "Count the number of live cells adjacent to COLUMN, ROW."
  (let ((neighbors (gol-get-neighbors column row)) (count 0))
    (dolist (n neighbors)
      (if (eq (elt (elt gol-board-array (car (cdr n))) (car n)) gol-alive)
          (setq count (+ count 1))
        )
      )
    count
    )
  )

(defun gol-next-generation()
  "Determine next state of the board base don previous state."
  (erase-buffer)
  (let ((new-board-array (make-vector gol-size-of-board nil)))
    (dotimes (i gol-size-of-board)
      (setf (aref new-board-array i) (make-vector gol-size-of-board 0))
      (dotimes (j gol-size-of-board)
        (let (
              (count (gol-neighbor-count j i))
              (live (if (eq (elt (elt gol-board-array i) j) gol-alive) t nil))
              )
          (cond
           ;; Any live cell with two or three live neighbours survives.
           ((and live (or (eq count 2) (eq count 3)))
            (setf (elt (elt new-board-array i) j) gol-alive))
           ;; Any gol-dead cell with three live neighbours becomes a live cell.
           ((and (not live) (eq count 3))
            (setf (elt (elt new-board-array i) j) gol-alive))
           ;; All other live cells die in the next generation. Similarly, all other gol-dead cells stay gol-dead.
           (t
            (setf (elt (elt new-board-array i) j) gol-dead))
           )
          )
        (insert (elt (elt new-board-array i) j))
        )
      (insert "\n")
      )
    (setq gol-board-array new-board-array)
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
