(defconst xo-buffer-name "*tic-tac-toe*")
(defvar xo-buffer nil)
(defvar xo-position '(2 . 2))
(defvar xo-board-start-point nil)
(defvar xo-player-one-region '(nil . nil))
(defvar xo-player-two-region '(nil . nil))
(defvar xo-message-point nil)
(defvar xo-board '(nil nil nil nil nil nil nil nil nil))
(defvar xo-win-conditions '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 4 7) (2 5 8) (0 4 8) (2 4 6)))
(defvar xo-game-finished nil)
(defvar xo-player 1)

;; helper function
(defun xo-peek-props ()
  (interactive)
  (message (format "props: %s" (text-properties-at (point)))))

(defun xo-get-point (x y)
  (+ xo-board-start-point 13 (* 28 x) 3 (* y 4)))

(defun xo-goto-xy (x y)
  (unless (or (> x 3) (> y 3))
    (goto-char (xo-get-point x y))
    (setq xo-position (cons x y))))

(defun xo-prev-line ()
  (interactive)
  (let ((line (car xo-position))
	(col (cdr xo-position)))
    (unless (eq line 0)
      (setq line (- line 1))
      (xo-goto-xy line col))))

(defun xo-next-line ()
  (interactive)
  (let ((line (car xo-position))
	(col (cdr xo-position)))
    (unless (>= line 2)
      (setq line (+ line 1))
      (xo-goto-xy line col))))

(defun xo-prev-col ()
  (interactive)
  (let ((line (car xo-position))
	(col (cdr xo-position)))
    (unless (eq col 0)
      (setq col (- col 1))
      (xo-goto-xy line col))))

(defun xo-next-col ()
  (interactive)
  (let ((line (car xo-position))
	(col (cdr xo-position)))
    (unless (>= col 2)
      (setq col (+ col 1))
      (xo-goto-xy line col))))

(defun xo-select-player-region (player)
  (if (eq player 1)
      xo-player-one-region
    xo-player-two-region))

(defun xo-highlight-player (player)
  (let* ((player-region (xo-select-player-region player))
	 (start (car player-region))
	 (end (cdr player-region)))
    (put-text-property start end 'face '(:background "red"))))

(defun xo-clear-player-highlight (player)
  (let* ((player-region (xo-select-player-region player))
	  (start (car player-region))
	  (end (cdr player-region)))
    (remove-text-properties start end '(face nil))))

(defun xo-switch-player ()
  (xo-clear-player-highlight xo-player)
  (if (eq xo-player 1)
	(setq xo-player 2)
    (setq xo-player 1))
  (xo-highlight-player xo-player))

(defun xo-get-player-mark ()
  (if (eq xo-player 1)
      "x"
    "o"))

(defun xo-set-mark ()
  (interactive)
  (let ((game-mark (xo-get-player-mark))
	(line (car xo-position))
	(col (cdr xo-position)))
    (unless (or xo-game-finished (nth (+ col (* line 3)) xo-board))
      (setcar (nthcdr (+ col (* line 3)) xo-board) game-mark)
      (insert-and-inherit game-mark)
      (delete-char 1)
      (backward-char)
      (if (xo-win game-mark xo-win-conditions)
	  (progn
	    (xo-message (format "Player %d won!" xo-player))
	    (setq xo-game-finished t))
	(if (xo-last-move)
	    (progn
	      (xo-message "Finish: draw!")
	      (setq xo-game-finished t))
	  (xo-switch-player))))))

(defun xo-draw-board ()
  (with-current-buffer (get-buffer-create xo-buffer)
    (setq xo-board-start-point (point))
    (insert "+---+---+---+\n")
    (insert "|   |   |   |\n")
    (insert "+---+---+---+\n")
    (insert "|   |   |   |\n")
    (insert "+---+---+---+\n")
    (insert "|   |   |   |\n")
    (insert "+---+---+---+\n\n")))

(defun xo-clear-board ()
  (dolist (x '(0 1 2))
    (dolist (y '(0 1 2))
      (xo-goto-xy x y)
      (setcar (nthcdr (+ y (* x 3)) xo-board) nil)
      (insert-and-inherit " ")
      (delete-char 1)
      (backward-char))))

(defun xo-new-game ()
  (interactive)
  (xo-clear-board)
  (xo-clear-message)
  (xo-goto-xy 2 2)
  (setq xo-game-finished nil)
  (when (eq xo-player 2)
    (xo-switch-player)))

(defun xo-put-board-text-property-at (x y prop value)
  (let* ((start (xo-get-point x y))
	(end (+ start 1)))
    (put-text-property start end prop value)))

(defun xo-set-board-properties ()
  (with-current-buffer xo-buffer
    (dolist (x '(0 1 2))
      (dolist (y '(0 1 2))
	(xo-put-board-text-property-at x y 'inhibit-read-only 't)
	(xo-put-board-text-property-at x y 'front-sticky 't)))))

(defun xo-message (msg)
  (xo-clear-message)
  (save-excursion
    (goto-char xo-message-point)
    (insert-and-inherit msg)))

(defun xo-clear-message ()
  (save-excursion
    (goto-char xo-message-point)
    (delete-region xo-message-point (- (point-max) 1))))

(defun xo-last-move ()
  (not
   (catch 'found-empty-pos
     (dolist (x xo-board)
       (unless x
	 (throw 'found-empty-pos t))))))

(defun xo-full-line (current-mark line)
  (not
   (catch 'found-difference
     (dolist (x line)
       (unless (eq (nth x xo-board) current-mark)
	 (throw 'found-difference t))))))

(defun xo-win (current-mark lines)
  (catch 'found-line
    (dolist (line lines)
      (when (xo-full-line current-mark line)
	(throw 'found-line t)))))

(defun xo-game ()
  (interactive)
  (if xo-buffer
      (switch-to-buffer xo-buffer)
    (setq xo-buffer (get-buffer-create xo-buffer-name))
    (switch-to-buffer xo-buffer)
    (insert "Tick Tack Toe\n")
    (insert "-------------\n\n")
    (setcar xo-player-one-region (point))
    (insert "Player one")
    (setcdr xo-player-one-region (point))
    (put-text-property (car xo-player-one-region) (cdr xo-player-one-region) 'inhibit-read-only 't)
    (insert " - ")
    (setcar xo-player-two-region (point))
    (insert "Player two")
    (setcdr xo-player-two-region (point))
    (put-text-property (car xo-player-two-region) (cdr xo-player-two-region) 'inhibit-read-only 't)
    (insert "\n\n")
    (xo-highlight-player 1)
    (xo-draw-board)
    (xo-set-board-properties)
    (insert "\n")
    (setq xo-message-point (point))
    (insert " ")
    (put-text-property xo-message-point (+ 1 xo-message-point) 'inhibit-read-only 't)
    (put-text-property xo-message-point (+ 1 xo-message-point) 'front-sticky 't)
    (xo-goto-xy (car xo-position) (cdr xo-position))
    (xo-mode)))

(defun xo-end-game ()
  (interactive)
  (let ((buffer (get-buffer-create xo-buffer-name)))
    (dolist (window (get-buffer-window-list buffer))
      (switch-to-prev-buffer window t))
    (kill-buffer buffer)
    (setq xo-buffer nil)))

(define-derived-mode xo-mode special-mode
  "xo"
  (define-key xo-mode-map "k" 'xo-prev-line)
  (define-key xo-mode-map "j" 'xo-next-line)
  (define-key xo-mode-map "h" 'xo-prev-col)
  (define-key xo-mode-map "l" 'xo-next-col)
  (define-key xo-mode-map "i" 'xo-set-mark)
  (define-key xo-mode-map "n" 'xo-new-game)
  (define-key xo-mode-map "p" 'xo-peek-props))

(provide 'xo)
