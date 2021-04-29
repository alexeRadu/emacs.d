(require 'vi)

;-------------------------------------------------------------------------------
; movement commands
(defun vi-goto-next-char ()
  (interactive)
  (unless (eolp)
    (forward-char)))

(defun vi-goto-prev-char ()
  (interactive)
  (unless (bolp)
    (backward-char)))

;-------------------------------------------------------------------------------
; inserting commands
(defun vi-switch-to-insert-state-before-first-non-blank-char ()
  (interactive)
  (back-to-indentation)
  (vi-switch-to-insert-state))

(defun vi-switch-to-insert-state-before-first-char ()
  (interactive)
  (beginning-of-line)
  (vi-switch-to-insert-state))

(defun vi-switch-to-insert-state-after-cursor ()
  (interactive)
  (forward-char)
  (vi-switch-to-insert-state))

(defun vi-switch-to-insert-state-at-end-of-line ()
  (interactive)
  (end-of-line)
  (vi-switch-to-insert-state))

(defun vi-switch-to-insert-state-next-line ()
  (interactive)
  (end-of-line)
  (newline)
  (vi-switch-to-insert-state))

(defun vi-switch-to-insert-state-prev-line ()
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (vi-switch-to-insert-state))

(defun vi-switch-to-normal-state ()
  (interactive)
  (vi-remove-all-minor-mode-maps)
  (push (cons 'vi-mode vi-normal-map) minor-mode-map-alist)
  (vi-mode-line-change-state "normal")
  (set-cursor-type 'box))

(defun vi-switch-to-insert-state ()
  (interactive)
  (vi-remove-all-minor-mode-maps)
  (push (const 'vi-mode vi-insert-map) minor-mode-map-alist)
  (vi-mode-line-change-state "insert")
  (set-cursor-type 'bar))


;; left-right movement commands
(define-key vi-normal-map "l" 'vi-goto-next-char)
(define-key vi-normal-map "h" 'vi-goto-prev-char)
(define-key vi-normal-map "0" 'move-beginning-of-line)
(define-key vi-normal-map "^" 'back-to-indentation)
(define-key vi-normal-map "$" 'move-end-of-line)

;; word-movement commands
(define-key vi-normal-map "w" 'forward-to-word)
(define-key vi-normal-map "e" 'forward-word)
(define-key vi-normal-map "b" 'backward-word)

;; line movement commands
(define-key vi-normal-map "j" 'next-line)
(define-key vi-normal-map "k" 'previous-line)
(define-key vi-normal-map "G" 'end-of-buffer)
(define-key vi-normal-map "gg" 'beginning-of-buffer)

;; switch to insert state commands
(define-key vi-normal-map "i" 'vi-switch-to-insert-state)
(define-key vi-normal-map "I" 'vi-switch-to-insert-state-before-first-non-blank-char)
(define-key vi-normal-map "gi" 'vi-switch-to-insert-state-before-first-char)
(define-key vi-normal-map "a" 'vi-switch-to-insert-state-after-cursor)
(define-key vi-normal-map "A" 'vi-switch-to-insert-state-at-end-of-line)
(define-key vi-normal-map "o" 'vi-switch-to-insert-state-next-line)
(define-key vi-normal-map "O" 'vi-switch-to-insert-state-prev-line)

(define-key vi-normal-map "x" 'delete-char)
(define-key vi-normal-map "dd" 'kill-whole-line)

;; insert mode mappings
(define-key vi-insert-map [escape] 'vi-switch-to-normal-state)

(provide 'keys)
