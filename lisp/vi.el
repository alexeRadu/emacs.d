(defvar vi-mode nil)
(defvar vi-unload-function nil)

;-------------------------------------------------------------------------------
; mapping functions and variables
(defvar vi-map-alist ())
(defvar vi-insert-map (make-keymap))
(defvar vi-normal-map (make-keymap))

;; append maps to the vi-map-alist
(push (cons "normal" 'vi-normal-map) vi-map-alist)
(push (cons "insert" 'vi-insert-map) vi-map-alist)

(defun vi-add-minor-mode-map (mode)
  (let ((map (eval (cdr (assoc mode vi-map-alist)))))
    (push (cons 'vi-mode map) minor-mode-map-alist)))

(defun vi-remove-all-minor-mode-maps ()
  (let ((rm-list nil))
    (dolist (al minor-mode-map-alist)
      (when (eq (car al) 'vi-mode)
       (push al rm-list)))
    (dolist (al rm-list)
      (setq minor-mode-map-alist (delq al minor-mode-map-alist)))))

;-------------------------------------------------------------------------------
; modeline functions and variables
(defvar vi-mode-line-state nil)

(defun vi-init-modeline ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unless (member 'vi-mode-line-state mode-line-format)
	(setq mode-line-format
	      (append (list (car mode-line-format) 'vi-mode-line-state)
		      (cdr mode-line-format)))))))

(defun vi-clear-modeline ()
  (when (member 'vi-mode-line-state mode-line-format)
    (setq mode-line-format
	  (cons (car mode-line-format) (cdr (cdr mode-line-format))))))

(defun vi-mode-line-change-state (name)
  (unless (eq vi-mode-line-state name)
    (setq vi-mode-line-state name)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(force-mode-line-update)))))

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


(defun vi-switch-to-normal-state ()
  (interactive)
  (vi-remove-all-minor-mode-maps)
  (vi-add-minor-mode-map "normal")
  (vi-mode-line-change-state "normal"))

(defun vi-switch-to-insert-state ()
  (interactive)
  (vi-remove-all-minor-mode-maps)
  (vi-add-minor-mode-map "insert")
  (vi-mode-line-change-state "insert"))


(defun vi-mode (&optional arg)
  (interactive (list (or current-prefix-arg 'toggle)))
  (let ((enable
	 (if (eq arg 'toggle)
	     (not vi-mode)
	   (> (prefix-numeric-value arg) 0))))
    (if enable
	(vi-mode-enable)
      (vi-mode-disable))))

(defun vi-mode-enable ()
  (unless vi-mode
    (setq vi-mode t)
    (unless (member 'vi-mode-disable minibuffer-setup-hook)
      (add-hook 'minibuffer-setup-hook 'vi-mode-disable))
    (vi-add-minor-mode-map "normal")
    (vi-init-modeline)
    (vi-mode-line-change-state "normal")))

(defun vi-mode-disable ()
  (when vi-mode
    (setq vi-mode nil)
    (unless (member 'vi-mode-enable minibuffer-exit-hook)
      (add-hook 'minibuffer-exit-hook 'vi-mode-enable))
    (vi-clear-modeline)
    (vi-remove-all-minor-mode-maps)))

(defun vi-unload-function ()
  (vi-clear-modeline)
  (vi-remove-all-minor-mode-maps))

(provide 'vi)

(defun vi-show-modeline ()
  (interactive)
  (print mode-line-format))
