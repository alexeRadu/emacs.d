(defvar vi-mode nil)
(defvar vi-insert-map nil)
(defvar vi-normal-map nil)
(defvar vi-mode-line-state nil)
(defvar vi-unload-function nil)

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

(defun vi-initialize-normal-map ()
  (unless vi-normal-map
    (setq vi-normal-map (make-keymap))
    (define-key vi-normal-map "l" 'forward-char)
    (define-key vi-normal-map "h" 'backward-char)
    (define-key vi-normal-map "j" 'next-line)
    (define-key vi-normal-map "k" 'previous-line)
    (define-key vi-normal-map "i" 'vi-switch-to-insert-state)
    (define-key vi-normal-map "x" 'delete-char)
    (define-key vi-normal-map "dd" 'kill-whole-line)))

(defun vi-initialize-insert-map ()
  (unless vi-insert-map
    (setq vi-insert-map (make-keymap))
    (define-key vi-insert-map [escape] 'vi-switch-to-normal-state)))

(defun vi-add-minor-mode-map (map)
  (push (cons 'vi-mode map) minor-mode-map-alist))

(defun vi-remove-all-minor-mode-maps ()
  (let ((rm-list nil))
    (dolist (al minor-mode-map-alist)
      (when (eq (car al) 'vi-mode)
	(push al rm-list)))
    (dolist (al rm-list)
      (setq minor-mode-map-alist (delq al minor-mode-map-alist)))))

(defun vi-switch-to-normal-state ()
  (interactive)
  (vi-remove-all-minor-mode-maps)
  (vi-add-minor-mode-map vi-normal-map)
  (vi-mode-line-change-state "normal"))

(defun vi-switch-to-insert-state ()
  (interactive)
  (vi-remove-all-minor-mode-maps)
  (vi-add-minor-mode-map vi-insert-map)
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
    (vi-initialize-normal-map)
    (vi-initialize-insert-map)
    (vi-add-minor-mode-map vi-normal-map)
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
