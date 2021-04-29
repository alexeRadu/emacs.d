(defvar vi-mode nil)

;-------------------------------------------------------------------------------
(defmacro vi-define-state (state &rest body)
  (let* ((keymap (intern (format "vi-%s-map" state))))
    `(progn
       (setq ,keymap (make-keymap)))))

;-------------------------------------------------------------------------------
; global variables
(defvar vi-map-alist ())

(vi-define-state insert)
(vi-define-state normal)

;-------------------------------------------------------------------------------
; mapping functions and variables
(defun vi-remove-all-minor-mode-maps ()
  (let ((rm-list nil))
    (dolist (al minor-mode-map-alist)
      (when (eq (car al) 'vi-mode)
       (push al rm-list)))
    (dolist (al rm-list)
      (setq minor-mode-map-alist (delq al minor-mode-map-alist)))))

;-------------------------------------------------------------------------------
; cursor functions
(defun set-cursor-type (type)
  (save-current-buffer
    (dolist (buffer (buffer-list))
      (set-buffer (get-buffer-create buffer))
      (setq cursor-type type))))

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
; vi-mode
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
    (vi-init-modeline)
    (vi-switch-to-normal-state)))

(defun vi-mode-disable ()
  (when vi-mode
    (setq vi-mode nil)
    (unless (member 'vi-mode-enable minibuffer-exit-hook)
      (add-hook 'minibuffer-exit-hook 'vi-mode-enable))
    (vi-clear-modeline)
    (vi-remove-all-minor-mode-maps)))

(provide 'vi)

(defun vi-show-modeline ()
  (interactive)
  (print mode-line-format))

(defun vi-visual-update ()
  (vi-init-modeline)
  (if (equal vi-mode-line-state "normal")
      (set-cursor-type 'box)
    (set-cursor-type 'bar)))

(add-hook 'window-configuration-change-hook 'vi-visual-update)
(add-hook 'emacs-startup-hook 'vi-visual-update)

