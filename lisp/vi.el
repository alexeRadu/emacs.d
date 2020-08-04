(defvar vi-mode nil)
(defvar vi-insert-map nil)
(defvar vi-normal-map nil)
(defvar vi-current-state nil)
(defvar vi-unload-function nil)

(defun vi-initialize-normal-map ()
  (unless vi-normal-map
    (setq vi-normal-map (make-keymap))
    (define-key vi-normal-map "l" 'forward-char)
    (define-key vi-normal-map "h" 'backward-char)))

(defun vi-initialize-insert-map ()
  (unless vi-insert-map
    (setq vi-insert-map (make-keymap))))

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
  (setq vi-current-state "normal"))

(defun vi-switch-to-insert-state ()
  (interactive)
  (vi-remove-all-minor-mode-maps)
  (vi-add-minor-mode-map vi-insert-map)
  (setq vi-current-state "insert"))

(defun vi-mode ()
  (interactive)
  (unless vi-mode
    (setq vi-mode t)
    (vi-initialize-normal-map)
    (vi-initialize-insert-map)
    (vi-add-minor-mode-map vi-normal-map)
    (setq vi-current-state "normal")
    (add-hook 'vi-unload-hooks 'vi-remove-all-minor-mode-maps)))

(defun vi-unload-function ()
  (message "vi unload")
  (vi-remove-all-minor-mode-maps))

(provide 'vi)
