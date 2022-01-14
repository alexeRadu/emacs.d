(require 'cl-extra)

(defun prev-monday-date ()
  (let ((day-in-sec (* 60 60 24))
	(days-since-monday (- (cl-parse-integer (format-time-string "%u")) 1)))
    (format-time-string "%D" (- (float-time) (* days-since-monday day-in-sec)))))

(defun next-sunday-date ()
  (let ((day-in-sec (* 60 60 24))
	(days-to-sunday (- 7 (cl-parse-integer (format-time-string "%u")))))
    (format-time-string "%D" (+ (float-time) (* days-to-sunday day-in-sec)))))

(defun insert-ww-timestamp ()
  (interactive)
  (insert
   (format "Work Week %s: %s - %s"
	   (format-time-string "%W")
	   (prev-monday-date)
	   (next-sunday-date))))

(defun my/insert-ww-structure ()
  (interactive)

  ;; goto to the beginning of the buffer
  (erase-buffer)

  ;; headers section
  (insert "#+STARTUP: overview")
  (insert "\n\n")

  ;; timestamp
  (require 'time)
  (insert-ww-timestamp)
  (insert "\n\n")

  ;; body
  (insert "* Monday:\n")
  (insert "* Tuesday:\n")
  (insert "* Wednesday:\n")
  (insert "* Thursday:\n")
  (insert "* Friday:\n")
  (insert "* TODO:\n")
  )

(defun my/find-or-create-ww-document ()
  (interactive)
  (let* ((ww (- (cl-parse-integer (format-time-string "%w")) 1))
	 (ww-path (format "c:/Users/nxa06732/Documents/WRs/%s/ww%d.org"
			  (format-time-string "%Y")
			  ww)))
    (if (file-exists-p ww-path)
	(find-file ww-path)
      (progn
	(find-file ww-path)
	(my/insert-ww-structure)
	(if (> ww 1)
	    (let ((prev-ww-path (format "c:/Users/nxa06732/Documents/WRs/%s/ww%d.org"
					 (format-time-string "%Y")
					 (- ww 1)))
		  (this-buffer (current-buffer))
		  (prev-todos))
	      (find-file prev-ww-path)
	      (goto-char 0)
	      (search-forward "TODO:")
	      (forward-line)
	      (setq prev-todos (buffer-substring-no-properties (line-beginning-position) (point-max)))
	      (switch-to-buffer this-buffer)
	      (insert prev-todos))
	  )
	(save-current-buffer)
	)
      )
    )
  )


  

(provide 'ww)
