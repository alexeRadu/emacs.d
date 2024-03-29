;; Setting appear in the order of importance:
;; For example I want evil to be before any other packages because if the 'init'
;; fails then at least I want to have the vim-keybindings enabled
;;
;; The same rationale is applied to the rest of the packages/settings. But their
;; importance changes with their stability and utility

(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))

;; -----------------------------------------------------------------------------
;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; -----------------------------------------------------------------------------
;; Setup for windows:
;; - To create a single icon in the taskbar follow the instructions from this
;;   link: https://www.emacswiki.org/emacs/MsWindowsSevenProblems
;; - In short here are the steps:
;;   - Go to C:/emacs/bin/ and run runemacs.exe
;;   - Pin the icon in the taskbar
;;   - Shift + RightClick to access properties
;;   - Change the "Target" field to:
;;         "C:/emacs/bin/emacsclientw.exe -c -n -a C:/emacs/bin/runemacs.exe"

;; -----------------------------------------------------------------------------
;; Package Management Initialization
(require `package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Add additional package archives.As of right now only melpa is used.
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; Install 'use-package'. This is needed for installing all the other packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; All my custom code/packages is located in "~/.emacs.d/lisp"
;; Add it to load-path as well
(push "~/.emacs.d/lisp" load-path)
(require 'ww)

;; -----------------------------------------------------------------------------
;; TODO: group all 'visual' settings
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

;; -----------------------------------------------------------------------------
;; some utils for debugging
(setq start-emacs-process nil)
(setq start-emacs-default-command "emacs -q -l ~/.emacs.d/lisp/vi.el")
(setq start-emacs-command-history nil)

(defun start-emacs-with-command (command)
  (interactive (list (read-string
		      "Start emacs: "
		      (if (null start-emacs-command-history)
			  start-emacs-default-command
			(car start-emacs-command-history))
		      nil nil)))

  ;; Save command in history
  (if (null start-emacs-command-history)
      (setq start-emacs-command-history (list command))
    (setq start-emacs-command-history (cons command (delete command start-emacs-command-history))))

  ;; run command in a new process
  (if (or (null start-emacs-process)
	  (eq (process-status start-emacs-process) 'exit))
      (setq start-emacs-process (start-process-shell-command "start-emacs" "*start-emacs*" command))))

;; -----------------------------------------------------------------------------
;; Usefull functions
(defun save-buffer-as (filename)
  (interactive "F")
  (write-region (point-min) (point-max) filename)
  (find-file filename))

;; -----------------------------------------------------------------------------
;; disable the bell
(setq ring-bell-function 'ignore)

;; change cursor shape to be a vertical line to better match modern editors
(setq-default cursor-type 'bar)

;; -----------------------------------------------------------------------------
;; function to evaluate a top level sexp when with the cursor inside it
(defun ra/eval-sexp ()
  (interactive)
  (save-excursion
    (ignore-errors
	(while t
	  (up-list -1 t)))
    (if (equal (following-char) ?\()
	(progn
	  (forward-sexp)
	  (eval-last-sexp nil)))))

;; Unbind 'C-x f'
;; I almost always hit this keybinding when I try to open a new file
(global-unset-key "\C-xf")

;(if (display-graphic-p)
;    (progn
;      (setq initial-frame-alist
;	    '(
;	      (width . 200)
;	      (height . 56)
;	      (left . 80)
;	      (top . 80)))))

;; start emacs maximized
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

(when (string-equal system-type "windows-nt")
  (let ((default-paths (split-string (getenv "PATH") ";"))
	(mypaths '("C:\\emacs\\bin"
		   "C:\\bin")))
    (setq exec-path (append mypaths default-paths))
    (setenv "PATH" (mapconcat 'identity exec-path ";"))
    ))

(setq inhibit-startup-message t)

(global-set-key (kbd "M-*") 'pop-tag-mark)

;; -----------------------------------------------------------------------------
;; Org-mode
(use-package org
  :ensure t
  :defer t
  :config
  (require 'ob-python)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t)
     (python . t)))
  (org-display-inline-images)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (setq org-babel-confirm-evaluate nil))

(use-package org-bullets
  :if (string-equal system-type "gnu/linux")
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ob-ipython
  :after org
  :ensure t)

;; -----------------------------------------------------------------------------
(use-package counsel
  :ensure t
  :defer t
  )

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    ))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(defun neotree-project-dir ()
  (interactive)
  (let ((project-dir (projectile-project-root))
	(file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
	(if (neo-global--window-exists-p)
	    (progn
	      (neotree-dir project-dir)
	      (neotree-find file-name)))
      (message "Could not find git project root."))))

(use-package neotree
  :ensure t
  :bind
  (:map global-map
	([f8] . neotree-project-dir))
  :config
  (progn
    (setq neo-smart-open t)
    (setq projectile-swith-project-action
	  '(progn
	     (neotree-projectile-action)
	     (neotree-toggle)
	     (print "Hello there"))
	  )
    )
  )

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    (setq company-tooltip-align-annotations t
	  company-show-numbers t))
  :diminish company-mode)

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status))
  )

(require 'display-line-numbers)
(defcustom display-line-numbers-exempt-modes '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
  "Major modes on which to disable the linum mode, exempts them from global requirement"
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "turn on line numbers but excempting certain majore modes defined in `display-line-numbers-exempt-modes'"
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))

(global-display-line-numbers-mode)

;; -----------------------------------------------------------------------------
;; Ruler at 80 chars - using fill-column-indicator
(use-package fill-column-indicator
  :ensure t
  :commands turn-on-fci-mode
  :init
  (add-hook 'prog-mode-hook 'turn-on-fci-mode)
  (add-hook 'text-mode-hook 'turn-on-fci-mode)
  (setq fci-rule-column 80))

;; -----------------------------------------------------------------------------
;; Highlight current line
(global-hl-line-mode 1)

;; -----------------------------------------------------------------------------
;; Line numbers
(when (version<= "26.0.50" emacs-version)
  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode))

;; Smart Mode Line (package & theme)
(use-package smart-mode-line-atom-one-dark-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/theme 'atom-one-dark)
    (sml/setup))
  )

;; -----------------------------------------------------------------------------
(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark t))

;; -----------------------------------------------------------------------------
;; Dired
(progn
  (require 'dired)
  (define-key dired-mode-map (kbd "h") (lambda() (interactive) (find-alternate-file "..")))
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "l") 'dired-find-alternate-file)

  (setq dired-dwim-target t)

  ;; allow dired to delete or copy directories
  (setq dired-recursive-copies 'always) ;; never ask
  (setq dired-recursive-deletes 'top)   ;; ask only once

  (add-hook 'dired-mode-hook
	    (lambda ()
	      (dired-hide-details-mode 1)))

  ;; other settings
  ;; on windows dired uses and emulated lisp ls; in order to show directories
  ;; first variable `ls-lisp-dirs-first` should be set to `t`
  (if (string-equal system-type "windows-nt")
      (setq ls-lisp-dirs-first t))
  (if (string-equal system-type "gnu/linux")
      (setq dired-listing-switches "-al --group-directories-first"))
  )

(defun kill-all-dired-buffers ()
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
	(set-buffer buffer)
	(when (equal major-mode 'dired-mode)
	  (setq count (1+ count))
	  (kill-buffer buffer))
	(message "Killed %i dired buffer(s)." count)))))

;; -----------------------------------------------------------------------------
;; Python settings

(add-hook `python-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode t)
	    (setq python-indent 4)
	    (setq tab-width 4)))

;; -----------------------------------------------------------------------------
;; cmake-mode

(use-package cmake-mode
  :ensure t)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; completion functions
;; TODO: test using (start-process-shell-command "test" "foo" "powershell -command \"get-childitem -recurse -name\"")
(defun my/fuzzy-find-completion (string predicate type)
  (let ((files (directory-files-recursively default-directory string)))
    (if (listp files)
	files
      nil)))

(defun my/fuzzy-find ()
  (interactive)
  (require 'ivy)
  (ivy-read "fuzzy-find: " 'my/fuzzy-find-completion))


;;(defun my/fuzzy-find-completion (string predicate type)
;;  (counsel--async-command (format "powershell -command \"get-childitem -recurse -name %s\"" string)))

;;(setq test-proc (make-process :name "test-proc"
;;			      :buffer " *test-output*"
;;			      :command (list "powershell" "-command"
;;					     (format "Get-ChildItem . -Recurse -Name | Select-String -Pattern '%s'" "emacs"))
;;			      ))
