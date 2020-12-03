;; Setting appear in the order of importance:
;; For example I want evil to be before any other packages because if the 'init'
;; fails then at least I want to have the vim-keybindings enabled
;;
;; The same rationale is applied to the rest of the packages/settings. But their
;; importance changes with their stability and utility

;; -----------------------------------------------------------------------------
;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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


;; -----------------------------------------------------------------------------
;; TODO: group all 'visual' settings
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; -----------------------------------------------------------------------------
;; set font to IBM Plex Mono Light
;; Set different font size on different platforms since I'm using different
;; screen sizes
;; TODO: maybe font size should be changed based on the screen size and not
;; system-type
(cond
 ((string-equal system-type "windows-nt")
  (progn
    (set-frame-font "IBM Plex Mono Light 9")))
 ((string-equal system-type "gnu/linux")
  (progn
    (set-frame-font "IBM Plex Mono Light 10"))))

;; -----------------------------------------------------------------------------
;; Evil Mode
;; TODO: use 'use-package' to install packages
(unless (package-installed-p 'evil)
  (package-install 'evil))

(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode)

(evil-set-leader 'normal (kbd "<SPC>"))
(evil-define-key 'normal 'global (kbd "<leader>g") 'magit-status)

;; Files
(evil-define-key 'normal 'global (kbd "<leader>ff") 'counsel-find-file)

;; Buffers
(evil-define-key 'normal 'global (kbd "<leader>bb") 'ivy-switch-buffer)


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

(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
	    '(
	      (width . 200)
	      (height . 56)
	      (left . 80)
	      (top . 80)))))
(when (string-equal system-type "windows-nt")
  (let (
	(mypaths
	 '(
	   "C:/emacs-26.1-x86_64/bin"
	   "C:/cygwin64/bin"
	   ))
	)

    (setq exec-path (append mypaths (list "." exec-directory)))
    (setenv "PATH" (mapconcat 'identity mypaths ";"))
    ))

(setq inhibit-startup-message t)

(global-set-key (kbd "M-*") 'pop-tag-mark)

;; -----------------------------------------------------------------------------
;; Org-mode
(use-package org
  :ensure t
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
;; Ruler at 80 chars
;;
;; I use fill-column-indicator.
;; TODO: there a lot of settings, customizations and issues. To read and see additional
;; usefull settings.
(unless (package-installed-p 'fill-column-indicator)
  (package-install 'fill-column-indicator))

(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-column 80)

;; -----------------------------------------------------------------------------
;; Evil Mode
(unless (package-installed-p 'evil)
  (package-install 'evil))

(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode)

(evil-set-leader 'normal (kbd "<SPC>"))
(evil-define-key 'normal 'global (kbd "<leader>g") 'magit-status)

;; Files
(evil-define-key 'normal 'global (kbd "<leader>ff") 'counsel-find-file)

;; Buffers
(evil-define-key 'normal 'global (kbd "<leader>bb") 'ivy-switch-buffer)

;; Lisp evaluation
(evil-define-key 'normal 'global (kbd "<leader>e") 'ra/eval-sexp)


;; -----------------------------------------------------------------------------
;; Highlight current line
(global-hl-line-mode 1)

;; -----------------------------------------------------------------------------
;; Line numbers
(when (version<= "26.0.50" emacs-version)
  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode))

;; -----------------------------------------------------------------------------
;; Evil Magit - evil keybindings for magit
(require 'evil-magit)

;; -----------------------------------------------------------------------------
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
