(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "9ffe970317cdfd1a9038ee23f4f5fe0b28b99950281799e4397e1a1380123147" default)))
 '(package-selected-packages
   (quote
    (doom-themes zenburn-theme evil-magit fill-column-indicator evil magit svg company counsel-gtags ggtags neotree avy counsel swiper projectile org-bullets use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; make "~/.emacs.d/lisp" directory as the primary location of my elisp code
(push "~/.emacs.d/lisp" load-path)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; disable the bell
(setq ring-bell-function 'ignore)

;; change cursor shape to be a vertical line to better match modern editors
(setq-default cursor-type 'bar)

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

(require `package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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

(require 'evil)
(evil-mode)

(evil-set-leader 'normal (kbd "<SPC>"))
(evil-define-key 'normal 'global (kbd "<leader>g") 'magit-status)

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
;; Doom Modeline
(require 'all-the-icons)
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-modal-icon nil)

;; -----------------------------------------------------------------------------
;; Doom themes
(load-theme 'doom-one t)
