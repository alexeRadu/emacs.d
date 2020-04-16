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
 '(package-selected-packages
   (quote
    (evil magit svg company counsel-gtags ggtags neotree avy counsel swiper projectile org-bullets use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

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

;; Evil mode
;;
;; This is the recommended way of installing the package (instead of use-package).
;; TODO: Should revise this later to see if use-package could be used.
(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)

;; TODO: further customize evil mode and integrate it better with emacs. For example
;; the command mode doesn't seem to fit well with emacs M-x. Maybe look for other
;; similar keybinding modes for a custom experience.

;; Line numbers
;;
;; Enable linum-mode for version of emacs >= 26.0.50.
;; TODO: for version < 25.0.50 should either look for a solution or throw a (warning)
;; message.
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; TODO: various customizations that exist @ https://www.emacswiki.org/emacs/LineNumbers.
;; TODO: selectively enable line-numbers depending on the file type.
