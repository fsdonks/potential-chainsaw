;;;
;;; Example Emacs startup for Clojure users.
;;;

(defconst emacs-start-time (current-time))

(unless noninteractive (message "Loading %s..." load-file-name))

(require 'cl)

(setq user-root (file-name-directory (or load-file-name (buffer-file-name))))
(add-to-list 'load-path (concat user-root "lisp/"))
(setq user-emacs-directory user-root) ; for use down the call stack.

(defun add-subdirs-to-load-path (DIR &optional root)
  (let* ((root (or root user-root))
         (elisp-dir (expand-file-name DIR root)))
    (when (file-exists-p elisp-dir)
      (add-to-list 'load-path elisp-dir)
      (let ((default-directory elisp-dir))
        (normal-top-level-add-subdirs-to-load-path)))))

;; The "src" directory can have, e.g., git-installed libraries.
(add-subdirs-to-load-path "src")

(setq package-user-dir (concat user-root "elpa"))
(setq custom-file (concat user-root "custom.el"))

(require 'package)
(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("melpa"       . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(package-initialize)

(setq load-prefer-newer t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  ;; installing `use-package' also installs its dependencies
  ;; `bind-key' and `diminish'.
  (package-install 'use-package))

(require 'use-package)

;; Stop polluting the directory with auto-saved files and backup
(setq auto-save-default nil)
(setq make-backup-files nil)


;;;----------------------------------------------------------------------
;;; Utils

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph.
;; You can convert an entire buffer from paragraphs to lines by
;; recording a macro that calls 'unfill-paragraph' and moves past the
;; blank-line to the next unfilled paragraph and then executing that
;; macro on the whole buffer, 'C-u 0 C-x e'.
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


;;;----------------------------------------------------------------------
;;; Look and feel

(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)
(setq ring-bell-function #'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode 1)

(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

(defun load-theme-if-available (THEME)
  (condition-case err
      (load-theme THEME t)
    (error (message "%s" (error-message-string err)) nil)))

(defmacro load-theme-by-preference (&rest THEMES)
  `(or ,@(mapcar (lambda (theme)
                   `(load-theme-if-available ,theme))
                 THEMES)))

(defun add-subdirs-to-custom-theme-load-path (DIR &optional root)
  (let* ((root (or root user-root))
         (srcdir (expand-file-name DIR root))
         (theme-subdirs
          (remove-if-not #'(lambda (nm)
                             (and (file-directory-p nm)
                                  (string-match-p ".*-theme$" nm)))
                         (mapcar #'(lambda (nm)
                                     (expand-file-name nm srcdir))
                                 (directory-files srcdir)))))
    (dolist (theme-subdir theme-subdirs)
      (add-to-list 'custom-theme-load-path theme-subdir))))

(cond ((display-graphic-p)
       (scroll-bar-mode -1)
       (when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
       (cond ((eq 'gnu/linux system-type)
              (set-face-attribute 'default nil :font "Ubuntu Mono-12"))
             ((member system-type '(windows-nt cygwin))
              (set-face-attribute 'default nil :font "Consolas-13:antialias=standard"))
             ((eq 'darwin system-type)
              (set-face-attribute 'default nil :font "Monaco-14")))
       (use-package ample-zen-theme :defer t :ensure t)
       (use-package cyberpunk-theme :defer t :ensure t)
       (load-theme-by-preference 'cyberpunk 'ample-zen 'misterioso))
      (t ;; Try to make terminal emacs look nice.
       (add-to-list 'custom-theme-load-path
                    (expand-file-name "src/replace-colorthemes" user-root))
       (add-subdirs-to-custom-theme-load-path "src")
       (load-theme 'cyberpunk t)
       ))

(custom-set-faces
  ;; Change the org title and headlines to normal size text (when the
  ;; theme tries to make them bigger, e.g. cyberpunk does this).
  '(org-document-title ((t (:height 1.0))))
  '(org-level-1 ((t (:height 1.0))))
  '(org-level-2 ((t (:height 1.0))))
  '(org-level-3 ((t (:height 1.0)))))


;;;----------------------------------------------------------------------
;;; Clojure

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'")

(use-package cider
  :ensure t
  :commands (cider-jack-in cider-connect)
  :config (require 'dot-cider))


;;;----------------------------------------------------------------------
;;; Org

(use-package org
  :ensure org-plus-contrib
  :commands (org-agenda org-agenda-list)
  :mode ("\\.org\\'" . org-mode)
  :bind (("\C-c l" . org-store-link)
         ("\C-c a" . org-agenda)
         ("\C-c c" . org-capture)
         ("\C-c b" . org-iswitchb)))


;;;----------------------------------------------------------------------
;;; This and that

(use-package ace-jump-mode
  :bind ("C-c <SPC>" . ace-jump-mode)
  :ensure t)

(use-package ido-vertical-mode
  :defer t
  :init (eval-after-load 'ido '(progn (ido-vertical-mode t)))
  :ensure t)

(use-package flx-ido
  :defer t
  :init (eval-after-load 'ido '(progn (flx-ido-mode t)))
  :ensure t)

(use-package smex
  :bind (("M-x" . smex)
         ("C-c M-x" . execute-extended-command))
  :commands smex
  :ensure t)

(use-package paredit
  ;; which should i use: paredit or smartparens? :)
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode))
  :ensure t)

(use-package smartparens
  :commands (smartparens-mode show-smartparens-mode)
  :config (require 'smartparens-config)
  :diminish smartparens-mode
  :ensure t)

(use-package magit
  ;;:bind ("C-x m" . magit-status)
  :commands magit-status
  :ensure t
  :config
  ;; hack
  (setq magit-git-executable
        "C:/Program Files/Git/bin/git.exe"))

(use-package git-gutter
  :defer t
  :diminish git-gutter-mode
  :init (global-git-gutter-mode t)
  :ensure t)

(use-package powerline
  :disabled t
  :if (display-graphic-p)
  :config (powerline-default-theme)
  :ensure t)

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t)

(use-package elscreen
  :init
  (progn
    (elscreen-start)
    (global-unset-key "\C-b")
    (elscreen-set-prefix-key (kbd "C-b"))
    (setq elscreen-display-tab nil)
    ;; Muscle memory: tmux uses "l" (for "last")
    (define-key elscreen-map "l" 'elscreen-toggle)
    (use-package elscreen-persist
      :init (elscreen-persist-mode 1)
      :ensure t))
  :ensure t)

(use-package org-present
  :init
  (progn
    (add-hook 'org-present-mode-hook
	      (lambda ()
		(org-present-big)
		(org-display-inline-images)
		(org-present-hide-cursor)
		(org-present-read-only)))
    (add-hook 'org-present-mode-quit-hook
	      (lambda ()
		(org-present-small)
		(org-remove-inline-images)
		(org-present-show-cursor)
		(org-present-read-write)))))


;;;----------------------------------------------------------------------
;;; Built-in packages

(use-package gnutls
  :config
  (progn
    ;; Got the following file from http://curl.haxx.se/ca/cacert.pem.
    (pushnew (concat user-root "ssl/cacert.pem") gnutls-trustfiles)))

(use-package mule
  :no-require t
  :config
  (prefer-coding-system 'utf-8)
  ;; Uncomment these next two lines in the future when I have a better
  ;; idea what they do.
  ;;(setq coding-system-for-read 'utf-8)
  ;;(setq coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8))

(use-package electric)

(use-package prog-mode
  :init (eval-after-load 'electric
          '(progn (add-hook 'prog-mode-hook 'electric-indent-mode))))

(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode)

(use-package lisp-mode
  :config (progn
            (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
            (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
            (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)))

(use-package hl-line
  :config (global-hl-line-mode t))


;;;----------------------------------------------------------------------
;;; Timing

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))
