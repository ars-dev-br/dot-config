;;;; Straight

;; Bootstrap straight.el for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package and set it to use straight by default.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; By default, straight will try to update all packages on start.
;; Changing the variable below speeds up booting up Emacs.
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;;;; Appearance

;; Install kaolin-themes, make the theme change with the system and
;; create a toggling binding.
(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-light t)

  (defun ars/system-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'kaolin-light t))
      ('dark (load-theme 'kaolin-galaxy t))))

  (add-hook 'ns-system-appearance-change-functions #'ars/system-theme))

;; Set font
(set-face-attribute 'default nil :font "Iosevka Slab" :height 140 :weight 'normal)

;;;; Saner defaults

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)
(menu-bar-mode 1)
(setq visible-bell t)
(add-hook 'after-init-hook #'global-display-line-numbers-mode)

;; Enable auto-revert mode
(setq auto-revert-verbose t)
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; Remove whitespace when saving files
(setq delete-trailing-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Supress native compilation warnings
(setq warning-suppress-types '((comp)))

;; Uniquify buffer names
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-strip-common-suffix t)
(setq uniquify-after-kill-buffer-p t)

;; Minibuffer history
(setq savehist-file (locate-user-emacs-file "savehist"))
(setq history-length 10000)
(setq history-delete-duplicates t)
(setq savehist-save-minubuffer-history t)
(add-hook 'after-init-hook #'savehist-mode)

;; Backup files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/"))))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq create-lockfiles nil)

;; Enable upcase- (C-x C-u) and downcase-region (C-x C-l)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Do not add encoding comment to Ruby files
(setq ruby-insert-encoding-magic-comment nil)

;; Add keybinding to browse URL under point/mouse
(global-set-key (kbd "C-c b") 'browse-url-at-point)
(global-set-key [s-mouse-1] 'browse-url-at-mouse)

;;;; Packages

;;; Emacs

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package rg)

(use-package helm
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)

  (helm-mode 1))

(use-package avy
  :config
  (global-set-key (kbd "C-c SPC") 'avy-goto-char-timer))

;;; Editing

;; Install evil but don't enable it by default
(use-package evil
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package tree-sitter)
(use-package tree-sitter-langs
  :after tree-sitter
  :init
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  :hook (after-init . global-tree-sitter-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook prog-mode)

(use-package hl-todo
  :hook (after-init . global-hl-todo-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook (after-init . smartparens-global-strict-mode))

(use-package company
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.5)
  :hook (after-init . global-company-mode))

;;; Project management

(use-package projectile
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;;; Version Control

(use-package magit
  :ensure t)

(use-package forge
  :after magit)

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :after git-gutter
  :demand fringe-helper
  :config
  ;; subtle diff indicators in the fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
  nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
  nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
  [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
  nil nil 'center)
  (global-git-gutter-mode))

(use-package git-link
  :config
  (global-set-key (kbd "C-c g l") 'git-link))

(use-package git-timemachine
  :config
  (global-set-key (kbd "C-c g t") 'git-timemachine))

(use-package gitignore-mode)

;;; Programming Languages

(use-package kotlin-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package yaml-mode)

(use-package clojure-mode)
(use-package cider
  :after clojure-mode)

;;; Appearance

(use-package telephone-line
  :config
  (require 'telephone-line)
  (telephone-line-mode 1))

;; Make the background color of file-buffers different from other buffers.
(use-package solaire-mode
  :hook (after-init . solaire-global-mode))
