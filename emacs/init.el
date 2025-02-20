;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'cl-extra)

;;;; Straight

;; Bootstrap straight.el for package management
(defvar bootstrap-version)
(setq straight-repository-branch "develop")
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

;; Set appearance constants
(defconst ars-theme--light-theme 'modus-operandi)
(defconst ars-theme--light-font "Aporetic Serif Mono")
(defconst ars-theme--light-height 120)
(defconst ars-theme--light-width 'regular)
(defconst ars-theme--light-weight 'normal)

(defconst ars-theme--dark-theme 'modus-vivendi-tinted)
(defconst ars-theme--dark-font "Aporetic Serif Mono")
(defconst ars-theme--dark-height 120)
(defconst ars-theme--dark-width 'regular)
(defconst ars-theme--dark-weight 'normal)

;; Install themes
(use-package modus-themes :ensure)

;; Make the theme change with the system (macos only) and create a
;; toggling binding.
(use-package all-themes
  :straight '(all-themes :type built-in)
  :init
  (load-theme ars-theme--dark-theme t)
  (set-face-attribute 'default nil
		      :font ars-theme--dark-font
		      :height ars-theme--dark-height
		      :weight ars-theme--dark-weight
		      :width ars-theme--dark-width)

  (provide 'all-themes)

  :config
  (defun ars-theme--system-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme ars-theme--light-theme t)
	      (set-face-attribute 'default nil
				  :font ars-theme--light-font
				  :height ars-theme--light-height
				  :weight ars-theme--light-weight
				  :width ars-theme--light-width))
      ('dark (load-theme ars-theme--dark-theme t)
	     (set-face-attribute 'default nil
				 :font ars-theme--dark-font
				 :height ars-theme--dark-height
				 :weight ars-theme--dark-weight
				 :width ars-theme--dark-width))))

  (defun ars-theme--toggle-theme ()
    "Toggle between a light or a dark theme."
    (interactive)
    (let ((enabled-themes custom-enabled-themes))
      (mapc #'disable-theme custom-enabled-themes)
      (if (member ars-theme--light-theme enabled-themes)
	  (ars-theme--system-theme 'dark)
	(ars-theme--system-theme 'light))))

  (add-hook 'ns-system-appearance-change-functions #'ars-theme--system-theme)

  :bind
  ("<f5>" . ars-theme--toggle-theme))

;;;; Saner defaults
(use-package saner-defaults
  :straight '(saner-defaults :type built-in)
  :init
  (setq inhibit-startup-message t)
  (column-number-mode 1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (repeat-mode 1)
  (add-hook 'after-init-hook #'global-display-line-numbers-mode)
  (setq frame-resize-pixelwise t)
  (setq-default indent-tabs-mode nil)

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
  (global-set-key (kbd "C-c c b") 'browse-url-at-point)
  (global-set-key [s-mouse-1] 'browse-url-at-mouse)

  ;; Increase kill-ring size
  (setq kill-ring-max 10000)

  (provide 'saner-defaults))


;;;; Packages

;;; Emacs

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package rg)
(use-package multi-term)

(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode)
  :bind ("C-c m" . marginalia-cycle))

(use-package consult
  :bind (("M-g g" . consult-goto-line)
	 ("M-y" . consult-yank-from-kill-ring)
	 ("C-c C-s" . consult-line)
	 ("C-c C-r" . consult-ripgrep)))

(use-package embark
  :bind (("C-." . embark-act)
	 ("C-;" . embark-dwim)
	 ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package avy
  :config
  (global-set-key (kbd "C-c SPC") 'avy-goto-char-timer)
  (global-set-key (kbd "C-c C-SPC") 'avy-goto-char-timer))

(use-package dired-preview)


;;; Editing

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package tree-sitter
  :init
  (setq major-mode-remap-alist
        '((ruby-mode . ruby-ts-mode)))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '( "\\.tsx\\'" . tsx-ts-mode))
  (require 'tree-sitter)
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

;;; Version Control

(use-package magit
  :ensure t)

(use-package forge
  :after magit)

(use-package git-gutter
  :hook ((prog-mode text-mode) . git-gutter-mode))

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
  nil nil 'center))

(use-package git-link
  :config
  (global-set-key (kbd "C-c g l") 'git-link))

(use-package git-timemachine
  :config
  (global-set-key (kbd "C-c g t") 'git-timemachine))

;; (use-package gitignore-mode)

(use-package ligature
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures t '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                              "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                              "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                              ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;; Programming Languages

(use-package csv-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package rust-mode)
(use-package yaml-mode)

(use-package clojure-mode)
(use-package cider
  :after clojure-mode)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")

  :hook ((ruby-mode . lsp)
	 (ruby-ts-mode . lsp)
         (typescript-ts-mode . lsp)
         (tsx-ts-mode . lsp))
  :commands lsp)

(use-package js-mode-defaults
  :straight '(js-mode-defaults :type built-in)
  :init
  (setq js-indent-level 2)
  (provide 'js-mode-defaults))

;;; Appearance

;; Make the background color of file-buffers different from other buffers.
(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

;;;; Custom functions
;; Most of these functions have been graciously provided by either
;; Laura Viglioni (https://github.com/Viglioni) and Sandro
;; Luiz (https://github.com/ansdor).

(defun ars-window--previous-files ()
  "Returns a list of buffers visiting files skipping the current one."
  (seq-filter 'buffer-file-name
	      (remq (current-buffer) (buffer-list))))

(defun ars-window--shift-list (list)
  "Returns a new list with the same elements of LIST shifted one position to the left.
I.e. (ars-window--shift-list '(a b c d)) returns '(b c d a)."
  (let ((reversed (reverse list)))
    (reverse (cons (car list)
		   (butlast reversed)))))

(defun ars-window--unshift-list (list)
  "Returns a new list with the same elements of LIST shifted one position to the right.
I.e. (ars-window--unshift-list '(a b c d)) returns '(d a b c)."
  (append (last list) (butlast list)))

(defun ars-window--rotate-list (list direction)
  "Returns a new list with the same elements of LIST shifted one
position to the right or to the left depending on the value of
DIRECTION (which must be either 'cw or 'ccw)."
  (if (eq direction 'cw)
      (ars-window--shift-list list)
    (ars-window--unshift-list list)))

(defun ars-window--shift-buffers (direction)
  "Rotates the currently open buffers between the windows of the
current frame. DIRECTION must be either 'cw or 'ccw."
  (let* ((buffers (mapcar #'window-buffer (window-list)))
	 (rotated-buffers (ars-window--rotate-list buffers direction)))
    (mapc (lambda (window)
	    (let ((window-index (seq-position (window-list) window)))
	      (set-window-buffer window (nth window-index rotated-buffers))))
	  (window-list))))

(defun ars-window--shift-buffers-cw ()
  "Rotates the currently open buffers between the windows of the
current frame in a clockwise direction."
  (interactive)
  (ars-window--shift-buffers 'cw))

(defun ars-window--shift-buffers-ccw ()
  "Rotates the currently open buffers between the windows of the
current frame in a counterclockwise direction."
  (interactive)
  (ars-window--shift-buffers 'ccw))

(defun ars-window--split-window-double-columns ()
  "Set the current frame layout to two columns."
  (interactive)
  (let ((previous-file (or (car (ars-window--previous-files))
			   (current-buffer))))
    (delete-other-windows)
    (set-window-buffer (split-window-right) previous-file)
    (balance-windows)))

(defun ars-window--split-window-triple-columns ()
  "Set the current frame layout to three columns."
  (interactive)
  (delete-other-windows)
  (let* ((previous-files (ars-window--previous-files))
	 (second-buffer (or (nth 0 previous-files) (current-buffer)))
	 (third-buffer (or (nth 1 previous-files) (current-buffer)))
	 (second-window (split-window-right))
	 (third-window (split-window second-window nil 'right)))
    (set-window-buffer second-window second-buffer)
    (set-window-buffer third-window third-buffer))
  (balance-windows))

(defun ars-window--split-window-quadruple-columns ()
  "Set the current frame layout to quadruple columns."
  (interactive)
  (delete-other-windows)
  (let* ((previous-files (ars-window--previous-files))
	 (second-buffer (or (nth 0 previous-files) (current-buffer)))
	 (third-buffer (or (nth 1 previous-files) (current-buffer)))
	 (fourth-buffer (or (nth 2 previous-files) (current-buffer)))
	 (second-window (split-window-right))
	 (third-window (split-window second-window nil 'right))
	 (fourth-window (split-window third-window nil 'right)))
    (set-window-buffer second-window second-buffer)
    (set-window-buffer third-window third-buffer)
    (set-window-buffer fourth-window fourth-buffer))
  (balance-windows))

(defun ars-window--split-window-two-by-two-grid ()
  "Set the current frame layout to a two-by-two grid."
  (interactive)
  (delete-other-windows)
  (let* ((previous-files (ars-window--previous-files))
	 (second-buffer (or (nth 0 previous-files) (current-buffer)))
	 (third-buffer (or (nth 1 previous-files) (current-buffer)))
	 (fourth-buffer (or (nth 2 previous-files) (current-buffer)))
	 (second-window (split-window-right))
	 (third-window (split-window-below))
	 (fourth-window (split-window second-window nil 'below)))
    (set-window-buffer second-window second-buffer)
    (set-window-buffer third-window third-buffer)
    (set-window-buffer fourth-window fourth-buffer))
  (balance-windows))

(defun ars-window--split-window-three-by-two-grid ()
  "Set the current frame layout to a three-by-two grid."
  (interactive)
  (delete-other-windows)
  (let* ((previous-files (ars-window--previous-files))
	 (second-buffer (or (nth 0 previous-files) (current-buffer)))
	 (third-buffer (or (nth 1 previous-files) (current-buffer)))
	 (fourth-buffer (or (nth 2 previous-files) (current-buffer)))
         (fifth-buffer (or (nth 3 previous-files) (current-buffer)))
         (sixth-buffer (or (nth 4 previous-files) (current-buffer)))
	 (second-window (split-window-right))
         (third-window (split-window-right))
         (fourth-window (split-window-below))
         (fifth-window (split-window second-window nil 'below))
         (sixth-window (split-window third-window nil 'below)))
    (set-window-buffer second-window second-buffer)
    (set-window-buffer third-window third-buffer)
    (set-window-buffer fourth-window fourth-buffer)
    (set-window-buffer fifth-window fifth-buffer)
    (set-window-buffer sixth-window sixth-buffer))
  (balance-windows))

(defun ars-window--split-window-four-by-two-grid ()
  "Set the current frame layout to a four-by-two grid."
  (interactive)
  (delete-other-windows)
  (let* ((previous-files (ars-window--previous-files))
	 (second-buffer (or (nth 0 previous-files) (current-buffer)))
	 (third-buffer (or (nth 1 previous-files) (current-buffer)))
	 (fourth-buffer (or (nth 2 previous-files) (current-buffer)))
         (fifth-buffer (or (nth 3 previous-files) (current-buffer)))
         (sixth-buffer (or (nth 4 previous-files) (current-buffer)))
         (seventh-buffer (or (nth 5 previous-files) (current-buffer)))
         (eighth-buffer (or (nth 6 previous-files) (current-buffer)))
	 (second-window (split-window-right))
         (third-window (split-window-right))
         (fourth-window (split-window-right))
         (fifth-window (split-window-below))
         (sixth-window (split-window second-window nil 'below))
         (seventh-window (split-window third-window nil 'below))
         (eighth-window (split-window fourth-window nil 'below)))
    (set-window-buffer second-window second-buffer)
    (set-window-buffer third-window third-buffer)
    (set-window-buffer fourth-window fourth-buffer)
    (set-window-buffer fifth-window fifth-buffer)
    (set-window-buffer sixth-window sixth-buffer))
  (balance-windows))

(defun ars-window--switch-to-previous-file ()
  "Switch to the most recently used file."
  (interactive)
  (switch-to-buffer (or (car (ars-window--previous-files))
			(current-buffer))))

(defun ars-window--kill-all-buffers ()
  "Kill all open buffers."
  (interactive)
  (mapcar 'kill-buffer (buffer-list)))

(defun ars-window--split-below-and-move-point ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun ars-window--split-right-and-move-point ()
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-c 2") 'ars-window--split-below-and-move-point)
(global-set-key (kbd "C-c 3") 'ars-window--split-right-and-move-point)

(global-set-key (kbd "C-c w s") 'ars-window--shift-buffers-cw)
(global-set-key (kbd "C-c w u") 'ars-window--shift-buffers-ccw)
(global-set-key (kbd "C-c w w") 'ars-window--switch-to-previous-file)
(global-set-key (kbd "C-c w 1") 'delete-other-windows) ; just for consistency's sake
(global-set-key (kbd "C-c w 2") 'ars-window--split-window-double-columns)
(global-set-key (kbd "C-c w 3") 'ars-window--split-window-triple-columns)
(global-set-key (kbd "C-c w 4") 'ars-window--split-window-quadruple-columns)

(global-set-key (kbd "C-c w x") 'ars-window--split-window-two-by-two-grid)
(global-set-key (kbd "C-c w 6") 'ars-window--split-window-three-by-two-grid)
(global-set-key (kbd "C-c w 8") 'ars-window--split-window-four-by-two-grid)

(global-set-key (kbd "C-c b k") 'ars-window--kill-all-buffers)

;;;; org-mode

(use-package org
  :ensure t
  :custom
  (org-directory "~/src/ars-dev-br/pkm/")
  (org-agenda-files '("~/src/ars-dev-br/pkm/"
                      "~/src/ars-dev-br/pkm/journal/"))
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                       (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
  (org-use-fast-todo-selection t)
  (org-agenda-custom-commands '(("n" "Agenda and all NEXT"
                                 ((agenda "")
                                  (todo "NEXT")))))
  :bind (("C-c n a" . org-agenda)))

(use-package org-journal
  :init
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/src/ars-dev-br/pkm/journal")
  (setq org-journal-date-format "%Y-%m-%d (%A, %d %B %Y)")
  (setq org-journal-file-type 'yearly)
  (setq org-journal-file-format "%Y.org")
  (setq org-journal-carryover-items "")
  :bind (("C-c n j" . org-journal-new-entry)))

(use-package org-ql)

(defun ars-org--getkey-gen (&rest properties)
  "Generates a getkey-func to be used with org-sort-entries. It returns a
lambda function that will return a list consisting of the PROPERTIES of
the node under point.

PROPERTIES can be of two types, a single STRING, which will get the
property with such name, or a list of STRING, which will return the
first non-nil property from the list.

For example:

  (ars-org--getkey-gen \"FOO\" \"BAR\")

will return '(\"value-of-foo\" \"value-of-bar\"). Whereas:

  (ars-org--getkey-gen \"FOO\" '(\"BAR\" \"BAZ\"))

will return '(\"value-of-foo\" \"value-of-bar-or-baz\")."
  (lambda ()
    (mapcar
     (lambda (property)
       (if (listp property)
           (let ((active-property (cl-some
                                   (lambda (p) (org-entry-get (point-marker) p))
                                   property)))
             (org-entry-get (point-marker) active-property))
         (org-entry-get (point-marker) property)))
     properties)))

(defun ars-org--compare-func (a b)
  ""
  (cond ((and (null a) (null b)) t)
        ((string= (car a) (car b)) (ars-org--compare-func (cdr a) (cdr b)))
        (t (org-string< (car a) (car b)))))

(defun ars-org--sort-albums-by-artist-and-by-released ()
  (interactive)
  (org-sort-entries nil
                    ?f
                    (ars-org--getkey-gen "ARTIST" "RELEASED")
                    #'ars-org--compare-func
                    ""
                    t))

(defun ars-org--sort-books-by-author-series-and-published ()
  (interactive)
  (org-sort-entries nil
                    ?f
                    (ars-org--getkey-gen "AUTHOR_SORT" "SERIES" '("TITLE_SORT" "TITLE"))
                    #'ars-org--compare-func
                    ""
                    t))
