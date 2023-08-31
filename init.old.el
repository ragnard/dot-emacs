;;; init.el -- my init
;;; Commentary:

;;; Code:

(defconst emacs-start-time (current-time))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-message t)

;; Set up load path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq auto-save-dir (concat user-emacs-directory "auto-save/"))
(if (not (file-exists-p auto-save-dir))
    (make-directory auto-save-dir))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name auto-save-dir) t )))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

(when is-mac
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))



;;--------------------------------------------------------------------
;; packages

(require 'package)

(setq package-user-dir (locate-user-emacs-file "packages"))

;; Add melpa to package repos
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; (setq package-pinned-packages
;;       '((cider              . "melpa-stable")
;;         ;(clojure-mode       . "melpa-stable")
;;         ))


(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package diminish
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance

(setq visible-bell nil
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil
      initial-scratch-message nil)

(defun bell ()
  "Rings the bell."
  (message "*ding*")
  (sit-for 0.2))

(setq ring-bell-function 'bell)


;; Highlight current line
(global-hl-line-mode 0)

;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(load-theme 'plain)

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(set-face-attribute
 'default nil
 :family "Fira Code"
 :height 140
 :width 'normal
 :weight 'normal)

;; (set-face-attribute
;;  'default nil
;;  :family "Liberation Mono"
;;  :height 120
;;  :width 'normal
;;  :weight 'normal)


;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sane defaults
;;(use-package smooth-scrolling)

;; Allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; Auto refresh buffers
(use-package autorevert
  :init
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil))

(use-package dired
  :init
  (setq dired-listing-switches "-aBhl --group-directories-first"))

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
;; (global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
(use-package recentf
  :init
  (setq recentf-max-saved-items 100)
  (recentf-mode 1))

;; Save minibuffer history
(use-package savehist
  :init
  (savehist-mode 1)
  (setq history-length 1000))

;; Undo/redo window configuration with C-c <left>/<right>
(use-package winner
  :init
  (winner-mode 1))

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Keep cursor away from edges when scrolling up/down
;; (require 'smooth-scrolling)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)



;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))

;; A saner ediff
(use-package ediff
  :init
  (setq ediff-diff-options "-w")
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages


;; (use-package exec-path-from-shell
;;  :ensure t
;;  :init
;;  (setq exec-path-from-shell-check-startup-files nil)
;;  :config
;;  ;(add-to-list 'exec-path-from-shell-variables "GOPATH")
;;  (exec-path-from-shell-initialize))

(use-package browse-url
  :init
  (setq browse-url-browser-function 'browse-url-default-macosx-browser))

(use-package whitespace
  :init
  ;(add-hook 'before-save-hook 'whitespace-cleanup)
  )

(use-package wgrep
  :ensure t)

;; (use-package xclip
;;   :ensure t
;;   :init
;;   (xclip-mode 1)
;;   (setq xclip-select-enable-clipboard t)
;;   (turn-on-xclip))

(use-package deft
  :ensure t
  :config
  (setq deft-directory (expand-file-name "~/notes"))
  (when (not (file-exists-p deft-directory))
    (make-directory deft-directory t))
  (setq deft-extensions '("org" "md"))
  (setq deft-default-extension "org")
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t))

(use-package imenu
  :config
  (set-default 'imenu-auto-rescan t))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
   ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))

        ;; '((swiper . ivy--regex-plus)
        ;;   (t      . ivy--regex-fuzzy))

  ;; does not count candidates
  ;;(setq ivy-count-format "")
  ;; no regexp by default
  ;;(setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  ;; (setq ivy-re-builders-alist
  ;;       ;; allow input not in order
  ;;       '((t   . ivy--regex-ignore-order)))
  )

(use-package counsel
  :ensure t
  :bind
  (("C-x C-i" . counsel-imenu)))

(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper-isearch)))


(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package comint
  :config
  (defun comint-clear-buffer ()
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))

  (bind-keys :map comint-mode-map
             ("C-c M-o" . comint-clear-buffer)))

;;--------------------------------------------------------------------
;; org-mode

(use-package org
  :config
  (unbind-key "M-e" org-mode-map)
  (setq org-directory "~/notes"))

(use-package ob
  :init
  :config
  (setq org-confirm-babel-evaluate nil)
  ;;(setq org-export-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)
     (clojure . t))))

;; (use-package ox-reveal
;;   :ensure t)

;; (defun ox-reveal-export-html-after-save-hook ()
;;   (interactive)
;;   (when (eq major-mode 'org-mode)
;;     (org-reveal-export-to-html)))

;; (add-hook 'after-save-hook
;;           'ox-reveal-export-html-after-save-hook)

;;--------------------------------------------------------------------
;; dired

(use-package dired
  :config
  (setq dired-dwim-target t)
  ;;(setq insert-directory-program "/usr/local/bin/gls")
  )

(use-package dired-x)
(use-package wdired)


;;--------------------------------------------------------------------
;; projectile

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-commander)))

;;--------------------------------------------------------------------
;; company

(use-package yasnippet
  :ensure t)

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-idle-delay nil)
  (global-company-mode)
  ;;(add-to-list 'company-backends 'company-clang)
  )

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (require 'lsp-clients))

;; (use-package lsp-ui
;;   :ensure t)

(use-package company-lsp
  :ensure t
  :config
  (add-to-list 'company-backends 'company-lsp)
  (setq company-lsp-enable-snippet nil))


(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package browse-kill-ring
  :ensure t)

(use-package expand-region
  :ensure t
  :bind
  (("C-c '" . er/expand-region)))

(use-package multiple-cursors
  :ensure t)


(use-package nix-mode
  :ensure t)

;;--------------------------------------------------------------------
;; programming?

(use-package cmake-mode
  :ensure t)


;;--------------------------------------------------------------------
;; clojure

(use-package paredit
  :ensure t
  :config
  (enable-paredit-mode))

(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook (lambda () (paredit-mode 1)))
  (setq clojure-indent-style :align-arguments)
  :config
  (put-clojure-indent 'match 'defun)
  (put-clojure-indent 'defrecord 'defun)
  (put-clojure-indent 'alt!! 'defun)
  (put-clojure-indent 'alt! 'defun)
  (put-clojure-indent 'fnk 'defun)
  (put-clojure-indent 'context* 'defun)
  (put-clojure-indent 'GET* 'defun)
  (put-clojure-indent 'POST* 'defun)
  (put-clojure-indent 'PUT* 'defun)
  (put-clojure-indent 'DELETE* 'defun))

(defun inf-clojure-load-complete ()
  "Load clojure completion support."
  (interactive)
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat user-emacs-directory "clojure/complete.clj")))
    (inf-clojure-eval-buffer)))

(use-package inf-clojure
  :ensure t
  )

;; (use-package cider
;;   :ensure t
;;   :init
;;   (add-hook 'cider-mode-hook #'eldoc-mode)
;;   (add-hook 'cider-repl-mode-hook (lambda () (paredit-mode 1)))
;;   ;(setq eldoc-idle-delay 0.1)
;;   (setq cider-repl-pop-to-buffer-on-connect nil)
;;   (setq cider-popup-stacktraces t)
;;   (setq cider-repl-popup-stacktraces t)
;;   (setq cider-auto-select-error-buffer t)
;;   (setq cider-use-overlays nil)
;;   (setq cider-repl-display-in-current-window nil)
;;   (setq cider-repl-prompt-function #'cider-repl-prompt-abbreviated)
;;   (setq cider-repl-display-help-banner nil)
;;   (setq cider-repl-tab-command #'company-indent-or-complete-common)

;;   :config
;;   (defun cider-reset-system ()
;;     (interactive)
;;     (message "Resetting")
;;     (if (not (cider-connected-p))
;;         (message "Not connected to repl")
;;       (cider-nrepl-request:eval "(reset)" (cider-interactive-eval-handler (current-buffer)) "user")))
;;   (bind-keys :map cider-mode-map
;;              ("C-c C-r" . cider-reset-system))
;;   (bind-keys :map cider-repl-mode-map
;;              ("C-c C-r" . cider-reset-system)))

;; (defun cider--local-listening-tcp-ports ()
;;   (mapcar (lambda (s)
;;             (destructuring-bind (pid addr) (read s)
;;               (list pid (cadr (split-string addr ":")))))
;;           (split-string
;;            (shell-command-to-string "lsof -nP -iTCP@0.0.0.0 -sTCP:LISTEN | tail -n +2 | awk '{ printf \"(\\\"\%s\\\" \\\"\%s\\\")\\n\", $2, $9; }'")
;;            "\n" t)))

;; (defun cider--local-jvms ()
;;   (mapcar (lambda (s)
;;             (let ((parts (split-string s " ")))
;;               (list (car parts) (string-join (cdr parts) " "))))
;;           (split-string
;;            (shell-command-to-string "jps -lm | sed /sun.tools.jps.Jps/d")
;;            "\n" t)))

;; (defun cider--local-jvm-endpoints ()
;;   (let* ((local-ports (cider--local-listening-tcp-ports))
;;          (local-jvms (cider--local-jvms))
;;          (local-endpoints (mapcar (lambda (v)
;;                                    (destructuring-bind (pid port) v
;;                                      (list port pid (cadr (assoc pid local-jvms)))))
;;                                  local-ports)))
;;     (remove-if-not (lambda (v) (car (last v))) local-endpoints)))


;; (defun cider-select-local-endpoint ()
;;   (let* ((endpoints (mapcar (lambda (v)
;;                              (destructuring-bind (port pid main) v
;;                                (list (format "%s: %s @ %s" pid main port) port)))
;;                            (cider--local-jvm-endpoints)))
;;          (selection (completing-read "Endpoint: " endpoints)))
;;     (cdr (assoc selection endpoints))))

;; (defun cider-local (port)
;;   (interactive (cider-select-local-endpoint))
;;   (cider-connect "0.0.0.0" port))

;;--------------------------------------------------------------------
;; elisp

(use-package elisp-slime-nav
  :ensure t
  :diminish
  :hook emacs-lisp-mode-hook)

;; (use-package powerline
;;   :ensure t
;;   :config
;;   ;;(powerline-revert)

;;   (powerline-default-theme)
;;   )

;; scheme

(use-package racket-mode
  :ensure t)

;;--------------------------------------------------------------------
;; R
;; (use-package ess
;;   :ensure t
;;   :init
;;   ;(unbind-key "M-j" ess-mode-map)
;;   ;(unbind-key "M-e" ess-mode-map)
;;   )


;;--------------------------------------------------------------------
;; rust

(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp)
  :config
  (rust-enable-format-on-save)
  (flymake-mode-off))

(use-package toml-mode
  :ensure t)

(use-package cargo
  :ensure t
  :init
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :init
  :hook (flycheck-mode . flycheck-rust-setup))


;;--------------------------------------------------------------------
;; haskell

(use-package haskell-mode
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'company-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-process-type 'cabal-repl)
  (setq haskell-process-log t)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-tags-on-save t)
  (setq haskell-process-show-debug-tips nil)

  :config
  (use-package haskell-interactive-mode)
  (use-package haskell-process)
  (bind-keys :map haskell-mode-map
             ("M-." . 'haskell-mode-jump-to-def-or-tag)))


(use-package magit
  :ensure t
  :init
  (setq vc-handled-backends nil))

(use-package csv-mode
  :ensure t)

(use-package go-mode
  :ensure t
  :init
  (setq gofmt-command "goimports")
  :hook
  (before-save . gofmt-before-save)
  (go-mode-hook . lsp))




;; (use-package company-go
;;   :after (company-mode go-mode)
;;   :ensure t
;;   :init
;;   (add-to-list 'company-backends 'company-go))






;; (use-package irony
;;   :ensure t
;;   :init
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'objc-mode-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; (use-package company-irony
;;   :ensure t
;;   :init
;;   (add-to-list 'company-backends 'company-irony))

;; (use-package company-irony-c-headers
;;   :ensure t
;;   :init
;;   (add-to-list 'company-backends 'company-irony-c-headers))

;; (use-package cmake-ide
;;   :ensure t
;;   :init
;;   (cmake-ide-setup))

;; (use-package java-mode
;;   :config
;;   ;(unbind-key "M-e" c-mode-base-map)
;;   ;(unbind-key "M-j" c-mode-base-map)
;;   )

;; (use-package javascript-mode
;;   :init
;;   (setq js-indent-level 2))

(use-package typescript-mode
  :ensure t)

(defun clang-format-before-save ()
  (interactive)
  (when (or (eq major-mode 'c-mode)
            (eq major-mode 'cc-mode))
    (clang-format-buffer)))

(use-package cc-mode
  :init
  ;; (use-package semantic-mode
  ;;   :init
  ;;   (bind-key "M-." #'semantic-ia-fast-jump)
  ;;   (bind-key "M-," #'pop-global-mark))
  (add-hook 'before-save-hook #'clang-format-before-save)
  :config
  (bind-keys :map c-mode-map
             ("C-c C-k" . desperately-compile))
  (unbind-key "M-e" c-mode-map)
  (unbind-key "M-j" c-mode-map))

(use-package clang-format
  :ensure t
  :config)

(use-package markdown-mode
  :ensure t)

(use-package nim-mode
  :ensure t)

(use-package lua-mode
  :ensure t
  :config
  (setq lua-default-application "luajit"))

(use-package js2-mode
  :ensure t
  :init
  (setq js2-basic-offset 2)
  (setq-default js2-enter-indents-newline nil)
  (setq-default js2-idle-timer-delay 0.1)
  (setq-default js2-indent-on-enter-key nil)
  (setq-default js2-mirror-mode nil)
  (setq-default js2-strict-inconsistent-return-warning nil)
  (setq-default js2-auto-indent-p t)
  :config
  (unbind-key "M-j" js2-mode-map))

(use-package rjsx-mode
  :ensure t)

(use-package tuareg
  :ensure t)

(use-package zig-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package python-mode
  :ensure t)

(use-package protobuf-mode
  :ensure t)


(use-package restclient
  :ensure t)
  
(use-package elm-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package elpy
  :ensure t
  :init
  (defun python-format-before-save-hook ()
    (elpy-format-code))
  
  (add-hook 'elpy-mode-hook
            (lambda () (add-hook 'before-save-hook
                                 #'python-format-before-save-hook nil
                                 'local)))
  :config
  (setq elpy-modules '(elpy-module-sane-defaults
                       elpy-module-company
                       elpy-module-eldoc
                       elpy-module-pyvenv))
  :bind (:map python-mode-map
              ("C-x C-e" . elpy-shell-send-statement-and-step)
              ("C-c C-k" . elpy-shell-send-buffer)
              ("C-c C-z" . elpy-shell-switch-to-shell)))

;; (use-package ruby-mode
;;   :ensure t
;;   :config
;;   ;; (setq ruby-align-chained-calls nil)
;;   ;; (setq ruby-align-to-stmt-keywords nil)
;;   ;; (setq ruby-deep-indent-paren nil)
;;   ;; (setq ruby-deep-indent-paren-style nil)
;;   ;; (setq ruby-use-smie t)
;;   (add-hook 'ruby-mode-hook #'subword-mode))

;; (use-package enh-ruby-mode
;;   :ensure t
;;   :mode "\\.rb\\'"
;;   :init
;;   (setq enh-ruby-deep-indent-paren nil))

;; (use-package shen-mode
;;   :ensure t)


;; Visual regexp
;(use-package visual-regexp-mode)
;(require 'visual-regexp)
;(define-key global-map (kbd "M-&") 'vr/query-replace)
;(define-key global-map (kbd "M-/") 'vr/replace)


;; Functions (load all files in defuns-dir)
(defvar defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup key bindings


;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)


;;; --------------------------------------------------
;;; CURSOR MOVEMENTS

;; Single char cursor movement
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-k") 'next-line)

;; Move by word
(global-set-key (kbd "M-u") 'backward-word)
;(global-set-key (kbd "M-U") 'backward-sexp)
(global-set-key (kbd "M-o") 'forward-word) ; was (prefix)
;(global-set-key (kbd "M-O") 'forward-sexp) ; was (prefix)

;; Move by paragraph
;;(global-set-key (kbd "M-S-U") 'scroll-down)
;;(global-set-key (kbd "M-S-O") 'scroll-up)

;; Move by screen (page up/down)
(global-set-key (kbd "M-I") 'backward-paragraph)
(global-set-key (kbd "M-K") 'forward-paragraph)


(global-set-key (kbd "M-e") 'backward-kill-word)
(global-set-key (kbd "M-r") 'kill-word)
(global-set-key (kbd "M-D") 'delete-indentation)

;; (defun maybe-toggle-rectangle-mark (&optional arg)
;;   (interactive "P")
;;   (if (or (not mark-active) arg)
;;       (set-mark-command arg)
;;     (toggle-rectangle-mark)))

(defun maybe-toggle-multiple-cursors (&optional arg)
   (interactive "P")
   (if (or (not mark-active) arg)
       (set-mark-command arg)
     (mc/edit-lines)))

(global-set-key (kbd "C-@") 'maybe-toggle-multiple-cursors)


;; M-i for back-to-indentation
;; (global-set-key (kbd "M-i") 'back-to-indentation)

(global-set-key (kbd "C-c C-l") 'indent-buffer)

;; Turn on the menu bar for exploring new modes
;;(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Use shell-like backspace C-h, rebind help to F1
;;(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

                                        ;(global-set-key (kbd "M-h") 'kill-region-or-backward-word)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Change next underscore with a camel case
;; (global-set-key (kbd "C-c C--") 'replace-next-underscore-with-camel)
;; (global-set-key (kbd "M-s M--") 'snakeify-current-word)

;; Change word separators
;; (global-unset-key (kbd "C-x +")) ;; used to be balance-windows
;; (global-set-key (kbd "C-x + -") (λ (replace-region-by 's-dashed-words)))
;; (global-set-key (kbd "C-x + _") (λ (replace-region-by 's-snake-case)))
;; (global-set-key (kbd "C-x + c") (λ (replace-region-by 's-lower-camel-case)))
;; (global-set-key (kbd "C-x + C") (λ (replace-region-by 's-upper-camel-case)))

;; Killing text
;; (global-set-key (kbd "C-S-k") 'kill-and-retry-line)
;; (global-set-key (kbd "C-w") 'kill-region-or-backward-word)
;; (global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)

;; Use M-w for copy-line if no active region
;; (global-set-key (kbd "M-w") 'save-region-or-current-line)
;; (global-set-key (kbd "s-w") 'save-region-or-current-line)
;; (global-set-key (kbd "M-W") (λ (save-region-or-current-line 1)))

;; Make shell more convenient, and suspend-frame less
(global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "C-x M-z") 'suspend-frame)

;; Zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char)
;;(global-set-key (kbd "s-z") (lambda (char) (interactive "cZap up to char backwards: ") (zap-up-to-char -1 char)))
;;(global-set-key (kbd "M-Z") (lambda (char) (interactive "cZap to char: ") (zap-to-char 1 char)))
;;(global-set-key (kbd "s-Z") (lambda (char) (interactive "cZap to char backwards: ") (zap-to-char -1 char)))

;; iy-go-to-char - like f in Vim
                                        ;(global-set-key (kbd "M-m") 'jump-char-forward)
                                        ;(global-set-key (kbd "M-S-M") 'jump-char-backward)
                                        ;(global-set-key (kbd "s-m") 'jump-char-backward)

;; vim's ci and co commands
;; (global-set-key (kbd "M-I") 'change-inner)
;; (global-set-key (kbd "M-O") 'change-outer)

;; (global-set-key (kbd "s-i") 'copy-inner)
;; (global-set-key (kbd "s-o") 'copy-outer)

;; Create new frame
                                        ;(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; Jump to a definition in the current file. (This is awesome)

;; File finding
;; (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
;; (global-set-key (kbd "C-x f") 'recentf-ido-find-file)
;; (global-set-key (kbd "C-x C-p") 'find-or-create-file-at-point)
;; (global-set-key (kbd "C-x M-p") 'find-or-create-file-at-point-other-window)
;; (global-set-key (kbd "C-c y") 'bury-buffer)
;; (global-set-key (kbd "C-c r") 'revert-buffer)
;; (global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
;;(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle two most recent buffers
;; (fset 'quick-switch-buffer [?\C-x ?b return])
;; (global-set-key (kbd "s-b") 'quick-switch-buffer)
;; (global-set-key (kbd "s-y") 'bury-buffer)

;; Revert without any fuss
                                        ;(global-set-key (kbd "M-<escape>") (lambda () (revert-buffer t t)))

(global-set-key (kbd "M-<SPC>") 'company-complete)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

;; Edit file with sudo
                                        ;(global-set-key (kbd "M-s e") 'sudo-edit)

;; Copy file path to kill ring
                                        ;(global-set-key (kbd "C-x M-w") 'copy-current-file-path)


;; Window switching
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)
                                        ;(global-unset-key (kbd "C-x C-+")) ;; don't zoom like this

                                        ;(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

;; Add region to *multifile*
                                        ;(global-set-key (kbd "C-!") 'mf/mirror-region-in-multifile)

;; Indentation help
                                        ;(global-set-key (kbd "M-j") (λ (join-line -1)))

;; Help should search more than just commands
                                        ;(global-set-key (kbd "<f1> a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; Navigation bindings
;; (global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "<prior>") 'scroll-down-command)
(global-set-key (kbd "<next>") 'scroll-up-command)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; (global-set-key (kbd "M-<up>") 'smart-up)
;; (global-set-key (kbd "M-<down>") 'smart-down)
;; (global-set-key (kbd "M-<left>") 'smart-backward)
;; (global-set-key (kbd "M-<right>") 'smart-forward)

;; Webjump let's you quickly search google, wikipedia, emacs wiki
;; (global-set-key (kbd "C-x g") 'webjump)
;; (global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; Completion at point
;; (global-set-key (kbd "C-<tab>") 'completion-at-point)

;; Like isearch, but adds region (if any) to history and deactivates mark
                                        ;(global-set-key (kbd "C-s") 'isearch-forward-use-region)
                                        ;(global-set-key (kbd "C-r") 'isearch-backward-use-region)

;; Like isearch-*-use-region, but doesn't fuck with the active region
;; (global-set-key (kbd "C-S-s") 'isearch-forward)
;; (global-set-key (kbd "C-S-r") 'isearch-backward)

;; Move more quickly
;; (global-set-key (kbd "C-S-n") (λ (ignore-errors (next-line 5))))
;; (global-set-key (kbd "C-S-p") (λ (ignore-errors (previous-line 5))))
;; (global-set-key (kbd "C-S-f") (λ (ignore-errors (forward-char 5))))
;; (global-set-key (kbd "C-S-b") (λ (ignore-errors (backward-char 5))))

;; (global-set-key (kbd "H-*") 'beginning-of-buffer) ;; H-p
;; (global-set-key (kbd "H-n") 'end-of-buffer)

;; Query replace regex key binding
(global-set-key (kbd "M-&") 'query-replace-regexp)

;; Yank selection in isearch
                                        ;(define-key isearch-mode-map (kbd "C-o") 'isearch-yank-selection)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Eval buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; Create scratch buffer
;;(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;; Move windows, even in org-mode
;; (global-set-key (kbd "<s-right>") 'windmove-right)
;; (global-set-key (kbd "<s-left>") 'windmove-left)
;; (global-set-key (kbd "<s-up>") 'windmove-up)
;; (global-set-key (kbd "<s-down>") 'windmove-down)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status)
(autoload 'magit-status "magit")

;; Mu4e
                                        ;(global-set-key (kbd "C-x M") 'mu4e-up-to-date-status)

;; Clever newlines
;; (global-set-key (kbd "C-o") 'open-line-and-indent)
;; (global-set-key (kbd "<C-return>") 'open-line-below)
;; (global-set-key (kbd "<C-S-return>") 'open-line-above)
;; (global-set-key (kbd "<M-return>") 'new-line-dwim)

;; Duplicate region
;; (global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Line movement
                                        ;(global-set-key (kbd "C-S-<down>") 'move-text-down)
                                        ;(global-set-key (kbd "C-S-<up>") 'move-text-up)

;; Fold the active region
;; (global-set-key (kbd "C-c C-f") 'fold-this-all)
;; (global-set-key (kbd "C-c C-F") 'fold-this)
;; (global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)

;; Yank and indent
                                        ;(global-set-key (kbd "C-S-y") 'yank-unindented)

;; Toggle quotes
                                        ;(global-set-key (kbd "C-\"") 'toggle-quotes)

;; Sorting
                                        ;(global-set-key (kbd "M-s l") 'sort-lines)

;; Increase number at point (or other change based on prefix arg)
;; (global-set-key (kbd "C-+") 'change-number-at-point)
;; (global-set-key (kbd "C-?") 'subtract-number-at-point)
;; (eval-after-load 'undo-tree '(define-key undo-tree-map (kbd "C-?") nil))

;; Browse the kill ring
;; (global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;; Buffer file functions
                                        ;(global-set-key (kbd "C-x t") 'touch-buffer-file)
                                        ;(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
                                        ;(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; Jump from file to containing directory
(global-set-key (kbd "C-x C-j") 'dired-jump) (autoload 'dired-jump "dired")
                                        ;(global-set-key (kbd "C-x M-j") '(λ (dired-jump 1)))

;; Easy-mode fullscreen rgrep
(global-set-key (kbd "C-c C-a") 'ag-project)
;; (global-set-key (kbd "M-s s") 'git-grep-fullscreen)
;; (global-set-key (kbd "M-s S") 'rgrep-fullscreen)

;; Multi-occur
;; (global-set-key (kbd "M-s m") 'multi-occur)
;; (global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;; Find files by name and display results in dired
;; (global-set-key (kbd "M-s f") 'find-name-dired)

;; Find file in project
;;(global-set-key (kbd "C-x o") 'find-file-in-project)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)


(defun pretty-format-xml ()
  (interactive)
  (shell-command-on-region (point-min)
                           (point-max)
                           "xmllint --format - "
                           (current-buffer)
                           t))

(defun pretty-format-json ()
  (interactive)
  (shell-command-on-region (point-min)
                           (point-max)
                           "jq ."
                           (current-buffer)
                           t))

(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))






;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(set-register ?i `(file . ,(buffer-file-name)))
;;(set-register ?n '(file . "/sudo:localhost:/etc/nixos/configuration.nix"))


;; timing

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed)))
          t)

(provide 'init)
;;; init ends here
