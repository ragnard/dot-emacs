;; -*- lexical-binding: t; flycheck-disabled-checkers: (emacs-lisp-checkdoc emacs-lisp-package); -*-
;;------------------------------------------------------------------------------
;; My init.
;;
;; Lots stolen from John Wiegley's dot-emacs: https://github.com/jwiegley/dot-emacs
;;

(defconst emacs-start-time (current-time))

(setq gc-cons-percentage 0.5
      gc-cons-threshold (* 128 1024 1024))

(add-hook 'after-init-hook #'garbage-collect t)

(defun report-time-since-load (&optional suffix)
  (message "Loading init...done (%.3fs)%s"
           (float-time (time-subtract (current-time) emacs-start-time))
           suffix))

(add-hook 'after-init-hook
          #'(lambda () (report-time-since-load " [after-init]"))
          t)

(eval-and-compile
  (defsubst emacs-path (path)
    (expand-file-name path user-emacs-directory))

  (setq package-enable-at-startup nil
        load-path
        (append (list (emacs-path "use-package"))
                (delete-dups load-path)
                (list (emacs-path "lisp"))
                (list (emacs-path "local")))))

(require 'use-package)

(setq use-package-verbose init-file-debug
      use-package-expand-minimally (not init-file-debug)
      use-package-compute-statistics nil
      debug-on-error init-file-debug)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)


(defun reload-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))


;;------------------------------------------------------------------------------
;; Emacs environment

(defconst emacs-environment (or (getenv "EMACS_HOME") "~/.emacs.d/"))

(defconst user-data-directory
  (emacs-path "data"))

(defun user-data (dir)
  (expand-file-name dir user-data-directory))

(use-package emacs
  :custom

  ;; C source code
  (auto-hscroll-mode t)
  (auto-save-interval 64)
  (auto-save-timeout 2)
  (create-lockfiles nil)
  (enable-recursive-minibuffers t)
  (truncate-lines t)
  ;; (fill-column 78)
  (history-delete-duplicates t)
  (history-length 200)
  (load-prefer-newer t)
  (menu-bar-mode nil)
  (message-log-max 16384)
  (redisplay-dont-pause t)
  (tool-bar-mode nil)
  (undo-limit 800000)
  (use-short-answers t)
  (visible-bell nil)
  (x-stretch-cursor t)
  (show-trailing-whitespace nil)

  ;; scroll-bar
  (scroll-bar-mode nil)

  ;; startup.el
  (auto-save-list-file-prefix (user-data "auto-save-list/.saves-"))
  (inhibit-startup-screen t)
  (initial-buffer-choice nil)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message "Oink!")

  ;; files.el
  (backup-directory-alist `(("." . ,(user-data "backups"))))
  (backup-by-copying t)
  (delete-old-versions t)
  (large-file-warning-threshold nil)
  (require-final-newline t)
  (save-abbrevs 'silently)
  (trash-directory "~/.trash")
  (version-control t)

  ;; simple.el
  (backward-delete-char-untabify-method 'untabify)
  (column-number-mode t)
  (indent-tabs-mode nil)
  (kill-do-not-save-duplicates t)
  (kill-ring-max 500)
  (kill-whole-line t)
  (line-number-mode t)
  (next-line-add-newlines nil)
  (save-interprogram-paste-before-kill t)
  (shift-select-mode nil)

  ;; paragraphs.el
  (sentence-end-double-space nil)

  ;; paren.el
  (show-paren-delay 0)

  ;; frame.el
  ;; (window-divider-default-bottom-width 1)
  ;; (window-divider-default-places 'bottom-only)

  ;; indent.el
  (tab-always-indent 'complete)

  ;; cus-edit.el
  (custom-file (user-data "settings.el"))

  :preface
  (defun next-defun ()
    (interactive)
    (beginning-of-defun -1))

  (defun previous-defun ()
    (interactive)
    (beginning-of-defun))

  (defun indent-buffer ()
    (interactive)
    (indent-region (point-min) (point-max)))

  (defun cleanup-buffer ()
    "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on
save."
    (interactive)
    (untabify-buffer)
    (delete-trailing-whitespace)
    (indent-buffer))

  :config
  (set-register ?i `(file . ,user-init-file))

  :bind
  (("M-i" . previous-line)
   ("M-k" . next-line)
   ("M-I" . backward-paragraph)
   ("M-K" . forward-paragraph)
   ("M-j" . left-char)
   ("M-l" . right-char)
   ("M-o" . forward-word)
   ("M-u" . backward-word)
   ("M-e" . backward-kill-word)
   ("M-r" . kill-word)
   ("M-ö" . change-inner)
   ("M-Ö" . change-outer)
   ("C-x -" . transpose-frame)
   ("C-c C-l" . indent-buffer)
   ("C-c l" . mc/edit-lines)

   :map prog-mode-map
   ("M-p" . previous-defun)
   ("M-n" . next-defun)))


;;------------------------------------------------------------------------------
;; Theme

(use-package catppuccin-theme
  :ensure t
  :preface
  (load-theme 'catppuccin :no-confirm)
  :custom-face
  (default ((t (:family "Iosevka Term SS08" :height 150))))
  (variable-pitch ((t (:family "Iosevka SS08"))))
  (fixed-pitch ((t (:family "Iosevka SS08"))))
  (cursor ((t (:background "#f38ba8"))))
  (show-paren-match ((t (:background "#f38ba8"))))
  (vertical-border ((t (:background "#11111b" :foreground "#11111b"))))
  (region ((t (:background "#313244"))))
  (font-lock-builtin-face ((t (:foreground ,(catppuccin-color 'sapphire)))))
  (mode-line ((t (:box (:line-width 1 :color ,(catppuccin-color 'crust))))))
  (mode-line-active ((t (:box (:line-width 1 :color ,(catppuccin-color 'blue))))))
  (mode-line-inactive ((t (:box (:line-width 1 :color ,(catppuccin-color 'crust)))))))

;;------------------------------------------------------------------------------
;; Packages

(use-package autorevert
  :diminish
  :init
  (global-auto-revert-mode))

(use-package browse-kill-ring
  :ensure t)

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c c" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup))

(use-package cider
  :ensure t
  :defer
  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook (lambda () (paredit-mode 1)))
                                        ;(setq eldoc-idle-delay 0.1)
  :custom
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-popup-stacktraces t)
  (cider-repl-popup-stacktraces t)
  (cider-auto-select-error-buffer t)
  (cider-use-overlays nil)
  (cider-repl-display-in-current-window nil)
  (cider-repl-prompt-function #'cider-repl-prompt-abbreviated)
  (cider-repl-display-help-banner nil))

(use-package clojure-mode
  :ensure t
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
  (put-clojure-indent 'DELETE* 'defun)

  :hook
  (clojure-mode-hook . paredit-mode)

  :custom
  (clojure-indent-style 'align-arguments))

(use-package comint
  :custom
  (comint-scroll-to-bottom-on-input t)
  (comint-move-point-for-output t)
  (comint-prompt-read-only t)
  :config
  (defun comint-clear-buffer ()
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))

  :bind
  (:map comint-mode-map
        ("C-c M-o" . comint-clear-buffer)))

(use-package consult
  :ensure t
  :custom
  (consult-line-start-from-top t)
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("C-x i" . consult-imenu)
   ("C-x f" . consult-flymake)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-y" . consult-yank-pop)))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t))

(use-package corfu-terminal
  :ensure t
  :diminish
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode)))

(use-package csv-mode
  :ensure t
  :defer)

(use-package delsel
  :init
  (delete-selection-mode))

(use-package diminish
  :ensure t)

(use-package dired
  :init
  (use-package dired-x)
  (use-package wdired)
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-ahlv --group-directories-first"))

(use-package ediff
  :custom
  (ediff-diff-options "-w")
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :bind
  (("C-c h" . eldoc))
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package elisp-mode
  :mode (("\\.el\\'" . emacs-lisp-mode))
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-k" . eval-buffer)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "OPENAI_API_KEY"))

(use-package expand-region
  :ensure t
  :bind
  (("C-c '" . er/expand-region)))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :bind
  (("C-c C-e" . flycheck-list-errors)))

(use-package flycheck-package
  :ensure t)

;; (use-package flycheck-rust
;;   :ensure t
;;   :init
;;   :hook (flycheck-mode . flycheck-rust-setup))

(use-package go-mode
  :ensure t
  :custom
  (gofmt-command "/usr/local/bin/goimports")
  :hook
  (before-save . gofmt-before-save))

(use-package goto-addr
  :config
  (global-goto-address-mode))

(use-package gptel
  :ensure t
  :config
  (setq gptel-api-key (lambda ()
                        (getenv "OPENAI_API_KEY"))))

(use-package gptel-magit
  :ensure t
  :after gptel
  :hook (magit-mode . gptel-magit-install))

(use-package htmlize
  :ensure t)

(use-package imenu
  :hook
  (emacs-lisp-mode . (lambda ()
                       (add-to-list 'imenu-generic-expression
                                    '("Packages" "^\\s-*(use-package\\s-+\\([A-Za-z0-9+-]+\\)" 1)))))

(use-package js
  :commands js-mode
  :custom
  (js-indent-level 2))

(use-package js2-mode
  :defer
  :custom
  (js2-basic-offset 2))

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)))

(use-package just-mode
  :ensure t)

(use-package jq-mode
  :ensure t
  :bind
  (:map json-mode-map
        ("C-c C-j" . jq-interactively)))

(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode))
  :custom
  (lua-default-application "lua"))

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-completion-provider :none)
  (lsp-signature-auto-activate nil)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((rust-mode . lsp)
         (rust-ts-mode . lsp)
         (go-mode . lsp)
         (go-ts-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (typescript-mode . lsp)
         (typescript-ts-mode . lsp)
         (web-mode . lsp)
         (python-mode . lsp)
         (python-ts-mode . lsp))
  :bind (("M-RET" . lsp-execute-code-action))
  :commands lsp)

(use-package lsp-pyright
  :ensure t
  :custom
  (lsp-pyright-langserver-command "basedpyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))


;; (use-package lsp-flycheck
;;   :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x m" . magit-status))

(use-package magit-svn
  :ensure t
  :after magit
  :defer)

(use-package man
  :custom
  (Man-notify-method 'pushy))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c C-d" . mc/mark-next-like-this)
   ("C-c C-a" . mc/mark-all-like-this)))

(use-package nhexl-mode
  :ensure t
  :defer
  :bind
  ("<f8>" . nhexl-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package org
  :ensure t
  :requires htmlize
  :defer
  :config
  ;; (unbind-key "M-e" org-mode-map)
  (setq org-directory "~/notes"))

(use-package package-lint
  :ensure t)

(use-package package-lint-flymake
  :ensure t)

(use-package paredit
  :ensure t
  :diminish
  :hook
  ((lisp-mode emacs-lisp-mode) . paredit-mode))

(use-package poke-mode
  :ensure t
  :defer)

(use-package poke
  :ensure t
  :commands poke)

(use-package projectile
  :ensure t
  :init
  (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package protobuf-mode
  :ensure t
  :mode (("\\.proto\\'" . protobuf-mode)))

(use-package python-mode
  :ensure t
  :defer
  :custom
  (python-shell-interpreter "uv")
  (python-shell-interpreter-args "run python"))

(use-package recentf
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 100))

(use-package rect
  :bind ("C-c r" . rectangle-mark-mode))

(use-package rego-mode
  :ensure t)

(use-package restclient
  :ensure t
  :requires json-mode)

(use-package restclient-jq
  :ensure t
  :requires json-mode
  :after restclient)

(use-package rg
  :ensure t)

(use-package rust-mode
  :ensure t
  :bind
  (:map rust-mode-map
        (("C-c C-k" . rust-compile)
         ("C-c C-t " . rust-test))))


(use-package savehist
  :init
  (savehist-mode 1)
  :custom
  (savehist-file (user-data "history")))

(use-package saveplace
  :init
  (save-place-mode 1)

  :custom
  (save-place-file (user-data "save-place")))

(use-package selected
  :ensure t
  :demand t
  :diminish selected-minor-mode
  :commands selected-minor-mode
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("w" . count-words-region)
              ("e" . mc/edit-lines))
  :config
  (selected-global-mode 1))

(use-package smartparens-mode
  :ensure smartparens
  :pin melpa
  :diminish smartparens-mode
  :commands smartparens-mode
  :hook (prog-mode text-mode markdown-mode)
  :bind
  ("C-<right>" . sp-forward-slurp-sexp)
  ("C-<left>" . sp-forward-barf-sexp)
  ("M-s" . sp-splice-sexp)
  :config
  (require 'smartparens-config))

(use-package sql
  :ensure t
  :defer
  :preface
  (defun sql-send-string-goto-eoi (_ _)
    (comint-goto-process-mark))
  :config
  (advice-add 'sql-input-sender :before #'sql-send-string-goto-eoi))

(use-package sql-clickhouse
  :ensure t
  :defer
  :custom
  (sql-clickhouse-options nil))

(use-package toml-mode
  :ensure t
  :defer)

(use-package transient
  :defer t
  :custom
  (transient-history-file (user-data "transient/history.el"))
  (transient-values-file (user-data "transient/values.el")))

(use-package transpose-frame
  :ensure t
  :commands (transpose-frame rotate-frame))

(use-package tree-sitter
  :ensure t
  :defer
  :diminish
  :init
  ;; (global-tree-sitter-mode)
  ;; :custom
  ;; (tsc-dyn-get-from '(:compilation))
  )

(use-package treesit-auto
  :ensure t
  :commands (global-treesit-auto-mode)
  :config
  (global-treesit-auto-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist `(("." . ,(user-data "undo-tree")))))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

(use-package visual-regexp
  :ensure t
  :bind
  ("M-%" . vr/query-replace))

(use-package vterm
  :ensure t)

(use-package web-mode
  :ensure t
  :mode (("\\.svelte\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (unbind-key (kbd "C-c C-l") web-mode-map))

(use-package wgrep
  :ensure t)

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish
  :init
  (global-whitespace-cleanup-mode))

(use-package winner
  :init
  (winner-mode 1))

(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings))

(use-package yaml-mode
  :ensure t
  :mode (("\\.ya?ml\\'" . yaml-mode)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  :hook
  ((lsp-mode . yas-minor-mode)
   (prog-mode-hook . yas-minor-mode)))

(use-package zig-mode
  :ensure t
  :mode (("\\.zig\\'" . zig-mode)))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
