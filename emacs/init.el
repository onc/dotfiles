;; package --- Summary

;; -*- coding: utf-8; lexical-binding: t -*

;;; Commentary:

;; Emacs configuration of Christian van Onzenoodt.

;;   ___  _ __   ___ _ __ ___   __ _  ___ ___
;;  / _ \| '_ \ / __| '_ ` _ \ / _` |/ __/ __|
;; | (_) | | | | (__| | | | | | (_| | (__\__ \
;;  \___/|_| |_|\___|_| |_| |_|\__,_|\___|___/

;;; Code:


;;; Constants
;;; ---------

;; Path of this config for leader binding
(defconst onc/init-el-path (file-truename (expand-file-name "~/.emacs.d/init.el")))

;; Path of themes
(defconst onc/custom-theme-load-path (expand-file-name "~/.emacs.d/themes"))

;; Font for emacs
(defconst onc/font-family "SF Mono")

;; Size of font
(defconst onc/font-size 100)


;;; Bootstrap package management
;;; ----------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package system-packages
  :straight t
  :init
  (setq system-packages-use-sudo nil)
  (when (eq system-type 'darwin)
    (setq system-packages-package-manager 'brew)))

(use-package use-package-ensure-system-package 
  :straight t)

;; Validation of setq and stuff
(use-package validate
  :straight t
  :commands (validate-value validate-setq))

;; themes
(use-package monokai-theme
  :straight t)


;;; Tweaking
;;; ---------

;; ;; GC tweaking (https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold)
(validate-setq gc-cons-threshold 100000000)

;; Increase the amount of data which Emacs reads from the process (https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process)
(validate-setq read-process-output-max (* 1024 1024)) ;; 1mb

;; If this is non-nil, then when both .el and .elc
;; versions of a file exist, and the caller did not explicitly specify
;; which one to load, then the newer file is loaded.
(validate-setq load-prefer-newer t)


;;; Appearance
;;; ----------

;; Disable toolbar
(tool-bar-mode -1)
;; Disable scrollbar
(scroll-bar-mode -1)
;; Disable menu-bar on anything but macOS
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

;; Disable cursor blink
(blink-cursor-mode 0)

;; Highlight current line
(global-hl-line-mode t)

;; Font
(set-face-attribute 'default nil :family onc/font-family :height onc/font-size :weight 'normal)
(set-face-attribute 'variable-pitch nil :family onc/font-family :height onc/font-size :weight 'normal)

(add-to-list 'custom-theme-load-path onc/custom-theme-load-path)
;; (load-theme 'base16-onc-dark 'no-confirm)
(load-theme 'monokai t)

;; Show system name and full file path in emacs frame title
(validate-setq frame-title-format
               (list (format "%s %%S: %%j " (system-name))
                     '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun onc/colorify (STRING COLOR)
  "Helper-function to colorize a STRING with a COLOR."
  (interactive)
  (propertize STRING 'font-lock-ignore t 'font-lock-face `(:foreground ,COLOR)))

;; Make the scratch buffer great again
(add-hook 'after-init-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (goto-char (point-min))
              (insert (concat
                       "\n"
                       "               " (onc/colorify "  ___  " "OrangeRed") (onc/colorify "_ __   " "orange")   (onc/colorify "___ " "yellow")   (onc/colorify "_ __ ___  " "green") (onc/colorify " __ _  " "cyan") (onc/colorify "___"   "blue") (onc/colorify " ___  \n"   "DarkMagenta")
                       "               " (onc/colorify " / _ \\" "OrangeRed") (onc/colorify "| '_ \\" "orange")   (onc/colorify " / __|" "yellow") (onc/colorify " '_ ` _ \\" "green") (onc/colorify " / _` |" "cyan") (onc/colorify "/ __"  "blue") (onc/colorify "/ __| \n"   "DarkMagenta")
                       "               " (onc/colorify "| (_) "  "OrangeRed") (onc/colorify "| | | | " "orange")  (onc/colorify "(__| " "yellow")  (onc/colorify "| | | | |"  "green") (onc/colorify " (_| | " "cyan") (onc/colorify "(__"   "blue") (onc/colorify "\\__ \\ \n" "DarkMagenta")
                       "               " (onc/colorify " \\___/" "OrangeRed") (onc/colorify "|_| |_|" "orange")   (onc/colorify "\\___|" "yellow") (onc/colorify "_| |_| |_|" "green") (onc/colorify "\\__,_|" "cyan") (onc/colorify "\\___" "blue") (onc/colorify "|___/ \n"   "DarkMagenta")
                       "\n"
                       "   This buffer is for text that is not saved, and for Lisp evaluation.\n"
                       "     To create a file, visit it with \ o and enter text in its buffer.\n"
                       "\n"
                       "\n")))))

;; stfu emacs!!!
(validate-setq ring-bell-function 'ignore)

;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(validate-setq x-stretch-cursor t)


;;; Global keybindings
;;; ------------------

;; macOS keybindings
(when (eq system-type 'darwin)
  (validate-setq mac-option-modifier 'option
                 mac-command-modifier 'meta))

;; macOS smooth scrolling
(when (eq system-type 'darwin)
  (validate-setq mouse-wheel-scroll-amount '(1))
  (validate-setq mouse-wheel-progressive-speed nil))

;; Fix black cursor in emacs-mac
(when (eq system-type 'darwin)
  (set-mouse-color "white"))

;; Map escape to cancel (like C-g)...
(define-key isearch-mode-map [escape] 'isearch-abort)   ;; isearch
(global-set-key [escape] 'keyboard-escape-quit)         ;; everywhere else

;; Revert buffer
(global-set-key [f5]
                (lambda ()
                  (interactive)
                  (revert-buffer nil t)
                  (message "Buffer reverted")))

;; Keyboard shortcuts for resizing windows
(global-set-key (kbd "<C-s-left>") (lambda () (interactive) (shrink-window-horizontally 5)))
(global-set-key (kbd "<C-s-right>") (lambda () (interactive) (enlarge-window-horizontally 5)))
(global-set-key (kbd "<C-s-down>") (lambda () (interactive) (shrink-window 5)))
(global-set-key (kbd "<C-s-up>") (lambda () (interactive) (enlarge-window 5)))

;; Unconditionally kill unmodified buffers.
(global-set-key (kbd "C-x k") 'onc/kill-this-buffer-if-not-modified)

;; Adjust font size
(global-set-key (kbd "s-+") 'onc/increase-default-font-height)
(global-set-key (kbd "s--") 'onc/decrease-default-font-height)


;;; Files
;;; -----

;; Fucking use spaces emacs
(setq-default tab-width 4 indent-tabs-mode nil)

;; From http://emacswiki.org/emacs/AutoSave
(validate-setq
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Don't create lockfiles
(validate-setq create-lockfiles nil)

;; Don't make me type 'yes' or 'no', y/n will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Dont ask to follow symlink in git
(validate-setq vc-follow-symlinks t)

;; Redraw completly before continue to avoid lagging
(validate-setq redisplay-dont-pause t)

;; Nonzero means echo unfinished commands after this many seconds of pause.
(validate-setq echo-keystrokes 0.02)

;; Make sure UTF-8 is used
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; Check (on save) whether the file edited contains a shebang, if yes, make it executable
;; from http://mbork.pl/2015-01-10_A_few_random_Emacs_tips
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(validate-setq scroll-preserve-screen-position 'always)

(set-default 'truncate-lines t)

;;; Dependencies/ Libraries
;;; -----------------------

;; Simple library for asynchronous processing
(use-package async
  :straight t
  :config (require 'async-bytecomp))


;; dash and s required for origami
;; List library
(use-package dash
  :straight t)


;; String library
(use-package s
  :straight t)


(use-package multi
  :straight t)


;; hide minor modes in modeline
(use-package diminish
  :straight t)

;; General packages
;; ----------------

;; keep packages up-to-date
(use-package auto-package-update
  :straight t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; modeline
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-minor-modes t))

(use-package nerd-icons
  :straight t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "SauceCodePro NF"))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package eldoc
  :diminish eldoc-mode)

(use-package hi-lock
  :diminish hi-lock-mode)

;; Emacs in server-mode
(use-package server
  :if (not noninteractive)
  :commands server-start
  :init (server-start)
  :diminish server-buffer-clients)

;; Save buffers
(use-package desktop
  :commands desktop-save-mode
  :init (desktop-save-mode t)
  :custom
  (desktop-auto-save-timeout 60 "Save desktop after one minute of idle")
  (desktop-load-locked-desktop t))

;; Simple
(use-package simple
  :commands (async-shell-command column-number-mode set-variable delete-trailing-whitespace shell-command)
  :custom (column-number-mode t))

;; Fringe mode (left and right borders stuff)
(use-package fringe
  :commands fringe-mode
  :config (fringe-mode '(4 . 0)))

;; Save recent files
(use-package recentf
  :commands recentf-mode
  :init (recentf-mode t)
  :custom (recentf-max-saved-items 1000))

;; Save position in files
(use-package saveplace
  :commands save-place-mode
  :init (save-place-mode t)
  :custom (save-place-file (expand-file-name "places" user-emacs-directory)))


;; Undo
(use-package undo-fu
  :straight t)

(use-package undo-fu-session
  :straight t
  :commands undo-fu-session-global-mode
  :init (undo-fu-session-global-mode)
  :config
  (validate-setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))


;; Auto-revert of changed files
(use-package autorevert
  :straight t
  :commands global-auto-revert-mode
  :init (global-auto-revert-mode t))


;; Insert matching delimiters
(use-package elec-pair
  :straight t
  :commands electric-pair-mode
  :init (electric-pair-mode t))


;; Load shell env
(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))


;; Highlight symbols in Emacs, used in leader binding
(use-package highlight-symbol
  :straight t
  :config
  (highlight-symbol-mode t))


(use-package origami
  :straight t
  :after (dash s)
  :config
  (global-origami-mode t))


(use-package flycheck
  :straight t)


(use-package perspeen
  :straight t
  :config
  (perspeen-mode)
  (custom-set-faces
   '(perspeen-selected-face ((t (:inherit mode-line :foreground "#A6E22E")))))

  ;; remove background as indicator of selected workspace
  ;;(face-remap-add-relative 'perspeen-selected-face '(:background default))
  ;; (set-face-attribute 'perspeen-selected-face nil :foreground "green" :background 'default)
  ;; (set-face-background 'perspeen-selected-face default)
  ;; (set-face-foreground 'perspeen-selected-face "green")
  )


(use-package company
  :straight t
  :commands (global-company-mode company-mode)
  :diminish company-mode
  :bind (:map company-active-map
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous))
  :custom
  ;; no delay no autocomplete
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-tooltip-limit 20)
  (company-tooltip-align-annotations t)

  :init (global-company-mode t)
  :config
  ;; remove unused backends
  (delete 'company-semantic company-backends)
  (delete 'company-eclim company-backends)
  (delete 'company-xcode company-backends)
  (delete 'company-clang company-backends)
  (delete 'company-bbdb company-backends)
  (delete 'company-oddmuse company-backends))


;; Emacs + vim = <3
(use-package evil
  :straight t
  :commands (evil-yank evil-set-initial-state evil-make-overriding-map evil-delay)
  :bind (:map evil-normal-state-map
              ("Y" . copy-to-end-of-line)
              ("j" . evil-next-visual-line)
              ("k" . evil-previous-visual-line)
              ("gj" . evil-next-line)
              ("gk" . evil-previous-line)
              ("C-u" . evil-scroll-up)
              ("C-d" . evil-scroll-down))
  :custom
  (evil-move-cursor-back nil "make the cursor stay in place after exiting insert mode")
  (evil-undo-system 'undo-fu)
  :hook (evil-normal-state . #'add-vim-bindings)
  :init
  (evil-mode t)

  ;; prevent accidential C-z pressed to switch to emacs keybindings, e.g. when trying to press C-x
  (keymap-unset evil-emacs-state-map "C-z")
  (keymap-unset evil-motion-state-map "C-z")
  (keymap-unset evil-insert-state-map "C-z")

  ;; save on double escape, works best with escape mapped to caps-lock.
  (add-hook 'evil-normal-state-entry-hook #'add-vim-bindings)

  :preface
  (defun copy-to-end-of-line ()
    "Yank from point to end of line."
    (interactive)
    (evil-yank (point) (point-at-eol)))

  (defun save-with-escape-and-timeout ()
    "if no second escape is pressed in a given timeout, dont wait for a second escape."
    (interactive)
    (block return-point
      (let ((timer (run-at-time "0.3 sec" nil (lambda () (return-from return-point))))
            (key (read-key)))
        (if (eq key 27)
            (progn
              (cancel-timer timer)
              (save-buffer))))))

  (defun add-vim-bindings()
    (define-key evil-normal-state-local-map (kbd "<escape>") 'save-with-escape-and-timeout))

  ;; unbind C-w from evil-window-map, so I can use it!
  (defun set-control-w-shortcuts ()
    (define-prefix-command 'onc-window-map)
    (global-set-key (kbd "C-w") 'onc-window-map)
    (define-key onc-window-map (kbd "h") 'evil-window-left)
    (define-key onc-window-map (kbd "j") 'evil-window-down)
    (define-key onc-window-map (kbd "k") 'evil-window-up)
    (define-key onc-window-map (kbd "l") 'evil-window-right)
    (define-key onc-window-map (kbd "%") 'split-window-right)
    (define-key onc-window-map (kbd "\"") 'split-window-below)
    (define-key onc-window-map (kbd "x") 'delete-window)
    (define-key onc-window-map (kbd "o") 'delete-other-windows)
    (define-key onc-window-map (kbd "c") 'perspeen-create-ws)
    (define-key onc-window-map (kbd "n") 'perspeen-next-ws)
    (define-key onc-window-map (kbd "p") 'perspeen-previous-ws)
    (define-key onc-window-map (kbd "r") 'perspeen-rename-ws)
    (define-key onc-window-map (kbd "d") 'perspeen-delete-ws))

  :config

  ;; from: https://lists.ourproject.org/pipermail/implementations-list/2014-September/002064.html
  (eval-after-load "evil-maps"
    '(dolist (map (list evil-motion-state-map
                        evil-insert-state-map
                        evil-emacs-state-map))
       (define-key map "\C-w" nil)
       (set-control-w-shortcuts)))

  ;; Some modes should not start in evil-mode
  (evil-set-initial-state 'paradox-menu-mode 'emacs)
  (evil-set-initial-state 'el-get-package-menu-mode 'emacs)
  (evil-set-initial-state 'ag-mode 'emacs)
  ;; (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
  ;; (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (evil-set-initial-state 'pdf-annot-list-mode 'emacs)
  (evil-set-initial-state 'calendar-mode 'emacs))

(use-package evil-leader
  :straight t
  :after evil
  :commands (global-evil-leader-mode
             evil-leader/set-key
             evil-leader/set-key-for-mode)
  :init (global-evil-leader-mode)
  :config
  (evil-leader/set-key
    "f" 'onc/indent-whole-buffer
    "o" 'origami-toggle-node
    "t" 'treemacs
    "init" (lambda () (interactive) (find-file onc/init-el-path))
    "e" 'eval-defun
    "1" 'highlight-symbol-at-point
    "0" 'highlight-symbol-remove-all
    "gst" 'magit-status
    "glg" 'magit-log-all
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    "a" 'align-regexp
    "s" 'helm-projectile-ag
    "pf" 'helm-projectile-find-file
    "po" 'helm-projectile-switch-project)

  (evil-leader/set-key-for-mode
    'python-mode "d" 'lsp-ui-peek-find-definitions)

  (evil-leader/set-key-for-mode
    'c++-mode "f" 'clang-format-buffer)

  (evil-leader/set-key-for-mode
    'python-mode "f" 'lsp-format-buffer)

  (evil-leader/set-key-for-mode
    'rust-mode "f" 'cargo-process-fmt))

(use-package evil-search-highlight-persist
  :straight t
  :after evil
  :commands global-evil-search-highlight-persist
  :init (global-evil-search-highlight-persist t)
  :config
  ;; Change highlight face of evil search
  (set-face-foreground 'evil-search-highlight-persist-highlight-face "#ff00ff")
  (set-face-background 'evil-search-highlight-persist-highlight-face "#ffffff"))

(use-package evil-nerd-commenter
  :straight t
  :after evil)

(use-package evil-surround
  :straight t
  :after evil
  :commands global-evil-surround-mode
  :init (global-evil-surround-mode t))


;; Highlight matching delimiters
(use-package rainbow-delimiters
  :straight t
  :commands rainbow-delimiters-mode
  :diminish rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; Show colors in code
(use-package rainbow-mode
  :straight t
  :commands rainbow-mode
  :diminish rainbow-mode
  :init
  (dolist
      (hook '(css-mode-hook
              html-mode-hook
              js-mode-hook
              emacs-lisp-mode-hook
              text-mode-hook))
    (add-hook hook #'rainbow-mode)))


(use-package highlight-thing
  :straight t
  :diminish highlight-thing-mode
  :custom (highlight-thing-exclude-thing-under-point t)
  :config
  (global-highlight-thing-mode)

  (custom-set-faces
   '(highlight-thing ((t (:background "#49483E" :foreground "#A6E22E"))))))


;; Global emacs bindings with prefix
(use-package hydra
  :straight t
  :commands (hydra-default-pre
             hydra-keyboard-quit
             hydra--call-interactively-remap-maybe
             hydra-show-hint
             hydra-set-transient-map)
  :bind (("C-x m" . onc/common-functions/body))
  :config
  (defhydra onc/common-functions (:color teal)
    "
      Onzes functions

     Buffers                       Other
------------------------------------------------------------------------------------------
  _i_: indent buffer %(onc/where-is-first 'onze-indent-whole-buffer)         _p_: switch project %(onc/where-is-first 'helm-projectile-switch-project)
  _r_: rename buffer and file    _s_: projectile ag %(onc/where-is-first 'helm-projectile-ag)
                               _c_: emacs config
"
    ("i" onc/indent-whole-buffer        nil)
    ("r" onc/rename-file-and-buffer     nil)
    ("p" helm-projectile-switch-project nil)
    ("c" (lambda () (interactive) (find-file onc/init-el-path)) nil)
    ("s" helm-projectile-ag nil)
    ("q" nil                            "cancel")))


;; Ido-mode replacement
(use-package helm
  :straight t
  :after async
  :diminish helm-mode
  :bind (("C-h C-h" . helm-apropos)
         ("M-x"     . helm-M-x)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-j" . helm-select-action)
         ("M-j" . helm-next-line)
         ("M-k" . helm-previous-line))
  :custom
  (helm-ff-skip-boring-files t)
  (helm-mode-fuzzy-match t)
  (helm-completion-in-region-fuzzy-match t)
  (helm-ff-file-name-history-use-recentf t)
  (helm-reuse-last-window-split-state t)
  (helm-split-window-inside-p t "Don't use full width of the frame")
  (helm-move-to-line-cycle-in-source nil)
  :config
  (helm-mode +1)

  ;; Ignore .DS_Store files with helm mode
  (add-to-list 'helm-boring-file-regexp-list "\\.DS_Store$")
  (helm-autoresize-mode t)

  ;; Use ack instead of grep
  (when (executable-find "ack")
    (validate-setq helm-grep-default-command "ack -Hn --no-group --no-color %p %f"
                   helm-grep-default-recurse-command "ack -H --no-group --no-color %p %f"))

  ;; Even better, use ag if it's available
  (when (executable-find "ag")
    (validate-setq helm-grep-default-command "ag --vimgrep -z %p %f"
                   helm-grep-default-recurse-command "ag --vimgrep -z %p %f")))

(use-package helm-ag
  :straight t
  :ensure-system-package ag
  :after (helm exec-path-from-shell))


(use-package helm-swoop
  :straight t
  :after helm
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-multi-swoop-projectile)))


(use-package helm-projectile
  :straight t
  :after (helm projectile)
  :commands (helm-projectile-on
             helm-projectile-switch-project)
  :init (helm-projectile-on)
  :custom (projectile-completion-system 'helm))


;; Projects in emacs
(use-package projectile
  :straight t
  :custom (projectile-mode-line '(:eval (format " Proj[(%s)]" (projectile-project-name))))
  :init (projectile-mode t))


;; Show git modifications
(use-package git-gutter
  :straight t
  :diminish git-gutter-mode
  :init (global-git-gutter-mode t)
  :custom (git-gutter:hide-gutter t "hide if there are no changes")
  :config
  (use-package git-gutter-fringe
    :ensure t
    :config
    ;; colored fringe "bars"
    (define-fringe-bitmap 'git-gutter-fr:added
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:modified
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:deleted
      [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
      nil nil 'center))

  ;; Refreshing git-gutter
  (advice-add 'evil-force-normal-state :after 'git-gutter)
  (add-hook 'focus-in-hook 'git-gutter:update-all-windows))


;; Git support for emacs
(use-package magit
  :straight t
  :commands (magit-status magit-log-all)
  :custom (magit-diff-refine-hunk t)
  :config
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list "~/.emacs.d/git-packages/magit/Documentation")))


(use-package magit-gitflow
  :straight t
  :after magit
  :commands turn-on-magit-gitflow
  :init
  (add-hook 'magit-mode-hook #'turn-on-magit-gitflow))


;; treemacs
(use-package treemacs
  :straight t
  :defer t
  :config
  (treemacs-follow-mode t)
  (treemacs-resize-icons 14)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-hide-gitignored-files-mode t)

  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  ;; dont use bold fonts in treemacs
  (dolist (face '(treemacs-root-face
                  treemacs-git-unmodified-face
                  treemacs-git-modified-face
                  treemacs-git-renamed-face
                  treemacs-git-ignored-face
                  treemacs-git-untracked-face
                  treemacs-git-added-face
                  treemacs-git-conflict-face
                  treemacs-directory-face
                  treemacs-directory-collapsed-face
                  treemacs-file-face
                  treemacs-tags-face))
    (set-face-attribute face nil :weight 'normal)))

(use-package treemacs-nerd-icons
  :straight t
  :after (treemacs nerd-icons)
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-evil
  :straight t
  :after (treemacs evil))

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile))

;; lsp
(use-package lsp-mode
  :straight t
  :after exec-path-from-shell
  :hook (python-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-pylsp-plugins-ruff-enabled t)
  (lsp-pylsp-plugins-mypy-enabled t)
  (lsp-pylsp-plugins-mypy-live-mode nil)
  (lsp-pylsp-plugins-black-enabled t)
  (lsp-pylsp-plugins-isort-enabled t)
  ;; Disable these as they're duplicated by flake8
  (lsp-pylsp-plugins-pycodestyle-enabled nil)
  (lsp-pylsp-plugins-mccabe-enabled nil)
  (lsp-pylsp-plugins-pyflakes-enabled nil)
  )

(use-package lsp-ui
  :straight t
  :commands (lsp-ui-mode)
  :config
  (lsp-ui-mode))

;; helper to load python venvs
;; https://slinkp.com/python-emacs-lsp-20231229.html
(use-package envrc
  :straight t
  :after exec-path-from-shell
  :hook (after-init . envrc-global-mode))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t)

;;; Language support
;;; ----------------

(use-package python
  :after exec-path-from-shell
  :mode ("\\.py\\'" . python-mode)
  ;; :interpreter ("python3" . python-mode)
  :bind(:map
        python-mode-map
        ("C-c r" . 'onc/run-current-file)))


(use-package python-pytest
  :straight t)


(use-package elisp-mode
  :diminish (emacs-lisp-mode . "elisp")
  :interpreter ("emacs" . emacs-lisp-mode))


;;; Modes for other filetypes
;;; -------------------------
(use-package sh-script
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\zshrc\\'" . sh-mode)))


(use-package yaml-mode
  :straight t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)
         ("\\.yaml.j2\\'" . yaml-mode)))


(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mmd\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook #'orgtbl-mode)
  (add-hook 'markdown-mode-hook
            (lambda()
              (add-hook 'after-save-hook 'org-tables-to-markdown  nil 'make-it-local))))


(use-package jupyter
  :straight t)


(use-package org
  :straight t
  :config
  ;; (advice-remove #'org-babel-do-load-languages #'ignore)
  (advice-add 'org-babel-eval :around #'envrc-propagate-environment)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python     . t)
     (jupyter    . t))))


(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  :custom (js-indent-level 2))


(use-package terraform-mode
  :straight t
  :hook (terraform-mode . outline-minor-mode))


(use-package csv-mode
  :straight t
  :mode "\\.csv\\'"
  :hook (csv-mode . csv-align-mode))


(use-package dockerfile-mode
  :straight t
  :mode "\\Dockerfile")

;;; Onc Functions
;;; -------------
(defun onc/indent-whole-buffer ()
  "Delete trailing whitespace, indent and untabify whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)))


(defun onc/kill-this-buffer-if-not-modified ()
  "Kill current buffer, even if it has been modified."
  (interactive)
  (kill-buffer (current-buffer)))


;; Source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun onc/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


(defun onc/where-is (definition count &optional length)
  "DEFINITION is the name of the function.
Return COUNT key sequences that invoke the command DEFINITTION as a string.
If COUNT is 0 then all key sequences will be returned.
The optional LENGTH argument limits the returned string to LENGTH number of
chars.

For convenience there are also the functions `onc/where-is-first'
and `onc/where-is-all'.

This function relies on Magnar Sveen's dash.el library."
  (interactive
   (let ((fn (function-called-at-point))
         (enable-recursive-minibuffers t)
         val)
     (setq val (completing-read
                (if fn
                    (format "Where is command (default %s): " fn)
                  "Where is command: ")
                obarray 'commandp t nil nil
                (and fn (symbol-name fn))))
     (list (unless (equal val "") (intern val))
           current-prefix-arg)))
  (unless definition (error "No command"))
  (let ((func (indirect-function definition))
        (defs nil)
        string)
    ;; In DEFS, find all symbols that are aliases for DEFINITION.
    (mapatoms (lambda (symbol)
                (and (fboundp symbol)
                     (not (eq symbol definition))
                     (eq func (condition-case ()
                                  (indirect-function symbol)
                                (error symbol)))
                     (push symbol defs))))
    ;; Look at all the symbols--first DEFINITION,
    ;; then its aliases.
    (dolist (symbol (cons definition defs))
      (let* ((remapped (command-remapping symbol))
             (keys (where-is-internal
                    symbol overriding-local-map nil nil remapped))
             (keys (if (> count 0)
                       (mapconcat 'key-description (-take count keys) ", ")
                     (mapconcat 'key-description keys ", "))))
        (setq string
              (if (> (length keys) 0)
                  (if remapped
                      (format "%s is remapped to %s which is on %s"
                              symbol remapped keys)
                    (format "%s" keys))
                ;; If this is the command the user asked about,
                ;; and it is not on any key, say so.
                ;; For other symbols, its aliases, say nothing
                ;; about them unless they are on keys.
                (if (eq symbol definition)
                    (format "NA"))))))
    (if length
        (substring string 0 (min (length string) (+ 1 length)))
      string)))


(defun onc/where-is-first (definition &optional length)
  "DEFINITION is the name of the function.
Return the first key sequence for DEFINITION as a string.
The optional LENGTH argument limits the returned string to LENGTH number of
chars.

This is a convenience function for `my-where-is'."
  (interactive)
  (onc/where-is definition 1 length))


(defun onc/where-is-all (definition &optional length)
  "DEFINITION is the name of the function.
Return all key sequence for DEFINITION as a string.
The optional LENGTH argument limits the returned string to LENGTH number of
chars.

This is a convenience function for `my-where-is'."
  (interactive)
  (onc/where-is definition 0 length))

(defun onc/font-name-replace-size (font-name new-size)
  "Changing font size of FONT-NAME and set it to NEW-SIZE."
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun onc/increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame.
Emacs will keep the pixel size of the frame approximately the
same.  DELTA should be a multiple of 10, to match the units used
by the :height face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (dolist (f (frame-list))
      (with-selected-frame f
        ;; Latest 'set-frame-font supports a "frames" arg, but
        ;; we cater to Emacs 23 by looping instead.
        (set-frame-font (onc/font-name-replace-size
                         (face-font 'default)
                         new-point-height)
                        t)))
    (set-face-attribute 'default nil :height new-height)
    (message "default font size is now %d" new-point-height)))

(defun onc/increase-default-font-height ()
  "Increate the font height."
  (interactive)
  (onc/increment-default-font-height 10))

(defun onc/decrease-default-font-height ()
  "Decrease the font height."
  (interactive)
  (onc/increment-default-font-height -10))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8cc8a266d92e987bdd4e379796c00004b5525ef29a62cb09f48b1d75e05a4509" "25b7f91ffd38218d329c83d29b61ac8d7c18f5be795710317c36fe6e11b47255" "c11414651f9af5b134f7d15ffd6f39898ae4309c2989de10f89cca0d7928b92e" "f40a2f2194a5a040ed0c52071a444735a7bc929d8275a33eb3be994d553896b8" default))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
