;;; package --- Summary

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
(defconst onc/init-el-compiled-path (file-truename (expand-file-name "~/.emacs.d/init.elc")))

;; Path of themes
(defconst onc/custom-theme-load-path (expand-file-name "~/.emacs.d/themes"))

;; Font for emacs
(defconst onc/font-family "SF Mono")

;; Size of font
(defconst onc/font-size 100)

;; Location of deft notes
(defconst onc/deft-directory (expand-file-name "~/Documents/Note"))

;; Clang paths
(defconst onc/clang-format-command-path "/usr/local/bin/clang-format")

;; Org-Mode
;; (defconst onc/org-agenda-file-location (expand-file-name "~/todo.org"))


;;; GC stuff
;;; ---------

;; GC tweaking according to
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; this also makes init.el load faster
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; (add-hook 'emacs-startup-hook
;;           (setq gc-cons-threshold 800000)
;;           (setq gc-cons-percentage 0.1))

(defun my-minibuffer-setup-hook ()
  "Crank up the Garbage Collection threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Put Garbage Collection back to default value."
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; If this is non-nil, then when both .el and .elc
;; versions of a file exist, and the caller did not explicitly specify
;; which one to load, then the newer file is loaded.
(setq load-prefer-newer t)

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
(load-theme 'base16-onc-dark 'no-confirm)

;; Show system name and full file path in emacs frame title
(setq frame-title-format
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
                       ";;\n"
                       ";;" (onc/colorify "  ___  " "OrangeRed") (onc/colorify "_ __   " "orange")   (onc/colorify "___ " "yellow")   (onc/colorify "_ __ ___  " "green") (onc/colorify " __ _  " "cyan") (onc/colorify "___"   "blue") (onc/colorify " ___  \n"   "DarkMagenta")
                       ";;" (onc/colorify " / _ \\" "OrangeRed") (onc/colorify "| '_ \\" "orange")   (onc/colorify " / __|" "yellow") (onc/colorify " '_ ` _ \\" "green") (onc/colorify " / _` |" "cyan") (onc/colorify "/ __"  "blue") (onc/colorify "/ __| \n"   "DarkMagenta")
                       ";;" (onc/colorify "| (_) "  "OrangeRed") (onc/colorify  "| | | | " "orange") (onc/colorify "(__| " "yellow")  (onc/colorify "| | | | |"  "green") (onc/colorify " (_| | " "cyan") (onc/colorify "(__"   "blue") (onc/colorify "\\__ \\ \n" "DarkMagenta")
                       ";;" (onc/colorify " \\___/" "OrangeRed") (onc/colorify "|_| |_|" "orange")   (onc/colorify "\\___|" "yellow") (onc/colorify "_| |_| |_|" "green") (onc/colorify "\\__,_|" "cyan") (onc/colorify "\\___" "blue") (onc/colorify "|___/ \n"   "DarkMagenta")
                       ";;\n"
                       ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"
                       ";; To create a file, visit it with \ o and enter text in its buffer.\n"
                       "\n"
                       "\n")))))


;; stfu emacs!!!
(setq ring-bell-function 'ignore)

;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)

;;; Global keybindings
;;; ------------------

;; macOS keybindings
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'option
        mac-command-modifier 'meta))

;; macOS smooth scrolling
(when (eq system-type 'darwin)
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil))

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

;;; Bootstrap package management
;;; ----------------------------

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;; Validation of setq and stuff
(use-package validate
  :ensure t)


;;; Files
;;; -----

;; Move custom-set-variables to a seperate file
(load (expand-file-name "custom-variables" user-emacs-directory))

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

;; Set programm for urls
(defun browse-url-default-macosx-browser (url &optional new-window)
  "Open the given URL with safari.  Optional in a NEW-WINDOW."
  (interactive (browse-url-interactive-arg "URL: "))
  (if (and new-window (>= emacs-major-version 23))
      (ns-do-applescript
       (format (concat "tell application \"Safari\" to make document with properties {URL:\"%s\"}\n"
                       "tell application \"Safari\" to activate") url))
    (start-process (concat "open " url) nil "open" url)))

(if (eq system-type 'darwin)
    (validate-setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (validate-setq browse-url-browser-function 'browse-url-chromium))

;; Automatically compile and save init.el
(defun byte-compile-init-files (file)
  "Automatically compile FILE."
  (interactive)
  (save-restriction
    ;; Suppress the warning when you setq an undefined variable.
    (if (>= emacs-major-version 23)
        (setq byte-compile-warnings '(not free-vars obsolete))
      (setq byte-compile-warnings '(unresolved callargs redefine obsolete noruntime cl-warnings interactive-only)))
    (byte-compile-file (expand-file-name file))))

(add-hook 'after-save-hook
          (function (lambda ()
                      (if (string= (file-truename onc/init-el-path)
                                   (file-truename (buffer-file-name)))
                          (byte-compile-init-files onc/init-el-path)))))

;; Byte-compile if init.el, is newer than compiled version of it.
(if (file-newer-than-file-p onc/init-el-path onc/init-el-compiled-path)
    (byte-compile-init-files "~/.emacs.d/init.el"))

;;; Packages
;;; --------


;;; Dependencies/ Libraries
;;; -----------------------

;; Simple library for asynchronous processing
(use-package async
  :defer t
  :config (require 'async-bytecomp))


;; List library
(use-package dash
  :defer t)


;; String library
(use-package s
  :ensure t)


;; no idea where this comes from...
(use-package unbound
  :ensure t)


(use-package font-lock+
  :load-path "git-packages/font-lock+")


;; addon for use-package to ensure system packages
(use-package use-package-ensure-system-package
  :ensure t)


;;; General packages
;;; ----------------

;; Emacs in server-mode
(use-package server
  :if (not noninteractive)
  :commands server-start
  :init (server-start)
  :diminish server-buffer-clients)


(use-package one-themes
  :ensure t)


;; Save buffers
(use-package desktop
  :commands desktop-save-mode
  :init (desktop-save-mode t)
  :custom
  (desktop-auto-save-timeout 60 "Save desktop after one minute of idle")
  (desktop-load-locked-desktop t))


(use-package prog-mode
  :preface
  (defun onc/add-todo-marker ()
    (interactive)
    (font-lock-add-keywords
     nil
     '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)" 1 font-lock-warning-face t))))
  :init
  ;; Highlight some keywords in prog-mode
  (add-hook 'prog-mode-hook #'onc/add-todo-marker))


;; Simple
(use-package simple
  :defer t
  :custom (column-number-mode t))


;; Reveal current buffer in finder
(use-package reveal-in-osx-finder
  :bind (("C-c f J" . reveal-in-osx-finder)))


;; Fringe mode (left and right borders stuff)
(use-package fringe
  :commands fringe-mode
  :config (fringe-mode '(4 . 0)))


(use-package compile
  :custom (compilation-scroll-output t))


;; Save recent files
(use-package recentf
  :commands recentf-mode
  :init (recentf-mode t)
  :custom (recentf-max-saved-items 1000))


;; Undo with branching
(use-package undo-tree
  :diminish undo-tree-mode
  :commands global-undo-tree-mode
  :init (global-undo-tree-mode t)
  :custom
  (undo-tree-auto-save-history nil)
  (undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo"))))
  (undo-tree-visualizer-timestamps t undo-tree-visualizer-diff t))


;; Save position in files
(use-package saveplace
  :commands save-place-mode
  :init (save-place-mode t)
  :custom (save-place-file (expand-file-name "places" user-emacs-directory)))


;; Auto-revert of changed files
(use-package autorevert
  :commands global-auto-revert-mode
  :init (global-auto-revert-mode t))


;; Insert matching delimiters
(use-package elec-pair
  :commands electric-pair-mode
  :init (electric-pair-mode t))


(use-package origami
  :ensure t
  :after (dash s)
  :commands global-origami-mode
  :init (global-origami-mode t))


;; Load shell env
(use-package exec-path-from-shell
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :commands (exec-path-from-shell-initialize exec-path-from-shell-copy-env)
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "RUST_SRC_PATH"))


;; Edit files as root, through Tramp
(use-package sudo-edit
  :ensure t
  :bind (("C-c f s" . sudo-edit)))


;; Highlight symbols in Emacs, used in leader binding
(use-package highlight-symbol
  :ensure t)


;; Emacs Start Up Profiler
(use-package esup
  :commands esup)


;; Show column
(use-package fill-column-indicator
  :ensure t
  :init
  (add-hook 'c-mode-common-hook #'turn-on-fci-mode)
  (add-hook 'js2-mode-hook #'turn-on-fci-mode)
  (add-hook 'swift-mode-hook #'turn-on-fci-mode)
  (add-hook 'python-mode-hook #'turn-on-fci-mode)
  (add-hook 'typescript-mode-hook #'turn-on-fci-mode)
  :custom
  (fill-column 80)
  (fci-rule-width 1)
  (fci-rule-color "gray71"))


;; Emacs + vim = <3
(use-package evil
  :ensure t
  :bind (:map evil-normal-state-map
              ("Y" . copy-to-end-of-line)
              ("j" . evil-next-visual-line)
              ("k" . evil-previous-visual-line)
              ("gj" . evil-next-line)
              ("gk" . evil-previous-line)
              ("C-u" . evil-scroll-up)
              ("C-d" . evil-scroll-down))
  :custom (evil-move-cursor-back nil "make the cursor stay in place after exiting insert mode")
  :hook (evil-normal-state . #'add-vim-bindings)
  :init
  (evil-mode t)

  ;; ;; save on double escape, works best with escape mapped to caps-lock.
  ;; (add-hook 'evil-normal-state-entry-hook #'add-vim-bindings)

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
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
  ;; (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (evil-set-initial-state 'pdf-annot-list-mode 'emacs)
  (evil-set-initial-state 'calendar-mode 'emacs))

(use-package evil-leader
  :after evil
  :commands (global-evil-leader-mode
             evil-leader/set-key
             evil-leader/set-key-for-mode)
  :init (global-evil-leader-mode)
  :config
  (evil-leader/set-key
    "f" 'onc/indent-whole-buffer
    "t" 'origami-toggle-node
    "init" (lambda () (interactive) (find-file onc/init-el-path))
    "o" 'find-file
    "e" 'eval-defun
    "d" 'dictcc
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
    "pf" 'helm-projectile-find-file)

  (evil-leader/set-key-for-mode
    'c++-mode "f" 'clang-format-buffer)

  (evil-leader/set-key-for-mode
    'rust-mode "f" 'cargo-process-fmt))

(use-package evil-search-highlight-persist
  :after evil
  :commands global-evil-search-highlight-persist
  :init (global-evil-search-highlight-persist t)
  :config
  ;; Change highlight face of evil search
  (set-face-foreground 'evil-search-highlight-persist-highlight-face "#ff00ff")
  (set-face-background 'evil-search-highlight-persist-highlight-face "#ffffff"))

(use-package evil-nerd-commenter
  :ensure t
  :after evil)

(use-package evil-numbers
  :ensure t
  :after evil
  :bind (("<M-up>" . evil-numbers/inc-at-pt)
         ("<M-down>" . evil-numbers/dec-at-pt)))

(use-package evil-surround
  :after evil
  :commands global-evil-surround-mode
  :init (global-evil-surround-mode t))


;; Workspaces in emacs
(use-package perspeen
  :ensure t
  :commands perspeen-mode
  :init (perspeen-mode t))


;; On-the-fly syntax checking
(use-package flycheck
  :commands (global-flycheck-mode flycheck-mode)
  :diminish flycheck-mode
  :init (global-flycheck-mode t)
  :config (setq-default flycheck-disabled-checkers '(javascript-jshint)))


;; Show argument list in echo area
(use-package eldoc
  :diminish eldoc-mode
  :init (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))


;; Highlight matching delimiters
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :diminish rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; Show colors in code
(use-package rainbow-mode
  :commands rainbow-mode
  :diminish (rainbow-mode . "rbow")
  :init
  (dolist
      (hook '(css-mode-hook
              html-mode-hook
              js-mode-hook
              emacs-lisp-mode-hook
              text-mode-hook))
    (add-hook hook #'rainbow-mode)))


;; Autocomplete
(use-package company
  :commands (global-company-mode company-mode)
  :diminish company-mode
  :bind (:map company-active-map
              ("M-j" . company-select-next)
              ("M-k" . company-select-previous))
  :custom
  ;; no delay no autocomplete
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-tooltip-limit 20)

  :preface
  ;; enable yasnippet everywhere
  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or
         (not company-mode/enable-yas)
         (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  :init (global-company-mode t)
  :config
  ;; remove unused backends
  (delete 'company-semantic company-backends)
  (delete 'company-eclim company-backends)
  (delete 'company-xcode company-backends)
  (delete 'company-clang company-backends)
  (delete 'company-bbdb company-backends)
  (delete 'company-oddmuse company-backends))


;; Sort company candidates by statistics
(use-package company-statistics
  :after company
  :commands company-statistics-mode
  :init (company-statistics-mode t))


;; Emojis completion like Github/Slack
(use-package company-emoji
  :ensure t
  :init
  (add-to-list 'company-backends (company-mode/backend-with-yas 'company-emoji)))


;; Snippets
(use-package yasnippet
  :commands yas-global-mode
  :diminish yas-minor-mode
  :init (yas-global-mode t)
  :config
  ;; preserve tab-completion in ansi-term
  (add-hook 'term-mode-hook (lambda()
                              (setq yas-dont-activate t))))

;; Terminal in emacs
(use-package multi-term
  :commands multi-term
  :custom (multi-term-program "/bin/zsh"))


;; Notes
(use-package deft
  :commands deft
  :bind (([f6] . deft))
  :custom
  (deft-directory onc/deft-directory)
  (deft-extensions '("md" "org" "txt"))
  (deft-default-extension "org")
  (deft-use-filename-as-title t)
  (deft-use-filter-string-for-filename t)
  (deft-recursive t)
  (deft-auto-save-interval 3.0))


(use-package dired
  :config
  (evil-make-overriding-map dired-mode-map 'normal t)
  (evil-define-key 'normal dired-mode-map
    "j" 'evil-next-line
    "k" 'evil-previous-line
    "h" 'dired-up-directory
    "l" 'dired-find-file))


(use-package dash-at-point
  :ensure t
  :commands dash-at-point
  :hook ((c++-mode . (lambda () (setq dash-at-point-docset "cpp")))
         (ruby-mode . (lambda () (setq dash-at-point-docset "ruby")))
         (cmake-mode . (lambda () (setq dash-at-point-docset "cmake")))
         (python-mode . (lambda () (setq dash-at-point-docset "python3")))
         (ess-mode . (lambda () (setq dash-at-point-docset "r")))
         (rust-mode . (lambda () (setq dash-at-point-docset "rust"))))
  :bind ("C-SPC" . dash-at-point))


(use-package doc-view
  :commands doc-view-fit-page-to-window
  :bind (:map doc-view-mode-map
              ("j" . doc-view-next-page)
              ("<SPC>" . doc-view-next-page)
              ("k" . doc-view-previous-page))
  :init (add-hook 'doc-view-mode-hook #'doc-view-fit-page-to-window))


;; Ido-mode replacement
(use-package helm
  :ensure t
  :after async
  :commands helm-autoresize-mode
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
  :config
  (require 'helm-config)
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
  :ensure t
  :ensure-system-package ag
  :after helm)

(use-package helm-dash
  :ensure t
  :after helm
  :preface
  (defun rust-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("Rust")))

  (defun cc-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("C\+\+")))
  :init
  (add-hook 'rust-mode-hook 'rust-doc)
  (add-hook 'c++-mode-hook 'cc-doc)
  :custom
  (helm-dash-browser-func 'eww))

(use-package helm-swoop
  :ensure t
  :after helm
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-multi-swoop-projectile)))

(use-package helm-projectile
  :after (helm projectile)
  :commands (helm-projectile-on
             helm-projectile-switch-project)
  :init (helm-projectile-on)
  :custom (projectile-completion-system 'helm))


(use-package helm-spotify-plus
  :ensure t
  :after (helm multi))


;; Projects in emacs
(use-package projectile
  :commands projectile-mode
  :custom (projectile-mode-line '(:eval (format " Proj[(%s)]" (projectile-project-name))))
  :init (projectile-mode t))


;; Show git modifications
(use-package git-gutter
  :commands global-git-gutter-mode
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


;; Dependencies of magit
(use-package with-editor
  :ensure t)

(use-package magit-popup
  :ensure t)

(use-package ghub
  :ensure t)


;; Git support for emacs
(use-package magit
  :ensure t
  :after (with-editor magit-popup ghub)
  :commands (magit-status magit-log-all)
  :custom (magit-diff-refine-hunk t)
  :config
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list "~/.emacs.d/git-packages/magit/Documentation")))

(use-package magit-gitflow
  :ensure t
  :after magit
  :commands turn-on-magit-gitflow
  :init
  (add-hook 'magit-mode-hook #'turn-on-magit-gitflow))


;; Moodle-destroyer plugin
(use-package moodle-destroyer
  :load-path "/Users/onze/Repos/moodle-destroyer.el/lisp"
  :bind(:map
        moodle-destroyer-mode-map
        ("C-c C-c" . moodle-destroyer-org-to-json))
  :commands (moodle-destroyer-json-to-org
             moodle-destroyer-org-to-json)
  :custom
  (moodle-destroyer-gradingfile-org-name "grading.org" "Set custom name for org-mode gradingfile")
  (moodle-destroyer-gradingfile-json-name "grading.ex.json" "Set custom name for exported json file"))


;; Better emacs package menu
(use-package paradox
  :commands (paradox-list-packages paradox-upgrade-packages)
  :custom
  (paradox-automatically-star nil)
  (paradox-display-star-count nil)
  (paradox-execute-asynchronously t))


;; Code-comprehension server
(use-package ycmd
  :commands ycmd-mode
  :init (add-hook 'c++-mode-hook #'ycmd-mode)
  :config
  (set-variable 'ycmd-server-command '("python3" "/Users/onze/Applications/ycmd/ycmd"))
  (set-variable 'ycmd-global-config (expand-file-name "~/Repos/dotfiles/ycmd/ycm_conf.py"))
  (set-variable 'ycmd-extra-conf-whitelist '("~/Uni/*" "~/Repos/*")))

(use-package flycheck-ycmd
  :after (ycmd flycheck)
  :commands (flycheck-ycmd-setup)
  :init (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))

(use-package company-ycmd
  :after(ycmd company)
  :commands (company-ycmd-setup)
  :config (add-to-list 'company-backends (company-mode/backend-with-yas 'company-ycmd)))


;; Control system services (like webservers and stuff) from emacs
(use-package prodigy
  :commands (prodigy-define-tag prodigy-define-service)
  :init
  (prodigy-define-tag
    :name 'flask
    :env '(("LANG" "en_US.UTF-8")
           ("LC_ALL" "en_US.UTF-8")))

  (prodigy-define-service
    :name "Scattervis flask"
    :command "python3"
    :args '("-m" "flask" "run")
    :cwd "/Users/onze/Uni/Masterarbeit/scatterplot-vis-backend"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t
    :env '(("FLASK_APP" "scatterplotvis"))
    :tags '(flask))

  (prodigy-define-service
    :name "Scatterplotvis gulp"
    :command "npm"
    :args '("start")
    :cwd "/Users/onze/Uni/Masterarbeit/scatterplot-vis"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))


;; manage system packages
(use-package system-packages
  :ensure t)


;; Restclient in Emacs
(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

(use-package company-restclient
  :ensure t
  :after (restclient company)
  :config (add-to-list 'company-backends (company-mode/backend-with-yas 'company-restclient)))


;; Dict.cc in Emacs
(use-package dictcc
  :commands dictcc
  :custom
  (dictcc-source-lang "de")
  (dictcc-destination-lang "en"))


;; Org-Mode
(use-package org
  :commands org-babel-do-load-languages
  :mode (("\\.org\\'" . org-mode)
         ("\\.org_archive\\'" . org-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("M-j" . org-forward-heading-same-level)
         ("M-k" . org-backward-heading-same-level))
  :custom
  (org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
  (org-src-fontify-natively t "fontify code in code blocks")

  (org-latex-packages-alist (quote (("" "color" t) ("" "minted" t) ("" "parskip" t))))

  :config
  ;; "remove 'inputenc' from default packages as it clashes with xelatex"
  (validate-setq org-latex-default-packages-alist
                 (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sh . t)
     (python . t)
     (ruby . t)
     (dot . t)
     (octave . t)
     (sqlite . t)
     (perl . t))))

(use-package org-clock
  :after org
  :commands (org-clock-persistence-insinuate
             org-clock-in
             org-clock-out
             org-clock-report)
  :custom (org-clock-persist 'history)
  :config (org-clock-persistence-insinuate))

(use-package org-bullets
  :after org
  :commands org-bullets-mode
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :custom (org-bullets-bullet-list '("●" "◼" "▶" "♦")))


;; Global emacs bindings with prefix
(use-package hydra
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

     Buffers                     Other
------------------------------------------------------------------------------------------
  _i_: indent buffer %(onc/where-is-first 'onze-indent-whole-buffer)     _p_: switch project %(onc/where-is-first 'helm-projectile-switch-project)
  _r_: rename buffer and file                               _s_: start clock
  _v_: toggle transparency                                  _f_: stop/finish cock
                                                          _t_: create report-table
"
    ("i" onc/indent-whole-buffer        nil)
    ("r" onc/rename-file-and-buffer     nil)
    ("p" helm-projectile-switch-project nil)
    ("s" org-clock-in                   nil)
    ("f" org-clock-out                  nil)
    ("t" org-clock-report               nil)
    ("v" onc/toggle-transparency        nil)
    ("q" nil                            "cancel")))


;; On-the-fly spell checking
(use-package flyspell
  :commands flyspell-buffer
  :bind (:map flyspell-mouse-map
              ("[down-mouse-3]" . ispell-word)
              ("[mouse-3]" . ispell-word))
  :preface
  (defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
    "If RUN-TOGETHER is true, spell check the CamelCase words."
    (let (args)
      (cond
       ((string-match  "aspell$" ispell-program-name)
        ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
        (validate-setq args (list "--sug-mode=ultra" "--lang=en_US"))
        (if RUN-TOGETHER
            (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2"))))))
      args))
  :init
  (dolist (hook '(text-mode-hook message-mode-hook))
    (add-hook hook 'turn-on-flyspell))
  :diminish(flyspell-mode . "spell")
  :config
  (cond
   ((executable-find "aspell")
    (validate-setq ispell-program-name "aspell"))
   (t (validate-setq ispell-program-name nil)))

  ;; ispell-cmd-args is useless, it's the list of *extra* arguments we will
  ;; append to the ispell process when "ispell-word" is called.
  ;; ispell-extra-args is the command arguments which will *always* be used
  ;; when start ispell process
  (validate-setq ispell-extra-args (flyspell-detect-ispell-args t))

  (defadvice ispell-word (around my-ispell-word activate)
    (let ((old-ispell-extra-args ispell-extra-args))
      (ispell-kill-ispell t)
      (validate-setq ispell-extra-args (flyspell-detect-ispell-args))
      ad-do-it
      (validate-setq ispell-extra-args old-ispell-extra-args)
      (ispell-kill-ispell t))))


;; Spell checker
(use-package ispell
  :defer t
  :custom
  (ispell-program-name (if (eq system-type 'darwin) (executable-find "aspell") (executable-find "hunspell")))
  (ispell-dictionary "en_US")
  :config
  (unless ispell-program-name
    (warn "No spell-checker available. Please install aspell or Hunspell")))


;; Automatically infer dictionary
(use-package auto-dictionary
  :commands auto-dictionary-mode
  :init (add-hook 'flyspell-mode-hook #'auto-dictionary-mode))


;;; Language support
;;; ----------------

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'")

(use-package cargo
  :after rust-mode
  :ensure-system-package cargo
  :commands cargo-minor-mode
  :bind(:map
        rust-mode-map
        ("C-c c" . cargo-process-build)
        ("C-c x" . cargo-process-run))
  :init (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package racer
  :after rust-mode
  :commands racer-mode
  :init (add-hook 'rust-mode-hook #'racer-mode)
  :custom (racer-rust-src-path (getenv "RUST_SRC_PATH")))

(use-package flycheck-rust
  :commands flycheck-rust-setup
  :after (flycheck rust-mode)
  :ensure t
  :init (add-hook 'rust-mode-hook #'flycheck-rust-setup))

(use-package company-racer
  :after (company racer)
  :ensure t
  :config (add-to-list 'company-backends (company-mode/backend-with-yas 'company-racer)))


;; Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :bind(:map
        python-mode-map
        ("C-c r" . onc/run-current-file)))

(use-package elpy
  :after python-mode
  :commands elpy-enable
  :custom
  (elpy-rpc-python-command "python3")
  (elpy-rpc-backend "jedi")
  (elpy-use-cpython "/usr/local/bin/python3")
  :config
  (elpy-enable)

  (delete 'elpy-module-company elpy-modules)

  (add-hook 'python-mode-hook
            (lambda ()
              (company-mode)
              (add-to-list 'company-backends
                           (company-mode/backend-with-yas 'elpy-company-backend)))))


(use-package flycheck-pycheckers
  :ensure t
  :config
  (setq flycheck-pycheckers-checkers '(pyflakes flake8 pep8))
  (with-eval-after-load 'flycheck
            (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))


;; Ruby
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :bind(:map
        ruby-mode-map
        ("C-c r" . onc/run-current-file)))

(use-package robe
  :commands robe-mode
  :after ruby-mode
  :init
  (add-hook 'ruby-mode-hook #'robe-mode)
  (add-to-list 'company-backends (company-mode/backend-with-yas 'company-robe)))

(use-package rubocop
  :ensure-system-package rubocop
  :hook ruby-mode)

(use-package inf-ruby
  :commands (inf-ruby-minor-mode inf-ruby-auto-enter)
  :after ruby-mode
  :init
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter))

(use-package company-inf-ruby
  :after (inf-ruby company)
  :config (add-to-list 'company-backends (company-mode/backend-with-yas 'company-inf-ruby)))


;; Elisp
(use-package elisp-mode
  :diminish (emacs-lisp-mode . "elisp")
  :interpreter ("emacs" . emacs-lisp-mode))


;; Javascript
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq-default js2-global-externs '("exports" "module" "require" "setTimeout" "THREE"))
  (setq-default js2-basic-offset 2))


(use-package rjsx-mode
  :mode "\\.jsx\\'"
  :config
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil))


(use-package tern
  :commands tern-mode
  :ensure-system-package (tern . "npm i -g tern")
  :init
  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  (add-hook 'web-mode-hook (lambda () (tern-mode t)))
  (add-hook 'rjsx-mode-hook (lambda () (tern-mode t)))
  :config
  (use-package company-tern
    :ensure t
    :config (add-to-list 'company-backends (company-mode/backend-with-yas 'company-tern))))

(use-package tide
  :commands tide-setup
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :init
  (progn
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (company-mode +1))
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (validate-setq company-tooltip-align-annotations t))
  :custom (typescript-indent-level 2))


;; Vue mode
(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :init
  (add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face "#212121")
            ('turn-off-flyspell))))


;; Applescript
(use-package apples-mode
  :mode (("\\.scpt\\'" . apples-mode)
         ("\\.scptd\\'" . apples-mode)
         ("\\.AppleScript\\'" . apples-mode)))


;; GLSL
(use-package glsl-mode
  :mode (("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)))


;; Swift
(use-package swift-mode
  :mode "\\.swift\\'"
  :custom (swift-mode:basic-offset 2))

(use-package flycheck-swift
  :after (flycheck swift-mode)
  :mode "\\.swift\\'"
  :init (add-hook 'swift-mode-hook 'flycheck-swift-setup))


;; Haskell
(use-package haskell-mode
  :mode "\\.hs\\'"
  :bind (:map haskell-mode-map
              ("C-c c" . haskell-process-load-file))
  :custom (haskell-interactive-popup-errors nil))

(use-package flycheck-haskell
  :commands flycheck-haskell-setup
  :after (flycheck haskell-mode)
  :mode "\\.hs\\'"
  :init (add-hook 'haskell-mode-hook #'flycheck-haskell-setup))


;; Groovy
(use-package groovy-mode
  :mode "\\.groovy\\'")


;; support for R
(use-package ess
  :mode (("\\.[rR]$" . R-mode))
  :init
  (add-hook 'ess-mode-hook
            '(lambda()
               (font-lock-add-keywords
                nil
                '(
                  ("\\<\\(if\\|for\\|function\\|return\\|$\\|@\\)\\>[\n[:blank:]]*(" 1
                   font-lock-keyword-face) ; must go first to override highlighting below

                  ("\\<\\([.A-Za-z][._A-Za-z0-9]*\\)[\n[:blank:]]*(" 1
                   font-lock-function-name-face) ; highlight function names

                  ("\\([(,]\\|[\n[:blank:]]*\\)\\([.A-Za-z][._A-Za-z0-9]*\\)[\n[:blank:]]*=[^=]"
                   2 font-lock-reference-face)

                  ;; highlight numbers
                  ("\\(-?[0-9]*\\.?[0-9]*[eE]?-?[0-9]+[iL]?\\)" 1 font-lock-type-face)

                  ;; highlight operators
                  ("\\(\\$\\|\\@\\|\\!\\|\\%\\|\\^\\|\\&\\|\\*\\|\(\\|\)\\|\{\\|\}\\|\\[\\|\\]\\|\\-\\|\\+\\|\=\\|\\/\\|\<\\|\>\\|:\\)" 1 font-lock-builtin-face)

                  ;; highlight S4 methods
                  ("\\(setMethod\\|setGeneric\\|setGroupGeneric\\|setClass\\|setRefClass\\|setReplaceMethod\\)" 1 font-lock-reference-face)

                  ;; highlight packages called through ::, :::
                  ("\\(\\w+\\):\\{2,3\\}" 1 font-lock-constant-face)

                  )))))


;; C/C++
(use-package cc-mode
  :mode (("\\.[hH]\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode))
  :bind(:map
        c++-mode-map
        ("C-c c" . onc/cmake-build)
        ("C-c x" . onc/cmake-run))
  :init
  (add-hook 'c++-mode-hook (lambda () (validate-setq flycheck-clang-language-standard "c++14")))

  :preface
  ;; -------------------------------------------------
  ;; build a cmakeproject
  ;; -------------------------------------------------
  (defcustom dirvars-chase-remote nil
    "Whether dirvars looks upward if in a remote filesystem."
    :type 'boolean)

  (defun dirvars-find-upwards (file-name)
    "Find a file in the current directory or one of its parents.
     Returns the fully qualified file name, or nil if it isn't found.
     The FILE-NAME specifies the file name to search for."
    (if (and (not dirvars-chase-remote) (file-remote-p default-directory))
        nil
      ;; Chase links in the source file and search in the dir where it
      ;; points.
      (setq dir-name (or (and buffer-file-name
                              (file-name-directory (file-chase-links
                                                    buffer-file-name)))
                         default-directory))
      ;; Chase links before visiting the file.  This makes it easier to
      ;; use a single file for several related directories.
      (setq dir-name (file-chase-links dir-name))
      (setq dir-name (expand-file-name dir-name))
      ;; Move up in the dir hierarchy till we find a change log file.
      (let ((file1 (concat dir-name file-name))
            parent-dir)
        (while (and (not (file-exists-p file1))
                    (progn (setq parent-dir
                                 (file-name-directory
                                  (directory-file-name
                                   (file-name-directory file1))))
                           ;; Give up if we are already at the root dir.
                           (not (string= (file-name-directory file1)
                                         parent-dir))))
          ;; Move up to the parent dir and try again.
          (setq file1 (expand-file-name file-name parent-dir)))
        ;; If we found the file in a parent dir, use that.  Otherwise,
        ;; return nil
        (if (or (get-file-buffer file1) (file-exists-p file1))
            file1
          nil))))

  (defun onc/cmake-build ()
    (interactive)
    (let* ((build-path-exists (dirvars-find-upwards "debug"))
           (cmakelists-dir (file-name-directory (dirvars-find-upwards "CMakeLists.txt")))
           (build-path (concat cmakelists-dir "debug"))
           )
      (if build-path-exists
          (compile (concat "make -j5 -k -C " build-path))
        ;; create build path and run cmake
        (make-directory build-path)
        (call-process "cmake" nil nil nil (concat "-B" build-path) (concat "-H" cmakelists-dir)))))

  (defun my-executable-path ()
    "Returns ...."
    (with-temp-buffer
      (if (zerop
           (call-process "/bin/bash" nil t nil "-c"
                         (concat
                          (concat "find " (dirvars-find-upwards "debug")) " -perm +111 -type f | grep -v CMake")))
          (buffer-substring (point-min) (1- (point-max)))
        nil)))

  (defun onc/cmake-run ()
    (interactive)
    (call-process (my-executable-path))))

(use-package clang-format
  :after cc-mode
  :ensure-system-package clang-format
  :commands (clang-format-buffer)
  :custom (clang-format-executable onc/clang-format-command-path))

(use-package company-cmake
  :ensure t
  :after (cc-mode company)
  :config (add-to-list 'company-backends (company-mode/backend-with-yas 'company-cmake)))

(use-package modern-cpp-font-lock
  :after cc-mode
  :commands modern-c++-font-lock-mode
  :init (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))


;;; Modes for other filetypes
;;; -------------------------

;; Zsh-files
(use-package sh-script
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\zshrc\\'" . sh-mode)))


;; Toml files from Rust
(use-package toml-mode
  :mode "\\.toml\\'")


;; Cmake Files
(use-package cmake-mode
  :mode "CMakeLists.txt")


;; Scss
(use-package scss-mode
  :mode "\\.scss\\'")


;; Nginx config files
(use-package nginx-mode
  :ensure t)


;; Yaml files
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))


;; Gitignore files
(use-package gitignore-mode
  :mode "\\.gitignore\\'")


;; Markdown files
(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mmd\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook #'orgtbl-mode)
  (add-hook 'markdown-mode-hook
            (lambda()
              (add-hook 'after-save-hook 'org-tables-to-markdown  nil 'make-it-local))))


(use-package rst-mode
  :mode "\\.rst\\'")


;; Web-Mode for html, php and the like
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php\\'"   . web-mode)
         ("\\.jsp\\'"   . web-mode)
         ("\\.erb\\'"   . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))


;; XML files
(use-package nxml-mode
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.xslt\\'" . nxml-mode)
         ("\\.xsd\\'" . nxml-mode))
  :custom
  (nxml-child-indent 2)
  (nxml-attribute-indent 2))


;; Support for AsciiDoc
(use-package adoc-mode
  :ensure t)


;; Gradle-files
(use-package gradle-mode
  :mode "\\.gradle\\'")


;; Json-files
(use-package json-mode
  :mode "\\.json\\'"
  :custom (js-indent-level 2))


;; Dockerfiles
(use-package dockerfile-mode
  :mode "Dockerfile\\'")


;;; Onc Functions
;;; -------------

(defun onc/kill-this-buffer-if-not-modified ()
  "Kill current buffer, even if it has been modified."
  (interactive)
  (kill-buffer (current-buffer)))


(defun onc/indent-whole-buffer ()
  "Delete trailing whitespace, indent and untabify whole buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))


(defun onc/run-current-file ()
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl,
then it'll call “perl x.pl” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
File suffix is used to determine what program to run.
If the file is modified, ask if you want to save first.
This command always run the saved version.
If the file is Emacs Lisp, run the byte compiled version if exist."
  (interactive)
  (let (suffixMap fName fSuffix progName cmdStr)

    ;; a keyed list of file suffix to comand-line program path/name
    (setq suffixMap
          '(("php" . "php")
            ("py" . "python3")
            ("rb" . "ruby")
            ("js" . "node")
            ("sh" . "bash")))
    (setq fName (buffer-file-name))
    (setq fSuffix (file-name-extension fName))
    (setq progName (cdr (assoc fSuffix suffixMap)))
    (setq cmdStr (concat progName " \""   fName "\""))

    (when (buffer-modified-p)
      (progn
        (when (y-or-n-p "Buffer modified.  Do you want to save first? ")
          (save-buffer) ) ) )

    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (progn
          (load (file-name-sans-extension fName)))
      (if progName
          (progn
            (message "Running…")
            ;; (message progName)
            (shell-command cmdStr "*run-current-file output*" ))
        (message "No recognized program file suffix for this file.")))))


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


(defun onc/noumlaut-to-umlaut ()
  "Search/replace non-umlaut candidates to umlauts in entire buffer.
This is done interactively and queries on every candidate."
  (interactive)
  (toggle-case-fold-search)
  (query-replace "ae" "ä" nil (point-min) (point-max))
  (query-replace "Ae" "Ä" nil (point-min) (point-max))
  (query-replace "oe" "ö" nil (point-min) (point-max))
  (query-replace "Oe" "Ö" nil (point-min) (point-max))
  (query-replace "ue" "ü" nil (point-min) (point-max))
  (query-replace "Ue" "Ü" nil (point-min) (point-max))
  (query-replace "sz" "ß" nil (point-min) (point-max))
  (toggle-case-fold-search))


(defun onc/insert-alphabets-az (&optional useUppercase-p)
  "Insert letters a to z vertically.
If USEUPPERCASE-P is set, use CAPITAL letters.
Note: this command is similar to `rectangle-number-lines', starting at 65 or 97, and with a format of 「%c」."
  (interactive "P")
  (let ((startChar (if useUppercase-p 65 97 )))
    (dotimes (ii 26 )
      (insert (format "%c\n" (+ startChar ii))))))


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


(defun onc/toggle-transparency ()
  "Toggle the transparency of the current frame."
  (interactive)
  (let ((alpha-value 90))
    (cond
     ((eq (frame-parameter nil 'alpha) nil)                                           ; case 1
      (set-frame-parameter (selected-frame) 'alpha (cons alpha-value alpha-value)))   ; action 1
     ((eq alpha-value (car (frame-parameter nil 'alpha)))                             ; case 2
      (set-frame-parameter (selected-frame) 'alpha '(100 100)))                       ; action 2
     (t                                                                               ; default case
      (set-frame-parameter (selected-frame) 'alpha (cons alpha-value alpha-value))))) ; default action
  )


;; Adjust font in Emacs
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


;; Make FlyCheck happy...
;;; init.el ends here
