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
(defconst onc/init-el-path (expand-file-name "~/.emacs.d/init.el"))
;; Path of themes
(defconst onc/custom-theme-load-path (expand-file-name "~/.emacs.d/themes"))

;; Font for emacs
(defconst onc/font-family "Hack")

;; Size of font
(defconst onc/font-size 100)

;; Location of deft notes
(defconst onc/deft-directory (expand-file-name "~/Dropbox/Notes"))

;; Clang paths
(defconst onc/clang-format-command-path "/usr/bin/clang-format")

;; Org-Mode
(defconst onc/org-agenda-file-location (expand-file-name "~/todo.org"))


;;; GC stuff
;;; ---------

;; GC tweaking according to
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; this also makes init.el load faster
(setq gc-cons-threshold most-positive-fixnum)

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
(set-face-attribute 'default nil
                    :family onc/font-family :height onc/font-size :weight 'normal)
(set-face-attribute 'variable-pitch nil
                    :family onc/font-family :height onc/font-size :weight 'normal)

;; Load theme
(defun remove-mode-line-box (&rest args)
  (set-face-attribute 'mode-line nil :box nil :underline nil)
  (set-face-attribute 'mode-line-inactive nil :box nil :underline nil))

(add-to-list 'custom-theme-load-path onc/custom-theme-load-path)
(load-theme 'base16-onc-dark 'no-confirm)
(when window-system
  (remove-mode-line-box))

;; Show system name and full file path in emacs frame title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Make the scratch buffer great again
(add-hook 'after-init-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (goto-char (point-min))
              (insert (concat
                       ";;\n"
                       ";;   ___  _ __   ___ _ __ ___   __ _  ___ ___  \n"
                       ";;  / _ \\| '_ \\ / __| '_ ` _ \\ / _` |/ __/ __| \n"
                       ";; | (_) | | | | (__| | | | | | (_| | (__\\__ \\ \n"
                       ";;  \\___/|_| |_|\\___|_| |_| |_|\\__,_|\\___|___/ \n"
                       ";;\n"
                       ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"
                       ";; To create a file, visit it with \ o and enter text in its buffer.\n"
                       "\n"
                       "\n")))))

;; stfu emacs!!!
(setq ring-bell-function 'ignore)

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

;; Map escape to cancel (like C-g)...
(define-key isearch-mode-map [escape] 'isearch-abort)   ;; isearch
(global-set-key [escape] 'keyboard-escape-quit)         ;; everywhere else

;; Tmux-like spliting
(global-set-key (kbd "C-x %") 'split-window-right)
(global-set-key (kbd "C-x \"") 'split-window-below)
(global-set-key (kbd "C-x t") 'make-frame-command)

;; Revert buffer
(global-set-key [f5]
                (lambda ()
                  (interactive)
                  (revert-buffer nil t)
                  (message "Buffer reverted")))

;; Bring the current line to the center
(global-set-key (kbd "C-SPC") 'recenter)

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

;; Highlight some keywords in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)" 1 font-lock-warning-face t)))))

;; Set programm for urls
(validate-setq browse-url-browser-function 'browse-url-chromium)


;;; Packages
;;; --------


;;; Dependencies/ Libraries
;;; -----------------------

;; Simple library for asynchronous processing
(use-package async
  :ensure t
  :defer t
  :config
  (require 'async-bytecomp))


;; List library
(use-package dash
  :defer t)


;; no idea where this comes from...
(use-package unbound
  :ensure t)


;; magit needs this
(use-package with-editor
  :ensure t)


;;; General packages
;;; ----------------

;; Emacs in server-mode
(use-package server
  :if (not noninteractive)
  :init (server-mode)
  :diminish server-buffer-clients)


;; Save buffers
(use-package desktop
  :init (desktop-save-mode t)
  ;; Save desktop after one minute of idle
  :config (validate-setq desktop-auto-save-timeout 60
                         desktop-load-locked-desktop t))


;; OS X window support
(use-package ns-win
  :defer t
  :if (eq system-type 'darwin)
  :config
  (validate-setq
   ;; Don't pop up new frames from the workspace
   ns-pop-up-frames nil))


;; Reveal current buffer in finder
(use-package reveal-in-osx-finder
  ;; Bind analogous to `dired-jump' at C-c f j
  :bind (("C-c f J" . reveal-in-osx-finder)))


;; Fringe mode (left and right borders stuff)
(use-package fringe
  :init (fringe-mode '(2 . 0)))


;; Save recent files
(use-package recentf
  :init(recentf-mode t)
  :config (validate-setq recentf-max-saved-items 1000))


;; Undo with branching
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode t)
  :config
  (validate-setq undo-tree-auto-save-history t)
  (validate-setq undo-tree-history-directory-alist
                 `(("." . ,(concat user-emacs-directory "undo")))))


;; Save position in files
(use-package saveplace
  :init(save-place-mode t)
  :config (validate-setq save-place-file (expand-file-name "places" user-emacs-directory)))


;; Auto-revert of changed files
(use-package autorevert
  :init (global-auto-revert-mode t))


;; Insert matching delimiters
(use-package elec-pair
  :init (electric-pair-mode t))


;; Load shell env
(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (validate-setq exec-path-from-shell-variables
                 '("RUST_SRC_PATH"
                   "PATH"))

  (exec-path-from-shell-initialize))


;; Edit files as root, through Tramp
(use-package sudo-edit
  :ensure t
  :bind (("C-c f s" . sudo-edit)))


;; Highlight symbols in Emacs, used in leader binding
(use-package highlight-symbol
  :ensure t)


;; Show column
(use-package fill-column-indicator
  :ensure t
  :init
  (add-hook 'c-mode-common-hook #'fci-mode)
  (add-hook 'mail-mode-hook #'fci-mode)
  (add-hook 'js2-mode-hook #'fci-mode)
  :config
  (validate-setq fci-rule-width 1)
  (validate-setq fci-rule-color "gray71")
  (setq-default fill-column 80))


;; Emacs + vim = <3
(use-package evil
  :ensure t
  :bind (:map evil-normal-state-map
              ("Y" . copy-to-end-of-line)
              ("j" . evil-next-visual-line)
              ("k" . evil-previous-visual-line)
              ("gj" . evil-next-line)
              ("gk" . evil-previous-line))
  :init
  (evil-mode t)

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
      (let ((timer (run-at-time "0.2 sec" nil (lambda () (return-from return-point))))
            (key (read-key)))
        (if (eq key 27)
            (progn
              (cancel-timer timer)
              (save-buffer))))))

  (defun add-vim-bindings()
    (define-key evil-normal-state-local-map (kbd "<escape>") 'save-with-escape-and-timeout))

  :config
  ;; make the cursor stay in place after exiting insert mode
  (validate-setq evil-move-cursor-back nil)

  ;; Some modes should not start in evil-mode
  (evil-set-initial-state 'paradox-menu-mode 'emacs)
  (evil-set-initial-state 'el-get-package-menu-mode 'emacs)
  (evil-set-initial-state 'ag-mode 'emacs)
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (evil-set-initial-state 'pdf-annot-list-mode 'emacs)
  (evil-set-initial-state 'calendar-mode 'emacs)

  (use-package evil-leader
    :ensure t
    :init (global-evil-leader-mode)
    :config
    (evil-leader/set-key
      "f" 'onc/indent-whole-buffer
      "init" (lambda () (interactive) (find-file onc/init-el-path))
      "o" 'find-file
      "e" 'eval-defun
      "d" 'dictcc
      "1" 'highlight-symbol-at-point
      "0" 'highlight-symbol-remove-all
      "gst" 'magit-status
      "ci" 'evilnc-comment-or-uncomment-lines
      "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
      "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
      "cc" 'evilnc-copy-and-comment-lines
      "cp" 'evilnc-comment-or-uncomment-paragraphs
      "cr" 'comment-or-uncomment-region
      "cv" 'evilnc-toggle-invert-comment-line-by-line
      "a" 'align-regexp)

    (evil-leader/set-key-for-mode
      'c++-mode "f" 'clang-format-buffer)

    (evil-leader/set-key-for-mode
      'rust-mode "f" 'cargo-process-fmt))

  (use-package evil-search-highlight-persist
    :ensure t
    :init (global-evil-search-highlight-persist t)
    :config
    ;; Change highlight face of evil search
    (set-face-foreground 'evil-search-highlight-persist-highlight-face "#ff00ff")
    (set-face-background 'evil-search-highlight-persist-highlight-face "#ffffff"))

  (use-package evil-nerd-commenter
    :ensure t)

  (use-package evil-numbers
    :ensure t
    :bind (("<M-up>" . evil-numbers/inc-at-pt)
           ("<M-down>" . evil-numbers/dec-at-pt)))

  (use-package evil-surround
    :ensure t
    :init (global-evil-surround-mode t)))


;; On-the-fly syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t)
  :diminish flycheck-mode)


;; Show argument list in echo area
(use-package eldoc
  :diminish eldoc-mode)


;; Highlight matching delimiters
(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; Show colors in code
(use-package rainbow-mode
  :ensure t
  :diminish (rainbow-mode . "🌈")
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
  :defer 10
  :diminish company-mode
  :bind (:map company-active-map
              ("M-j" . company-select-next)
              ("M-k" . company-select-previous))
  :preface
  ;; enable yasnippet everywhere
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  :init (global-company-mode t)
  :config
  ;; no delay no autocomplete
  (validate-setq
   company-idle-delay 0
   company-minimum-prefix-length 2
   company-tooltip-limit 20)

  (validate-setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  ;; Sort company candidates by statistics
  (use-package company-statistics
    :ensure t
    :config (company-statistics-mode)))


;; Emojis completion like Github/Slack
(use-package company-emoji
  :ensure t
  :preface
  (defun --set-emoji-font (frame)
    "Adjust the font settings of FRAME so Emacs can display emoji properly."
    (if (eq system-type 'darwin)
        ;; For NS/Cocoa
        (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
      ;; For Linux
      (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))
  :config
  (add-to-list 'company-backends 'company-emoji)
  ;; For when Emacs is started in GUI mode:
  (--set-emoji-font nil)
  ;; Hook for when a frame is created with emacsclient
  ;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
  (add-hook 'after-make-frame-functions '--set-emoji-font))


;; Snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode t))


;; Notes
(use-package deft
  :ensure t
  :bind (([f6] . deft))
  :config
  (validate-setq
   deft-directory onc/deft-directory
   deft-extensions '("md" "org" "txt")
   deft-default-extension "org"
   deft-use-filename-as-title t
   deft-use-filter-string-for-filename t
   deft-recursive t
   deft-auto-save-interval 3.0))


;; Pdf in Emacs
(use-package pdf-tools
  :commands (pdf-tools-install)
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :bind
  (:map pdf-view-mode-map
        ("C-w l" . evil-window-right)
        ("C-w h" . evil-window-left)
        ("j" . pdf-view-next-line-or-next-page)
        ("k" . pdf-view-previous-line-or-previous-page)
        ("l" . pdf-view-next-page-command)
        ("h" . pdf-view-previous-page-command))
  :config (add-hook 'pdf-view-mode-hook #'pdf-view-fit-page-to-window))


;; Ido-mode replacement
(use-package helm
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
  :config
  (require 'helm-config)
  (helm-mode +1)

  ;; Ignore .DS_Store files with helm mode
  (add-to-list 'helm-boring-file-regexp-list "\\.DS_Store$")
  (validate-setq helm-ff-skip-boring-files t)

  ;; Fuzzy matching
  (validate-setq helm-mode-fuzzy-match t)
  (validate-setq helm-completion-in-region-fuzzy-match t)

  (validate-setq helm-ff-file-name-history-use-recentf t)

  (validate-setq helm-reuse-last-window-split-state t)
  ;; Don't use full width of the frame
  (validate-setq helm-split-window-in-side-p t)
  (helm-autoresize-mode t)

  ;; Use ack instead of grep
  (when (executable-find "ack")
    (validate-setq helm-grep-default-command "ack -Hn --no-group --no-color %p %f"
                   helm-grep-default-recurse-command "ack -H --no-group --no-color %p %f"))

  ;; Even better, use ag if it's available
  (when (executable-find "ag")
    (validate-setq helm-grep-default-command "ag --vimgrep -z %p %f"
                   helm-grep-default-recurse-command "ag --vimgrep -z %p %f"))

  (use-package helm-ag
    :ensure t)

  (use-package helm-dash
    :ensure t
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
    :config
    (validate-setq helm-dash-browser-func 'eww))

  (use-package helm-projectile
    :ensure t
    :init (helm-projectile-on)
    :config
    (validate-setq projectile-completion-system 'helm)))


;; Projects in emacs
(use-package projectile
  :ensure t
  :init (projectile-mode t))


;; Show git modifications
(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init (global-git-gutter-mode +1)
  :config
  ;; hide if there are no changes
  (validate-setq git-gutter:hide-gutter t)

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
  :load-path "git-packages/magit/lisp"
  :commands (magit-status)
  :config
  (validate-setq magit-diff-refine-hunk t)
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list "~/.emacs.d/git-packages/magit/Documentation"))

  (use-package magit-gitflow
    :load-path "git-packages/magit-gitflow"
    :init
    (add-hook 'magit-mode-hook #'turn-on-magit-gitflow)))


;; Requirement of Spaceline
(use-package powerline
  :ensure t
  :config
  ;; (validate-setq powerline-height (truncate (* 1.0 (frame-char-height)))
  ;;ns-use-srgb-colorspace nil
  )


;; Icon package
(use-package all-the-icons
  :ensure t
  :config
  (use-package all-the-icons-dired
    :ensure t))


;; Mode line
(use-package spaceline-config
  :ensure spaceline
  :config
  ;; Custom (iconized) spaceline theme
  (use-package spaceline-all-the-icons
    :load-path "spaceline"
    :config
    (use-package spaceline-colors
      :load-path "spaceline"
      :init (add-hook 'after-init-hook 'spaceline-update-faces)
      :config (advice-add 'load-theme :after 'spaceline-update-faces)))
  ;; set spacemacs theme
  ;; (spaceline-spacemacs-theme)
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati))))
  ;; show where you are in the file in the right corner
  (spaceline-toggle-hud-on)
  ;; set spaceline in helm-mode
  (spaceline-helm-mode))


;; Icons for Modes
(use-package mode-icons
  :ensure t
  :init (mode-icons-mode))


;; Better emacs package menu
(use-package paradox
  :commands (paradox-list-packages)
  :config
  (validate-setq paradox-automatically-star nil)
  (validate-setq paradox-display-star-count nil)
  (validate-setq paradox-execute-asynchronously t))


;; Code-comprehension server
(use-package ycmd
  :ensure t
  :init (add-hook 'c++-mode-hook #'ycmd-mode)
  :config
  (set-variable 'ycmd-server-command '("python" "/Users/onze/Applications/ycmd/ycmd"))
  (set-variable 'ycmd-global-config (expand-file-name "~/Applications/ycmd/cpp/ycm/.ycm_extra_conf.py"))

  (eval-after-load "flycheck-ycmd-autoloads" '(add-hook 'ycmd-mode-hook #'flycheck-ycmd-setup))

  (use-package company-ycmd
    :ensure t
    :init (company-ycmd-setup)
    :config (add-to-list 'company-backends (company-mode/backend-with-yas 'company-ycmd))))


;; Restclient in Emacs
(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode)
  :config

  (use-package company-restclient
    :ensure t
    :config (add-to-list 'company-backends 'company-restclient)))


;; Dict.cc in Emacs
(use-package dictcc
  :ensure t
  :config
  (validate-setq dictcc-source-lang "de")
  (validate-setq dictcc-destination-lang "en"))


;; Org-Mode
(use-package org
  :mode (("\\.org\\'" . org-mode)
         ("\\.org_archive\\'" . org-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("M-j" . org-forward-heading-same-level)
         ("M-k" . org-backward-heading-same-level))
  :config
  (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sh . t)
     (python . t)
     (ruby . t)
     (dot . t)
     (octave . t)
     (sqlite . t)
     (perl . t)))

  ;; fontify code in code blocks
  (validate-setq org-src-fontify-natively t)

  (validate-setq org-agenda-files onc/org-agenda-file-location)

  (use-package org-clock
    :config
    (validate-setq org-clock-persist 'history)
    (org-clock-persistence-insinuate))

  (use-package org-bullets
    :ensure t
    :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    :config (validate-setq org-bullets-bullet-list '("●" "◼" "▶" "♦"))))


;; Global emacs bindings with prefix
(use-package hydra
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
        (validate-setq args (list "--sug-mode=ultra" "--lang=de_DE"))
        (if RUN-TOGETHER
            (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2"))))))
      args))

  :config
  (cond
   ((executable-find "aspell")
    (validate-setq ispell-program-name "aspell"))
   (t (validate-setq ispell-program-name nil)))

  ;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
  ;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
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
  :bind (([f8]. fd-switch-dictionary))
  :preface
  (defun fd-switch-dictionary()
    (interactive)
    (let* ((dic ispell-current-dictionary)
           (change (if (string= dic "english") "deutsch" "english")))
      (ispell-change-dictionary change)
      (message "Dictionary switched from %s to %s" dic change))))


;; Text-checker interface for Emacs
(use-package wcheck-mode
  :ensure t
  :preface
  ;; This setup was initially found at
  ;; https://github.com/ReanGD/dotfiles/blob/414a6bd2eeb8df0fab2d93c5d6c5e90aa03c2e2d/.config/emacs/minor/spelling-cfg.el
  ;; and then modified to provide simultanous languages for spelling corrections.
  (defvar lcl-var:spelling-ignore nil)

  (defun lcl:spelling-add-to-dictionary (marked-text)
    (let* ((word (downcase (aref marked-text 0)))
           (dict (if (string-match "[a-zA-Z]" word)
                     (message "en_US.dic")
                   (message "de_DE.dic")))
           (file (concat "~/.config/enchant/" dict)))
      (when (and file (file-writable-p file))
        (with-temp-buffer
          (insert word) (newline)
          (append-to-file (point-min) (point-max) file)
          (message "Added word \"%s\" to the \"%s\" dictionary" word dict))
        (wcheck-mode 0)
        (wcheck-mode 1))))

  (defun lcl:spelling-add-to-ignore (marked-text)
    (let ((word (aref marked-text 0)))
      (add-to-list 'lcl-var:spelling-ignore word)
      (message "Added word \"%s\" to the ignore list" word)
      (wcheck--hook-outline-view-change)))

  (defun lcl:spelling-action-menu (marked-text)
    (append (wcheck-parser-lines)
            (list (cons "[Add to dictionary]" 'lcl:spelling-add-to-dictionary)
                  (cons "[Ignore]" 'lcl:spelling-add-to-ignore))))

  (defun lcl:delete-list (delete-list list)
    (dolist (el delete-list)
      (setq list (remove el list)))
    list)

  (defun lcl:spelling-parser-lines (&rest ignored)
    (lcl:delete-list lcl-var:spelling-ignore
                     (delete-dups
                      (split-string
                       (buffer-substring-no-properties (point-min) (point-max))
                       "\n+" t))))

  (defun lcl:spelling-hotkeys ()
    (define-key wcheck-mode-map (kbd "M-w RET") 'wcheck-actions)
    (define-key wcheck-mode-map (kbd "M-w l") 'wcheck-jump-forward)
    (define-key wcheck-mode-map (kbd "M-w j") 'wcheck-jump-backward))

  (defun lcl:wcheck ()
    (defun wcheck--choose-action-minibuffer (actions)
      (cdr
       (assoc
        (ido-completing-read "Choose " (mapcar #'car actions))
        actions)))
    (setq-default
     wcheck-language "All"
     wcheck-language-data
     '(("All"
        (program . "/home/onze/Applications/emacs-scripts/spell_check_text.sh")
        (parser . lcl:spelling-parser-lines)
        (action-program . "/home/onze/Applications/emacs-scripts/spell_check_word.py")
        (action-parser . lcl:spelling-action-menu)
        (read-or-skip-faces
         ((emacs-lisp-mode c-mode c++-mode python-mode)
          read font-lock-comment-face)
         (org-mode
          skip org-block-begin-line org-block-end-line org-meta-line org-link)
         (nil))
        )))
    (lcl:spelling-hotkeys))

  (defun wcheck-activate ()
    (interactive)
    (lcl:wcheck))
  :config
  (wcheck-activate))


;; Latex
(use-package ox-latex
  :defer t
  :ensure nil
  :config

  ;; XeLaTeX customisations
  ;; remove "inputenc" from default packages as it clashes with xelatex
  (setf org-latex-default-packages-alist
        (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))

  (validate-setq org-latex-pdf-process
                 '("xelatex --shell-escape -interaction nonstopmode -output-directory %o %f"
                   "biber $(basename %b)"
                   "xelatex --shell-escape -interaction nonstopmode -output-directory %o %f"
                   "xelatex --shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; you have to install pygmentize to use minted
  (validate-setq org-latex-packages-alist
                 (quote (("" "color" t) ("" "minted" t) ("" "parskip" t))))
  (validate-setq org-latex-listings 'minted)
  (validate-setq org-latex-minted-options
                 '(("frame" "lines")
                   ("linenos")
                   ("samepage")))

  ;; add emacs lisp support for minted
  (validate-setq org-latex-custom-lang-environments '((emacs-lisp "common-lisp")))
  (add-to-list 'org-latex-minted-langs '(elisp "common-lisp"))

  (add-to-list 'org-latex-classes
               '("thesis" "\\documentclass{thesis}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  ;; debugging new classes like this. this prevents to add the same class multiple times, by only setting this one
  (add-to-list 'org-latex-classes
               '("documentation" "
\\NeedsTeXFormat{LaTeX2e}
\\documentclass[a4paper,10pt,twoside,openright,numbers=noenddot,headings=normal]{scrbook}
[NO-DEFAULT-PACKAGES]

% default packages (without inputenc, because we use xetex)
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\tolerance=1000

% Encoding
\\usepackage[ngerman,english]{babel}
\\usepackage{fontspec}
\\usepackage{polyglossia}

% Fonts
\\setmainfont{Source Serif Pro}
\\setsansfont{Source Sans Pro}
\\setromanfont{Source Sans Pro}
\\setmonofont{Source Code Pro}[Scale=MatchLowercase]

% \\linespread{1.05}                      % Palatino needs more leading (space between lines)
% \\usepackage{setspace}
% \\setstretch{1.4}
% \\usepackage{microtype}

% \\usepackage{framed}
% \\usepackage{xcolor}
% \\definecolor{shadecolor}{gray}{.95}
% \\newenvironment{results}{\\begin{shaded}}{\\end{shaded}}

% Default packages
\\usepackage{multirow}                  % Table rows multiline
\\usepackage{graphicx}
\\usepackage{verbatim}
\\usepackage{subfigure}
\\usepackage{url}
\\usepackage{amssymb}
\\usepackage{amsmath}

% biblio
\\usepackage{cite}

% Layout
\\usepackage[scale=0.70, marginratio={4:5, 3:4}, ignoreall, headsep=8mm]{geometry}
\\setlength{\\parskip}{1.4ex plus 0.35ex minus 0.3ex}
\\renewcommand\\arraystretch{1.3}       % stretch lines in tables
\\clubpenalty10000                      % no orphan lines
\\widowpenalty10000                     % no widowed lines
\\setcounter{tocdepth}{3}               % max depth of in toc

% Header and Footer
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\fancyhead[RO]{\\slshape \\rightmark}
\\fancyhead[LE]{\\slshape \\leftmark}
\\fancyhead[LO,RE]{}
\\fancyheadoffset[L,R]{0.5cm}
\\fancypagestyle{plain}{
\\fancyhf{}                           % clear all header and footer fields
\\fancyfoot[C]{\\thepage}             % except the center
\\renewcommand{\\headrulewidth}{0pt}
\\renewcommand{\\footrulewidth}{0pt}}

\\usepackage{hyperref}
\\hypersetup{
colorlinks=false,
pdfborder=0 0 0                       % no boxes on links
}
"
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("djcb-org-article" "\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{graphicx}
\\usepackage{geometry}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}
\\title{}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


;;; Language support
;;; ----------------

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (use-package cargo
    :ensure t
    :bind(:map
          rust-mode-map
          ("C-c c" . cargo-process-build)
          ("C-c x" . cargo-process-run))
    :init (add-hook 'rust-mode-hook #'cargo-minor-mode))

  (use-package racer
    :ensure t
    :init (add-hook 'rust-mode-hook #'racer-mode)
    :config
    (validate-setq racer-rust-src-path (getenv "RUST_SRC_PATH")))

  (use-package flycheck-rust
    :ensure t
    :init (add-hook 'rust-mode-hook #'flycheck-rust-setup))

  (use-package company-racer
    :ensure t
    :config (add-to-list 'company-backends 'company-racer)))


;; Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (use-package elpy
    :config
    (validate-setq elpy-rpc-backend "jedi")
    (validate-setq elpy-modules (delq 'elpy-module-company elpy-modules))
    (elpy-enable)

    (add-hook 'python-mode-hook
              (lambda ()
                (company-mode)
                (add-to-list 'company-backends
                             (company-mode/backend-with-yas 'elpy-company-backend))))
    (elpy-use-cpython)))


;; Ruby
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :bind(:map
        ruby-mode-map
        ("C-c r" . onc/run-current-file))
  :config
  (use-package robe
    :ensure t
    :init
    (add-hook 'ruby-mode-hook #'robe-mode)
    (add-to-list 'company-backends (company-mode/backend-with-yas 'company-robe)))

  (use-package rubocop
    :ensure t)

  (use-package inf-ruby
    :ensure t
    :init
    (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
    (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter))

  (use-package company-inf-ruby
    :ensure t
    :config (add-to-list 'company-backends 'company-inf-ruby)))


;; Elisp
(use-package elisp-mode
  :interpreter ("emacs" . emacs-lisp-mode))


;; Javascript
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq-default js2-global-externs '("exports" "module" "require" "setTimeout" "THREE"))
  (setq-default js2-basic-offset 2))

(use-package tern
  :defer t
  :init
  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  (add-hook 'web-mode-hook (lambda () (tern-mode t)))
  :config
  (use-package company-tern
    :ensure t
    :config (add-to-list 'company-backends 'company-tern)))


;; GLSL
(use-package glsl-mode
  :mode (("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)))


;; Haskell
(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (use-package flycheck-haskell
    :ensure t
    :init (add-hook 'haskell-mode-hook #'flycheck-haskell-setup)))


;; Groovy
(use-package groovy-mode
  :mode "\\.groovy\\'")


;; C/C++
(use-package cc-mode
  :ensure t
  :config (setq-default c-basic-offset 2))

(use-package c++-mode
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
  (add-hook 'c++-mode-hook (lambda () (c-set-style "stroustrup")))

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
    (let* ((build-path-exists (dirvars-find-upwards "build"))
           (cmakelists-dir (file-name-directory (dirvars-find-upwards "CMakeLists.txt")))
           (build-path (concat cmakelists-dir "build"))
           )
      (if build-path-exists
          (compile (concat "make -k -C " build-path))
        ;; create build path and run cmake
        (make-directory build-path)
        (call-process "cmake" nil nil nil (concat "-B" build-path) (concat "-H" cmakelists-dir)))))

  (defun my-executable-path ()
    "Returns ...."
    (with-temp-buffer
      (if (zerop
           (call-process "/bin/bash" nil t nil "-c"
                         (concat
                          (concat "find " (dirvars-find-upwards "build")) " -executable -type f | grep -v CMake")))
          (buffer-substring (point-min) (1- (point-max)))
        nil)))

  (defun onc/cmake-run ()
    (interactive)
    (call-process (my-executable-path)))

  :config
  (use-package clang-format
    :commands (clang-format-buffer)
    :config (validate-setq clang-format-executable onc/clang-format-command-path))

  (use-package company-cmake
    :ensure t
    :config (add-to-list 'company-backends 'company-cmake)))


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


;; Web-Mode for html, php and the like
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php\\'"   . web-mode)
         ("\\.jsp\\'"   . web-mode)
         ("\\.erb\\'"   . web-mode))
  :config
  (validate-setq web-mode-markup-indent-offset 2
                 web-mode-css-indent-offset 2
                 web-mode-code-indent-offset 2))


;; XML files
(use-package nxml-mode
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.xslt\\'" . nxml-mode)
         ("\\.xsd\\'" . nxml-mode))
  :config
  (validate-setq nxml-child-indent 2
                 nxml-attribute-indent 2))


;; Support for AsciiDoc
(use-package adoc-mode
  :ensure t)


;; Gradle-files
(use-package gradle-mode
  :mode "\\.gradle\\'")


;; Json-files
(use-package json-mode
  :mode "\\.json\\'"
  :config (validate-setq js-indent-level 2))


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
            ("py" . "python")
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
If `universal-argument' is called first, use CAPITAL letters.
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


;; SSH Setup
(defun read-file (filename)
  "Read the data from FILENAME and return it as a string."
  (let ((real-filename (expand-file-name filename)))
    (with-temp-buffer
      (insert-file-contents real-filename)
      (buffer-string))))

(defun load-ssh-agent-env ()
  "Read an SSH agent environment file and set the necessary environment variables."
  (interactive)
  (let ((agent-env-fn (concat (file-name-as-directory (getenv "HOME"))
                              (file-name-as-directory ".ssh")
                              "agent_env.sh")))
    (if (file-readable-p agent-env-fn)
        (let* ((ssh-data (read-file agent-env-fn))
               (auth-sock (progn
                            (string-match "SSH_AUTH_SOCK=\\(.*?\\)\n" ssh-data)
                            (match-string 1 ssh-data)))
               (agent-pid (progn
                            (string-match "SSH_AGENT_PID=\\([0-9]*\\)?;" ssh-data)
                            (match-string 1 ssh-data)))
               )
          (setenv "SSH_AUTH_SOCK" auth-sock)
          (setenv "SSH_AGENT_PID" agent-pid)
          (list auth-sock agent-pid)
          (message (format "Using SSH agent %s via %s" agent-pid auth-sock)))
      (message (format "No SSH agent environment file found: " agent-env-fn)))))

(load-ssh-agent-env)


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
