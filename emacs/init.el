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

;; Path of themes
(defconst onc/custom-theme-load-path (expand-file-name "~/.emacs.d/themes"))

;; Font for emacs
(defconst onc/font-family "Hack")

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

;; Highlight some keywords in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)" 1 font-lock-warning-face t)))))

;; Set programm for urls
(validate-setq browse-url-browser-function 'browse-url-chromium)

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
                      (if (string= (file-truename (expand-file-name "~/.emacs.d/init.el"))
                                   (file-truename (buffer-file-name)))
                          (byte-compile-init-files "~/.emacs.d/init.el")))))

;; Byte-compile again who init.elc If it is older than init.el.
(if (file-newer-than-file-p (expand-file-name "~/.emacs.d/init.el")
                            (expand-file-name "~/.emacs.d/init.elc"))
    (byte-compile-init-files "~/.emacs.d/init.el"))

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


(use-package font-lock+
  :ensure t
  :load-path "git-packages/font-lock+")


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

;; Simple
(use-package simple
  :defer t
  :init
  (validate-setq column-number-mode t))


;; Reveal current buffer in finder
(use-package reveal-in-osx-finder
  :bind (("C-c f J" . reveal-in-osx-finder)))


;; Fringe mode (left and right borders stuff)
(use-package fringe
  :init (fringe-mode '(4 . 0)))


(use-package compile
  :config (validate-setq compilation-scroll-output t))


;; Save recent files
(use-package recentf
  :init(recentf-mode t)
  :config (validate-setq recentf-max-saved-items 1000))


;; Undo with branching
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (validate-setq undo-tree-auto-save-history nil)
  (validate-setq undo-tree-history-directory-alist
                 `(("." . ,(concat user-emacs-directory "undo"))))
  (validate-setq undo-tree-visualizer-timestamps t
                 undo-tree-visualizer-diff t)
  (global-undo-tree-mode))


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
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "RUST_SRC_PATH"))


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
              ("gk" . evil-previous-line)
              ("C-u" . evil-scroll-up)
              ("C-d" . evil-scroll-down))
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
  ;; make the cursor stay in place after exiting insert mode
  (validate-setq evil-move-cursor-back nil)

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
      "glg" 'magit-log-all
      "ci" 'evilnc-comment-or-uncomment-lines
      "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
      "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
      "cc" 'evilnc-copy-and-comment-lines
      "cp" 'evilnc-comment-or-uncomment-paragraphs
      "cr" 'comment-or-uncomment-region
      "cv" 'evilnc-toggle-invert-comment-line-by-line
      "a" 'align-regexp
      "s" 'helm-projectile-ag)

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


;; Workspaces in emacs
(use-package perspeen
  :ensure t
  :config (perspeen-mode t))


;; On-the-fly syntax checking
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init (global-flycheck-mode t))


;; Show argument list in echo area
(use-package eldoc
  :diminish eldoc-mode
  :init (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))


;; Highlight matching delimiters
(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; Show colors in code
(use-package rainbow-mode
  :ensure t
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
    (if (or
         (not company-mode/enable-yas)
         (and (listp backend) (member 'company-yasnippet backend)))
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

  ;; remove unused backends
  (validate-setq company-backends (delete 'company-semantic company-backends))
  (validate-setq company-backends (delete 'company-eclim company-backends))
  (validate-setq company-backends (delete 'company-xcode company-backends))
  (validate-setq company-backends (delete 'company-clang company-backends))
  (validate-setq company-backends (delete 'company-cmake company-backends))
  (validate-setq company-backends (delete 'company-bbdb company-backends))
  (validate-setq company-backends (delete 'company-oddmuse company-backends))

  (validate-setq company-backends
                 (mapcar #'company-mode/backend-with-yas company-backends))

  ;; Sort company candidates by statistics
  (use-package company-statistics
    :ensure t
    :config (company-statistics-mode)))


;; Emojis completion like Github/Slack
(use-package company-emoji
  :ensure t
  :init
  (add-to-list 'company-backends (company-mode/backend-with-yas 'company-emoji)))


;; Snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode t)
  :config
  ;; preserve tab-completion in ansi-term
  (add-hook 'term-mode-hook (lambda()
                              (setq yas-dont-activate t))))

;; Terminal in emacs
(use-package multi-term
  :commands multi-term
  :config
  (setq multi-term-program "/bin/zsh"))

;; Notes
(use-package deft
  :commands deft
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

(use-package dired
  :config
  (evil-make-overriding-map dired-mode-map 'normal t)
  (evil-define-key 'normal dired-mode-map
    "j" 'evil-next-line
    "k" 'evil-previous-line
    "h" 'dired-up-directory
    "l" 'dired-find-file))


;; Pdf in Emacs
;; (use-package pdf-tools
;;   :commands (pdf-tools-install)
;;   :mode (("\\.pdf\\'" . pdf-view-mode))
;;   :bind (:map pdf-view-mode-map
;;               ("C-w l" . evil-window-right)
;;               ("C-w h" . evil-window-left)
;;               ("j" . pdf-view-next-line-or-next-page)
;;               ("k" . pdf-view-previous-line-or-previous-page)
;;               ("l" . pdf-view-next-page-command)
;;               ("h" . pdf-view-previous-page-command))
;;   :config (add-hook 'pdf-view-mode-hook #'pdf-view-fit-page-to-window))

(use-package doc-view
  :ensure t
  :bind (:map doc-view-mode-map
              ("j" . doc-view-next-page)
              ("<SPC>" . doc-view-next-page)
              ("k" . doc-view-previous-page))
  :init (add-hook 'doc-view-mode-hook #'doc-view-fit-page-to-window))


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

  (use-package helm-swoop
    :ensure t
    :bind (("M-i" . helm-swoop)
           ("M-I" . helm-multi-swoop-projectile)))

  (use-package helm-projectile
    :ensure t
    :init (helm-projectile-on)
    :config
    (validate-setq projectile-completion-system 'helm)))


;; Projects in emacs
(use-package projectile
  :ensure t
  :config
  (validate-setq projectile-mode-line
                 '(:eval (format " Proj[(%s)]"
                                 (projectile-project-name))))
  (projectile-global-mode))


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
  :commands (magit-status magit-log-all)
  :config
  (validate-setq magit-diff-refine-hunk t)
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list "~/.emacs.d/git-packages/magit/Documentation"))

  (use-package magit-gitflow
    :load-path "git-packages/magit-gitflow"
    :init
    (add-hook 'magit-mode-hook #'turn-on-magit-gitflow)))


;; Moodle-destroyer plugin
(use-package moodle-destroyer
  :load-path "/Users/onze/Repos/moodle-destroyer.el/lisp"
  :bind(:map
        moodle-destroyer-mode-map
        ("C-c C-c" . moodle-destroyer-org-to-json))
  :commands (moodle-destroyer-json-to-org
             moodle-destroyer-org-to-json)
  :config
  ;; set custom name for org-mode gradingfile
  (setq moodle-destroyer-gradingfile-org-name "grading.org")
  ;; set custom name for exported json file
  (setq moodle-destroyer-gradingfile-json-name "grading.ex.json"))


;; Requirement of Spaceline
(use-package powerline
  :ensure t
  :config

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
        :config (advice-add 'load-theme :after 'spaceline-update-faces)

        (validate-setq powerline-height 13)
        (setq-default powerline-default-separator 'nil)
        (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati))))
        (spaceline-helm-mode)))))


(use-package atom-one-dark-theme
  :defer t
  ;; :config (load-theme 'atom-one-dark t)
  )


(use-package spacemacs-theme
  :ensure t)


;; Better emacs package menu
(use-package paradox
  :commands (paradox-list-packages)
  :config
  (validate-setq paradox-automatically-star nil
                 paradox-display-star-count nil
                 paradox-execute-asynchronously t))


;; Code-comprehension server
(use-package ycmd
  :defer t
  :init (add-hook 'c++-mode-hook #'ycmd-mode)
  :config
  (set-variable 'ycmd-server-command '("python3" "/Users/onze/Applications/ycmd/ycmd"))
  (set-variable 'ycmd-global-config (expand-file-name "~/Repos/dotfiles/ycmd/ycm_conf.py"))

  (set-variable 'ycmd-extra-conf-whitelist '("~/Uni/*" "~/Repos/*"))

  (use-package flycheck-ycmd
    :commands (flycheck-ycmd-setup)
    :init (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))

  (use-package company-ycmd
    :commands (company-ycmd-setup)
    :config (add-to-list 'company-backends (company-mode/backend-with-yas 'company-ycmd))
    ;; :config (add-to-list 'company-backends 'company-ycmd)
    ))



;; Restclient in Emacs
(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode)
  :config

  (use-package company-restclient
    :ensure t
    :config (add-to-list 'company-backends 'company-restclient)))


;; Dict.cc in Emacs
(use-package dictcc
  :commands dictcc
  :config
  (validate-setq dictcc-source-lang "de"
                 dictcc-destination-lang "en"))


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

  ;; (validate-setq org-agenda-files onc/org-agenda-file-location)

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
  :config
  (validate-setq
   ispell-program-name (if (eq system-type 'darwin)
                           (executable-find "aspell")
                         (executable-find "hunspell"))
   ispell-dictionary "en_US")
  (unless ispell-program-name
    (warn "No spell-checker available. Please install aspell or Hunspell")))


;; Automatically infer dictionary
(use-package auto-dictionary
  :ensure t
  :init
  (add-hook 'flyspell-mode-hook #'auto-dictionary-mode))


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
                   ("linenos" "")
                   ("samepage" "")))

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
  :interpreter ("python3" . python-mode)
  :bind(:map
        python-mode-map
        ("C-c r" . onc/run-current-file))
  :config

  (use-package elpy
    :ensure t
    :config
    (elpy-enable)
    (validate-setq elpy-rpc-backend "jedi")
    (validate-setq elpy-modules (delq 'elpy-module-company elpy-modules))

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


(use-package rjsx-mode
  :mode "\\.jsx\\'"
  :config
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil))


(use-package tern
  :defer t
  :init
  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  (add-hook 'web-mode-hook (lambda () (tern-mode t)))
  (add-hook 'rjsx-mode-hook (lambda () (tern-mode t)))
  :config
  (use-package company-tern
    :ensure t
    :config (add-to-list 'company-backends 'company-tern)))


;; Applescript
(use-package apples-mode
  :mode (("\\.scpt\\'" . apples-mode)
         ("\\.scptd\\'" . apples-mode)
         ("\\.AppleScript\\'" . apples-mode)))


;; GLSL
(use-package glsl-mode
  :mode (("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)))


;; Haskell
(use-package haskell-mode
  :mode "\\.hs\\'"
  :bind (:map haskell-mode-map
              ("C-c c" . haskell-process-load-file))
  :config
  (validate-setq haskell-interactive-popup-errors nil))

(use-package flycheck-haskell
  :mode "\\.hs\\'"
  :init (add-hook 'haskell-mode-hook #'flycheck-haskell-setup))


;; Groovy
(use-package groovy-mode
  :mode "\\.groovy\\'")


;; support for R
(use-package ess
  :mode "\\.r\\'")


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
                          (concat "find " (dirvars-find-upwards "debug")) " -perm +111 -type f | grep -v CMake")))
          (buffer-substring (point-min) (1- (point-max)))
        nil)))

  (defun onc/cmake-run ()
    (interactive)
    (call-process (my-executable-path)))
  :config

  (use-package clang-format
    :commands (clang-format-buffer)
    :config
    (validate-setq clang-format-executable onc/clang-format-command-path))

  (use-package company-cmake
    :ensure t
    :config (add-to-list 'company-backends 'company-cmake))

  (use-package modern-cpp-font-lock
    :ensure t
    :init (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)))


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
      (message (format "No SSH agent environment file found: %s" agent-env-fn)))))

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
