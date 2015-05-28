;;; package --- Summary

;; -*- coding: utf-8; lexical-binding: t -*

;;; Commentary:

;;; Code:

;; #############################################################################
;; ################################# SOURCES ###################################
;; #############################################################################

(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;; from purcell/emacs.d
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;; #############################################################################
;; ################################# SETTINGS ##################################
;; #############################################################################

;; start emacs in server mode
(load "server")
(unless (server-running-p) (server-start))


;; disable toolbar
(tool-bar-mode -1)
;; disable scrollbar
(scroll-bar-mode -1)

;; disable cursor blink
(blink-cursor-mode 0)

;; highlight current line
(global-hl-line-mode t)

;; save current session
(desktop-save-mode t)

(recentf-mode t)
(setq recentf-max-saved-items 1000)

;; If this is non-nil, then when both .el and .elc
;; versions of a file exist, and the caller did not explicitly specify
;; which one to load, then the newer file is loaded.
(setq load-prefer-newer t)

;; Show system name and full file path in emacs frame title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; line numbers
;; (global-linum-mode 1)

;; theme
;; (require-package 'base16-theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; set font
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline-9" ))
(set-face-attribute 'default t :font "DejaVu Sans Mono for Powerline-9")

;; fucking use spaces emacs
(setq-default tab-width 4 indent-tabs-mode nil)

;; set indention in c files
(setq-default c-basic-offset 4)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
;; Write auto save files to own directory
;; http://stackoverflow.com/a/2020954/29618
(defvar autosave-dir (expand-file-name (concat user-emacs-directory "autosaves/")))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

;; Don't make me type 'yes' or 'no', y/n will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; dont ask to follow symlink in git
(setq vc-follow-symlinks t)

;; Nonzero means echo unfinished commands after this many seconds of pause.
;; The value may be integer or floating point.
(setq echo-keystrokes 0.02)

;; Auto refresh buffers
(global-auto-revert-mode t)

;; Make sure UTF-8 is used
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Add .h and .cc files to c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))

;; email mode
(add-to-list 'auto-mode-alist '("\\.mail\\'" . mail-mode))
(add-to-list 'auto-mode-alist '("\\.eml\\'" . mail-mode))
(add-hook 'mail-mode-hook
          (lambda ()
            (setq fill-column 80)
            (auto-fill-mode t)))

;; #############################################################################
;; ################################# PACKAGES ##################################
;; #############################################################################

;; load use-package module
(require-package 'use-package)
(require 'use-package)

;; make emacs usable
(use-package evil
  :ensure t
  :config (progn
            (evil-mode 1)
            (setq evil-move-cursor-back nil)

            ;; Some modes should not start in evil-mode
            (evil-set-initial-state 'paradox-menu-mode 'emacs)
            (evil-set-initial-state 'el-get-package-menu-mode 'emacs)
            (evil-set-initial-state 'ag-mode 'emacs)
            (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
            (evil-set-initial-state 'dired-mode 'emacs)
            (evil-set-initial-state 'neotree-mode 'emacs)
            (evil-set-initial-state 'magit-popup-mode 'emacs)
            (evil-set-initial-state 'magit-mode 'emacs)

            (defun copy-to-end-of-line ()
              "Yank from point to end of line."
              (interactive)
              (evil-yank (point) (point-at-eol)))
            ;; copy to end of line
            (define-key evil-normal-state-map "Y" 'copy-to-end-of-line)

            ;; if no second escape is pressed in a given timeout, dont wait for a second escape
            (defun save-with-escape-and-timeout ()
              (interactive)
              (block return-point
                (let ((timer (run-at-time "0.2 sec" nil (lambda () (return-from return-point))))
                      (key (read-key)))
                  (if (eq key 27)
                      (progn
                        (cancel-timer timer)
                        (save-buffer))))))

            ;; save on double escape and space for command mode
            (defun add-vim-bindings()
              (define-key evil-normal-state-local-map (kbd "<escape>") 'save-with-escape-and-timeout))
            (define-key evil-normal-state-local-map (kbd "<SPC>") 'evil-ex)
            (define-key evil-normal-state-local-map (kbd "<DEL>") 'evil-search-highlight-persist-remove-all)

            (add-hook 'evil-normal-state-entry-hook 'add-vim-bindings)

            (use-package evil-leader
              :ensure t
              :config (progn
                        (global-evil-leader-mode)

                        (evil-leader/set-key
                          "n" 'neotree-toggle
                          "f" 'onze-indent-whole-buffer
                          "init" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
                          "b" 'helm-mini
                          "o" 'find-file
                          "e" 'eval-defun
                          "1" 'highlight-symbol-at-point
                          "0" 'highlight-symbol-remove-all
                          "gst" 'magit-status
                          "p" 'helm-projectile)))

            (use-package evil-search-highlight-persist
              :ensure t
              :config (progn
                        (global-evil-search-highlight-persist)))

            ;; PACKAGE: EVIL-SURROUND
            (use-package evil-surround
              :defer t
              :ensure t
              :config (progn
                        (global-evil-surround-mode 1)))))

(use-package smartparens
  :ensure t
  :config (progn
            (smartparens-global-mode t)
            (show-smartparens-global-mode)))

(use-package org
  :config (progn
            (setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))))

;; (use-package smooth-scrolling
;;   :ensure t
;;   :config (progn
;;             ;; (setq scroll-margin 5)
;;             ;; (setq scroll-conservatively 10000)
;;             (setq scroll-step 1)
;;             (setq auto-window-vscroll nil)
;;
;;             (setq scroll-margin 1
;;                   scroll-conservatively 0
;;                   scroll-up-aggressively 0.01
;;                   scroll-down-aggressively 0.01)
;;             (setq-default scroll-up-aggressively 0.01
;;                           scroll-down-aggressively 0.01)))

(use-package undo-tree
  :ensure t
  :config (progn
            (setq undo-tree-auto-save-history t)
            (setq undo-tree-history-directory-alist
                  `(("." . ,(concat user-emacs-directory "undo"))))
            (global-undo-tree-mode)))

(use-package rainbow-delimiters
  :ensure t
  :config (progn
            (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package company
  :ensure t
  :config (progn
            (add-hook 'after-init-hook 'global-company-mode)
            ;; no delay no autocomplete
            (setq company-idle-delay 0)
            (setq company-minimum-prefix-length 2)

            (use-package company-cmake
              :ensure t)))

(use-package neotree
  :ensure t
  :defer t
  :config (progn
            (global-set-key [f8] 'neotree-toggle)
            (evil-set-initial-state 'neotree-mode 'emacs)
            (setq neo-theme 'arrow)

            (define-key neotree-mode-map (kbd "j") 'next-line)
            (define-key neotree-mode-map (kbd "k") 'previous-line)
            (define-key neotree-mode-map (kbd "s") 'neotree-enter-vertical-split)
            (define-key neotree-mode-map (kbd "i") 'neotree-enter-horizontal-split)
            (define-key neotree-mode-map (kbd "C-w l") 'other-window)
            ;; (define-key neotree-mode-map (kbd "<escape>") 'neotree-hide)
            ;; If this variable is non-nil then iT's not possible to
            ;; get help in Helm buffers
            ;; https://github.com/jaypei/emacs-neotree/issues/26
            (setq neo-persist-show nil)
            (add-hook 'neotree-mode-hook
                      (lambda ()
                        (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                        (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
                        (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                        (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))))

(use-package helm
  :ensure t
  :config (progn
            (helm-mode 1)
            (setq helm-buffer-details-flag nil)
            ;; fuzzy matching
            (setq helm-apropos-fuzzy-match t)
            (setq helm-ff-file-name-history-use-recentf t)

            (setq helm-reuse-last-window-split-state t)
            ;; Don't use full width of the frame
            (setq helm-split-window-in-side-p t)
            (helm-autoresize-mode t)

            ;; rebind tab to do persistens action
            (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
            ;; make tab work in terminal
            (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
            ;; list actions using C-j
            (define-key helm-map (kbd "C-j") 'helm-select-action)
            (global-set-key (kbd "C-h C-h") 'helm-apropos)
            ;; use helm for meta x
            (global-set-key (kbd "M-x") 'helm-M-x)
            (global-set-key (kbd "C-x b") 'helm-mini)

            (use-package helm-projectile
              :ensure t
              :config (progn
                        (setq projectile-completion-system 'helm)
                        (helm-projectile-on)))))

(use-package hydra
  :ensure t
  :config (progn

            (global-set-key (kbd "C-x m")
                            (defhydra hydra-onze (:color teal)
                              "
      Onzes functions

     Buffers                       Blog                       Projects
-------------------------------------------------------------------------------------------
  _i_: indent buffer              _n_: create new blog post    _p_: switch project %(my-where-is-first 'helm-projectile-switch-project)
  _r_: rename buffer and file                                _s_: start clock
                                                           _f_: stop/finish clock
                                                           _t_: create report-table
"
                              ("i" onze-indent-whole-buffer       nil)
                              ("r" onze-rename-file-and-buffer    nil)
                              ("n" onze-create-new-blog-post      nil)
                              ("p" helm-projectile-switch-project nil)
                              ("s" org-clock-in                   nil)
                              ("f" org-clock-out                  nil)
                              ("t" org-clock-report               nil)
                              ("q" nil                            "cancel" :color blue)))))

(use-package git-gutter
  :ensure t
  :config (progn
            (global-git-gutter-mode +1)
            ;; hide if there are no changes
            (setq git-gutter:hide-gutter t)))

(use-package ycmd
  :ensure t
  :config (progn
            (add-hook 'c++-mode-hook 'ycmd-mode)
            (set-variable 'ycmd-server-command '("python2" "/home/onze/Applications/ycmd/ycmd"))
            (set-variable 'ycmd-global-config "/home/onze/Applications/ycmd/cpp/ycm/.ycm_extra_conf.py")

            (eval-after-load "flycheck-ycmd-autoloads" '(add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))))

(use-package company-ycmd
  :ensure t
  :config (progn
            (company-ycmd-setup)))

(use-package projectile
  :ensure t
  :config (progn
            (projectile-global-mode)))

(use-package powerline
  :ensure t
  :config (progn
            (powerline-default-theme)))

(use-package yasnippet
  :ensure t
  :config (progn
            (yas-global-mode 1)
            (global-set-key (kbd "C-c y") 'company-yasnippet)
            ;; enable yasnippet everywhere
            (defvar company-mode/enable-yas t
              "Enable yasnippet for all backends.")
            (defun company-mode/backend-with-yas (backend)
              (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
                  backend
                (append (if (consp backend) backend (list backend))
                        '(:with company-yasnippet))))

            (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))))

(use-package magit
  :defer t
  :load-path "/home/onze/.emacs.d/git-package/magit"
  :config (progn

            (add-to-list 'load-path "/home/onze/.emacs.d/git-package/magit-gitflow")
            (require 'magit-gitflow)
            (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

            (use-package magit-gitflow
              :load-path "/home/onze/.emacs.d/git-packages/magit-gitflow"
              :disabled t
              :config (progn
                        add-hook 'magit-mode-hook 'turn-on-magit-gitflow))))

(use-package org-clock
  :defer t
  :config (progn
            (setq org-clock-persist 'history)
            (org-clock-persistence-insinuate)))

(use-package async
  :defer t
  :ensure t
  :config (progn
            (require 'async-bytecomp)))

(use-package dash
  :defer t
  :ensure t)

(use-package paradox
  :ensure t
  :defer t
  :commands (paradox-list-packages))

(use-package glsl-mode
  :ensure t
  :mode (("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)))

(use-package cmake-mode
  :ensure t)

(use-package highlight-symbol
  :ensure t)

(use-package diminish
  :ensure t
  :config (progn
            ;; Clean up mode line
            ;; (eval-after-load "company" '(diminish 'company-mode "cpy"))
            (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
            (eval-after-load "eldoc" '(diminish 'eldoc-mode))
            (eval-after-load "helm" '(diminish 'helm-mode))

            (add-hook 'emacs-lisp-mode-hook
                      (lambda ()
                        (setq mode-name "el")))))

(use-package scss-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :defer t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php\\'"   . web-mode)
         ("\\.jsp\\'"   . web-mode)
         ("\\.erb\\'"   . web-mode)))

(use-package yaml-mode
  :ensure t)

(use-package google-c-style
  :ensure t
  :config (progn
            (add-hook 'c-mode-common-hook 'google-set-c-style)))

(use-package fill-column-indicator
  :ensure t
  :config (progn
            (setq fci-rule-width 1)
            (setq fci-rule-color "gray71")
            (add-hook 'c-mode-common-hook 'fci-mode)
            (add-hook 'mail-mode-hook 'fci-mode)))


(use-package markdown-mode
  :ensure t
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
            (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
            (add-to-list 'auto-mode-alist '("\\.mmd\\'" . markdown-mode))

            (add-hook 'markdown-mode-hook 'orgtbl-mode)
            (add-hook 'markdown-mode-hook
                      (lambda()
                        (add-hook 'after-save-hook 'org-tables-to-markdown  nil 'make-it-local)))))

(use-package rainbow-mode
  :ensure t
  :config (progn
            (dolist
                (hook '(css-mode-hook
                        html-mode-hook
                        js-mode-hook
                        emacs-lisp-mode-hook
                        ;; org-mode-hook
                        text-mode-hook))
              (add-hook hook 'rainbow-mode))))

(use-package flycheck
  :ensure t
  :config (progn
            (flycheck-mode 1)
            (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))
            (flycheck-add-next-checker 'c/c++-clang 'c/c++-googlelint 'append)))

(use-package flycheck-google-cpplint
  :ensure t
  :config (progn
            (custom-set-variables
             '(flycheck-c/c++-googlelint-executable "/usr/local/bin/cpplint.py"))))

(use-package flyspell
  :ensure t
  :config (progn
            ;; if (aspell installed) { use aspell}
            ;; else if (hunspell installed) { use hunspell }
            ;; whatever spell checker I use, I always use English dictionary
            ;; I prefer use aspell because:
            ;; 1. aspell is older
            ;; 2. looks Kevin Atkinson still get some road map for aspell:
            ;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
            (defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
              "If RUN-TOGETHER is true, spell check the CamelCase words."
              (let (args)
                (cond
                 ((string-match  "aspell$" ispell-program-name)
                  ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
                  (setq args (list "--sug-mode=ultra" "--lang=en_US"))
                  (if RUN-TOGETHER
                      (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
                 ((string-match "hunspell$" ispell-program-name)
                  (setq args nil)))
                args
                ))

            (cond
             ((executable-find "aspell")
              (setq ispell-program-name "aspell"))
             ((executable-find "hunspell")
              (setq ispell-program-name "hunspell")
              ;; just reset dictionary to the safe one "en_US" for hunspell.
              ;; if we need use different dictionary, we specify it in command line arguments
              (setq ispell-local-dictionary "en_US")
              (setq ispell-local-dictionary-alist
                    '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
             (t (setq ispell-program-name nil)))

            ;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process
            ;; when "ispell-word" is called.
            ;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
            (setq ispell-extra-args (flyspell-detect-ispell-args t))
            ;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
            (defadvice ispell-word (around my-ispell-word activate)
              (let ((old-ispell-extra-args ispell-extra-args))
                (ispell-kill-ispell t)
                (setq ispell-extra-args (flyspell-detect-ispell-args))
                ad-do-it
                (setq ispell-extra-args old-ispell-extra-args)
                (ispell-kill-ispell t)
                ))

            (eval-after-load "flyspell"
              '(progn
                 (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
                 (define-key flyspell-mouse-map [mouse-3] #'flyspell-correct-word)))))

(use-package ispell
  :defer t
  :config (progn
            (defun fd-switch-dictionary()
              (interactive)
              (let* ((dic ispell-current-dictionary)
                     (change (if (string= dic "deutsch") "english" "deutsch")))
                (ispell-change-dictionary change)
                (message "Dictionary switched from %s to %s" dic change)))))

(use-package cc-mode
  :config (progn
            (define-key c++-mode-map (kbd "C-c x") 'recompile)))

;; #############################################################################
;; ################################# KEY BINDINGS ##############################
;; #############################################################################
;; Map escape to cancel (like C-g)...
(define-key isearch-mode-map [escape] 'isearch-abort)   ;; isearch
(global-set-key [escape] 'keyboard-escape-quit)         ;; everywhere else

;; tmux-like spliting
(global-set-key (kbd "C-x %") 'split-window-right)
(global-set-key (kbd "C-x \"") 'split-window-below)
(global-set-key (kbd "C-x t") 'make-frame-command)

;; revert buffer
(global-set-key [f5]
                (lambda ()
                  (interactive)
                  (revert-buffer nil t)
                  (message "Buffer reverted")))

;; Unconditionally kill unmodified buffers.
(global-set-key (kbd "C-x k") 'kill-this-buffer-if-not-modified)

;; #############################################################################
;; ################################# FUNCTIONS #################################
;; #############################################################################

;; indent whole buffer
(defun onze-indent-whole-buffer ()
  "Delete trailing whitespace, indent and untabify whole buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(defun onze-create-new-blog-post (name)
  "Create a new blog post in my blog directory.  NAME ist the name of the post, which is a suffix for the filename."
  (interactive "sEnter name of new blog-post: ")
  (let ((filename (concat (concat (format-time-string "%Y-%m-%d-") name) ".md")))
    (find-file (concat "/mnt/hdd/Blog/onc.github.io/_posts/" filename))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun onze-rename-file-and-buffer (new-name)
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

(defun kill-this-buffer-if-not-modified ()
  "Kill current buffer, even if it has been modified."
  (interactive)
  (kill-buffer (current-buffer)))

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

;; #############################################################################
;; ################################# FUNCTIONS PATRICK #########################
;; #############################################################################

(defun noumlaut-to-umlaut ()
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
  (toggle-case-fold-search))

(defun my-where-is (definition count &optional length)
  "DEFINITION is the name of the function.
Return COUNT key sequences that invoke the command DEFINITTION as a string.
If COUNT is 0 then all key sequences will be returned.
The optional LENGTH argument limits the returned string to LENGTH number of
chars.

For convenience there are also the functions `my-where-is-first'
and `my-where-is-all'.

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

(defun my-where-is-first (definition &optional length)
  "DEFINITION is the name of the function.
Return the first key sequence for DEFINITION as a string.
The optional LENGTH argument limits the returned string to LENGTH number of
chars.

This is a convenience function for `my-where-is'."
  (interactive)
  (my-where-is definition 1 length))

(defun my-where-is-all (definition &optional length)
  "DEFINITION is the name of the function.
Return all key sequence for DEFINITION as a string.
The optional LENGTH argument limits the returned string to LENGTH number of
chars.

This is a convenience function for `my-where-is'."
  (interactive)
  (my-where-is definition 0 length))

;; #############################################################################
;; ################################# EMACS #####################################
;; #############################################################################

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (base16-monokai-dark)))
 '(custom-safe-themes
   (quote
    ("4f034ec900e09a0ecfe7025d5f8c02bcf4448d131c5d0ea962b8b74b2ce4d4ea" "3994497138fa263aadde66e0f869e2c2426afc342bf2b06da4c2431473fde61c" "7e3b0c38791f8ee1d067bf7b84cd916ffea6392b428db4b375b47504a89edc6c" default)))
 '(desktop-load-locked-desktop t)
 '(fringe-mode (quote (1 . 1)) nil (fringe))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Make FlyCheck happy...
;;; init.el ends here
