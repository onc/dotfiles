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

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; start emacs in server mode
(load "server")
(unless (server-running-p) (server-start))

;; email mode
(add-to-list 'auto-mode-alist '("\\.mail\\'" . mail-mode))
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
  :config
  (progn
    (evil-mode 1)
    (setq evil-move-cursor-back nil)))

(use-package evil-leader
  :ensure t
  :config
  (progn
    (global-evil-leader-mode)

    (evil-leader/set-key
      "n" 'neotree-toggle
      "f" 'pamo-indent-whole-buffer
      "init" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
      "b" 'helm-mini
      "o" 'find-file
      "e" 'eval-defun)))

(use-package smartparens
  :ensure t
  :config (progn
            (smartparens-global-mode t)
            (show-smartparens-global-mode)))

(use-package smooth-scrolling
  :ensure t
  :config (progn
            (setq scroll-margin 5 scroll-conservatively 9999 scroll-step 1)))

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
  :ensure t
  :load-path "/home/onze/.emacs.d/git-package/magit"
  :pin manual)

;; #############################################################################
;; ################################# KEY BINDINGS ##############################
;; #############################################################################

;; save on double escape and space for command mode
(defun add-vim-bindings()
  (define-key evil-normal-state-local-map (kbd "<escape> <escape>") 'save-buffer)
  (define-key evil-normal-state-local-map (kbd "<SPC>") 'evil-ex))

(add-hook 'evil-normal-state-entry-hook 'add-vim-bindings)

;; #############################################################################
;; ################################# FUNCTIONS #################################
;; #############################################################################

;; indent whole buffer
(defun pamo-indent-whole-buffer ()
  "Delete trailing whitespace, indent and untabify whole buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

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
    ("4f034ec900e09a0ecfe7025d5f8c02bcf4448d131c5d0ea962b8b74b2ce4d4ea" "3994497138fa263aadde66e0f869e2c2426afc342bf2b06da4c2431473fde61c" "7e3b0c38791f8ee1d067bf7b84cd916ffea6392b428db4b375b47504a89edc6c" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Make FlyCheck happy...
;;; init.el ends here
