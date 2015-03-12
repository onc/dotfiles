;;; package --- Summary

;; -*- coding: utf-8; lexical-binding: t -*

;;; Commentary:


;;; Code:

;; #############################################################################
;; ################################# SOURCES ###################################
;; #############################################################################

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

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
(desktop-save-mode 1)

(recentf-mode t)
(setq recentf-max-saved-items 1000)

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

;; #############################################################################
;; ################################# PACKAGES ##################################
;; #############################################################################

;; load use-package module
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
  :config
  (progn
    (smartparens-global-mode t)
    (show-smartparens-global-mode)))

(use-package smooth-scrolling
  :ensure t
  :config
  (progn
    (setq scroll-margin 5 scroll-conservatively 9999 scroll-step 1)))

(use-package undo-tree
  :ensure t
  :config
  (progn
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist
          `(("." . ,(concat user-emacs-directory "undo"))))
    (global-undo-tree-mode)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package company
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    ;; no delay no autocomplete
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 2)))

(use-package neotree
  :ensure t
  :config
  (progn
    (global-set-key [f8] 'neotree-toggle)
    (evil-set-initial-state 'neotree-mode 'emacs)
    (setq neo-theme 'arrow)

    (define-key neotree-mode-map (kbd "j") 'next-line)
    (define-key neotree-mode-map (kbd "k") 'previous-line)
    (define-key neotree-mode-map (kbd "s") 'neotree-enter-vertical-split)
    (define-key neotree-mode-map (kbd "i") 'neotree-enter-horizontal-split)
    (define-key neotree-mode-map (kbd "C-w l") 'other-window)
    ;; (define-key neotree-mode-map (kbd "<escape>") 'neotree-hide)
    ;; If this variable is non-nil then it's not possible to
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
  :config
  (progn
    (helm-mode 1)
    (setq helm-buffer-details-flag nil)
    ;; fuzzy matching
    (setq helm-apropos-fuzzy-match t)
    (setq helm-ff-file-name-history-use-recentf t)
    ;; rebind tab to do persistens action
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    ;; make tab work in terminal
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    ;; list actions using C-j
    (define-key helm-map (kbd "C-j") 'helm-select-action)
    (global-set-key (kbd "C-h C-h") 'helm-apropos)
    ;; use helm for meta x
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x b") 'helm-mini)))

(use-package git-gutter
  :ensure t
  :config
  (progn
    (global-git-gutter-mode +1)))

(use-package ycmd
  :ensure t
  :config
  (progn
    (add-hook 'c++-mode-hook 'ycmd-mode)
    (set-variable 'ycmd-server-command '("python2" "/home/onze/Applications/ycmd/ycmd"))
    (set-variable 'ycmd-global-config "/home/onze/Applications/ycmd/cpp/ycm/.ycm_extra_conf.py")))

(use-package company-ycmd
  :ensure t
  :config
  (progn
    (company-ycmd-setup)))

(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-global-mode)))

(use-package helm-projectile
  :ensure t
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))

(use-package powerline
  :ensure t
  :config
  (progn
    (powerline-default-theme)))

(use-package yasnippet
  :ensure t
  :config
  (progn
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
