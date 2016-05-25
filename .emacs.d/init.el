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
;; disable menu-bar
(menu-bar-mode -1)

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
;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline-9" ))
;; (set-face-attribute 'default t :font "DejaVu Sans Mono for Powerline-9")

(add-to-list 'default-frame-alist '(font . "Source Code Pro-9:weight=medium"))
(set-face-attribute 'default t :font "Source Code Pro-9:weight=medium")

;; (add-to-list 'default-frame-alist '(font . "Monoid-10"))
;; (set-face-attribute 'default t :font "Monoid-10")

;; fucking use spaces emacs
(setq-default tab-width 4 indent-tabs-mode nil)

;; set indention in c files
(setq-default c-basic-offset 4)

;; Write backup files to own directory
;; (setq backup-directory-alist
;;       `(("." . ,(expand-file-name
;;                  (concat user-emacs-directory "backups")))))
;; Write auto save files to own directory
;; http://stackoverflow.com/a/2020954/29618
;; (defvar autosave-dir (expand-file-name (concat user-emacs-directory "autosaves/")))
;; (setq auto-save-list-file-prefix autosave-dir)
;; (setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; from http://emacswiki.org/emacs/AutoSave
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq create-lockfiles nil)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

;; Don't make me type 'yes' or 'no', y/n will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; dont ask to follow symlink in git
(setq vc-follow-symlinks t)

;; redraw completly before continue to avoid lagging
(setq redisplay-dont-pause t)

;; Nonzero means echo unfinished commands after this many seconds of pause.
;; The value may be integer or floating point.
(setq echo-keystrokes 0.02)

;; Auto refresh buffers
(global-auto-revert-mode t)

;; Make sure UTF-8 is used
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; Add .h and .cc files to c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))

;; Open zsh-files in shell-mode
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; email mode
(add-to-list 'auto-mode-alist '("\\.mail\\'" . mail-mode))
(add-to-list 'auto-mode-alist '("\\.eml\\'" . mail-mode))
(add-hook 'mail-mode-hook
          (lambda ()
            (setq fill-column 80)
            (auto-fill-mode t)))

(require 'mu4e)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

(electric-pair-mode)

;; check (on save) whether the file edited contains a shebang, if yes, make it executable
;; from http://mbork.pl/2015-01-10_A_few_random_Emacs_tips
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; #############################################################################
;; ################################# PACKAGES ##################################
;; #############################################################################

;; load use-package module
(require-package 'use-package)
(require 'use-package)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

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
            (evil-set-initial-state 'pdf-view-mode 'emacs)
            (evil-set-initial-state 'pdf-annot-list-mode 'emacs)
            (evil-set-initial-state 'calendar-mode 'emacs)

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
                          "p" 'helm-projectile
                          "ci" 'evilnc-comment-or-uncomment-lines
                          "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
                          "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
                          "cc" 'evilnc-copy-and-comment-lines
                          "cp" 'evilnc-comment-or-uncomment-paragraphs
                          "cr" 'comment-or-uncomment-region
                          "cv" 'evilnc-toggle-invert-comment-line-by-line
                          "a" 'align-regexp)))

            (use-package evil-search-highlight-persist
              :ensure t
              :config (progn
                        (global-evil-search-highlight-persist)))

            (use-package evil-nerd-commenter
              :ensure t)

            ;; PACKAGE: EVIL-NUMBERS
            (use-package evil-numbers
              :ensure t
              ;; :bind (:map evil-normal-state-map
              :bind (("<M-up>" . evil-numbers/inc-at-pt)
                     ("<M-down>" . evil-numbers/dec-at-pt)))

            (use-package evil-mu4e
              :ensure t)

            ;; PACKAGE: EVIL-SURROUND
            (use-package evil-surround
              :ensure t
              :config (progn
                        (global-evil-surround-mode 1)))))

;; (use-package smartparens
;;   :ensure t
;;   :config (progn
;;             (smartparens-global-mode t)
;;             (show-smartparens-global-mode)))

;; (use-package autopair
;;   :ensure t
;;   :config (progn
;;             (autopair-global-mode)))

(use-package mu4e
  :ensure nil
  :bind (([f7] . mu4e))
  :config
  ;; default
  (setq mu4e-maildir (expand-file-name "~/Mail"))
  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "offlineimap")

  (setq mu4e-drafts-folder "/UniMail/Drafts")
  (setq mu4e-sent-folder   "/UniMail/Sent")
  (setq mu4e-trash-folder  "/UniMail/Trash")

  ;; fetch mails every 3 min
  (setq mu4e-update-interval (* 3 60))

  ;; Don't keep message buffers around.
  (setq message-kill-buffer-on-exit t)

  ;; http://www.djcbsoftware.nl/code/mu/mu4e/Displaying-rich_002dtext-messages.html
  (setq mu4e-html2text-command "html2text | grep -v '&nbsp_place_holder;'")

  (setq mu4e-show-images t)

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (setq mu4e-split-view 'vertical)
  (setq mu4e-headers-visible-columns 120)

  (setq mu4e-maildir-shortcuts
        '(("/UniMail/INBOX"                             . ?u)
          ("/GoogleMail/INBOX"                          . ?g)
          ("/Onze-io/OnzeMail/INBOX"                    . ?o)
          ("/Onze-io/AnyoneMail/INBOX"                  . ?a)
          ("/UniMail/Uni-Mails"                         . ?w)
          ("/UniMail/Uni-Mails.jugendhackt.jugendhackt" . ?j)))

  (add-to-list 'mu4e-bookmarks '("/UniMail/Uni-Mails.jugendhackt.jugendhackt"   "Jugendhackt" ?j) t)
  (add-to-list 'mu4e-bookmarks '("/UniMail/INBOX OR /GoogleMail/INBOX OR /Onze-io/OnzeMail/INBOX OR /Onze-io/AnyoneMail/INBOX" "Combined Inbox" ?i) t)

  (setq mu4e-attachment-dir  "~/Downloads/Mail")

  ;; spell check
  (add-hook 'mu4e-compose-mode-hook
            (defun my-do-compose-stuff ()
              "My settings for message composition."
              (set-fill-column 72)
              (flyspell-mode)))

  ;; adjust columns of headers view
  (setq mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 6)
          (:mailing-list . 12)
          (:from . 22)
          (:subject)))

  (setq mu4e-use-fancy-chars t
        mu4e-headers-first-child-prefix  '("\\" . "┗▶")
        mu4e-headers-unread-mark '("u" . "❌")
        mu4e-headers-unseed-mark '("u" . "❌")
        mu4e-headers-replied-mark '("R" . "←")
        mu4e-headers-seen-mark '("S" . "✓")
        mu4e-headers-attach-mark '("a" . "↓")
        mu4e-headers-signed-mark '("s" . "™")
        mu4e-headers-encrypted-mark '("x" . "🔒")
        mu4e-headers-flagged-mark '("F" . "♥"))

  ;; something about ourselves
  (setq
   user-mail-address "christian.van-onzenoodt@uni-ulm.de"
   user-full-name  "Christian van Onzenoodt")

  ;; Silly mu4e only shows names in From: by default. Of course we also
  ;; want the addresses.
  (setq mu4e-view-show-addresses t)

  (setq mu4e-headers-time-format "%H:%M")
  (setq mu4e-date-format-long "%d/%m/%Y %H:%M")
  (setq mu4e-headers-date-format "%d/%m/%y")

  (use-package mu4e-alert
    :ensure t
    :config

    (mu4e-alert-set-default-style 'libnotify)
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))
  )

(use-package smtpmail
  :ensure nil
  :config
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials
        '(("mail.uni-ulm.de" 587 nil nil))
        smtpmail-auth-credentials (expand-file-name "~/.authinfo")
        smtpmail-default-smtp-server "mail.uni-ulm.de"
        smtpmail-smtp-server "mail.uni-ulm.de"
        smtpmail-smtp-service 587
        smtpmail-debug-info t))

(use-package pdf-tools
  :config
  (pdf-tools-install)

  (add-hook 'pdf-view-mode-hook 'pdf-view-fit-page-to-window)

  (define-key pdf-view-mode-map (kbd "C-w l") 'evil-window-right)
  (define-key pdf-view-mode-map (kbd "C-w h") 'evil-window-left)
  (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "l") 'pdf-view-next-page-command)
  (define-key pdf-view-mode-map (kbd "h") 'pdf-view-previous-page-command))

(use-package deft
  :ensure t
  :bind (([f6] . deft))
  :config
  (setq deft-directory "~/Dropbox/Notes")
  (setq deft-extensions '("md" "org" "txt"))
  (setq deft-default-extension "md")
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-recursive t)
  (setq deft-auto-save-interval 3.0))

(use-package calendar
  :config
  (define-key calendar-mode-map (kbd "j") 'calendar-forward-week)
  (define-key calendar-mode-map (kbd "k") 'calendar-backward-week)
  (define-key calendar-mode-map (kbd "h") 'calendar-backward-day)
  (define-key calendar-mode-map (kbd "l") 'calendar-forward-day))

(use-package org
  :config (progn
            (setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

            (use-package org-bullets
              :ensure t
              :config (progn
                        (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
                        (setq org-bullets-bullet-list '("●" "◼" "▶" "♦"))))))

(use-package ox-latex
  :defer t
  :ensure nil
  :config
  (setq org-latex-packages-alist
        (quote (("" "color" t) ("" "minted" t) ("" "parskip" t))))

  ;; XeLaTeX customisations
  ;; remove "inputenc" from default packages as it clashes with xelatex
  (setf org-latex-default-packages-alist
        (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))

  (setq org-latex-pdf-process
        '("xelatex --shell-escape -interaction nonstopmode -output-directory %o %f"
          "biber $(basename %b)"
          "xelatex --shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex --shell-escape -interaction nonstopmode -output-directory %o %f"))

  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (setq org-latex-listings 'minted)

  ;; add emacs lisp support for minted
  (setq org-latex-custom-lang-environments '((emacs-lisp "common-lisp")))
  (add-to-list 'org-latex-minted-langs '(elisp "common-lisp"))

  (add-to-list 'org-latex-classes
               '("thesis" "\\documentclass{thesis}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

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
            (global-company-mode)
            ;; no delay no autocomplete
            (setq company-idle-delay 0)
            (setq company-minimum-prefix-length 2)

            (define-key company-active-map (kbd "M-j") 'company-select-next)
            (define-key company-active-map (kbd "M-k") 'company-select-previous)

            (use-package company-cmake
              :ensure t)
            (use-package company-emoji
              :ensure t)
            (add-to-list 'company-backends 'company-emoji)))

;; emoji font
(set-fontset-font
 t 'symbol
 (font-spec :family "Symbola") nil 'prepend)

(use-package elpy
  :ensure t
  :init
  (setq elpy-rpc-backend "jedi")
  :config
  (setq elpy-modules (delq 'elpy-module-company elpy-modules))
  (elpy-enable)

  (add-hook 'python-mode-hook
            (lambda ()
              (company-mode)
              (add-to-list 'company-backends
                           (company-mode/backend-with-yas 'elpy-company-backend))))
  (elpy-use-cpython))

(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (push 'company-robe company-backends))

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
            (global-set-key (kbd "C-x C-f") 'helm-find-files)

            (use-package helm-ag
              :ensure t)

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
  _i_: indent buffer %(my-where-is-first 'onze-indent-whole-buffer)       _n_: create new blog post    _p_: switch project %(my-where-is-first 'helm-projectile-switch-project)
  _r_: rename buffer and file                                _s_: start clock
  _v_: toggle transparency                                   _f_: stop/finish clock
                                                           _t_: create report-table
"
                              ("i" onze-indent-whole-buffer       nil)
                              ("r" onze-rename-file-and-buffer    nil)
                              ("n" onze-create-new-blog-post      nil)
                              ("p" helm-projectile-switch-project nil)
                              ("s" org-clock-in                   nil)
                              ("f" org-clock-out                  nil)
                              ("t" org-clock-report               nil)
                              ("v" toggle-transparency            nil)
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

;; (use-package powerline
;;   :ensure t
;;   :config (progn
;;             (powerline-center-evil-theme)))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

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
  :ensure nil
  :load-path "/home/onze/.emacs.d/git-package/magit/lisp"
  :bind (("M-s" . magit-status))
  :config (progn

            (with-eval-after-load 'info
              (info-initialize)
              (add-to-list 'Info-directory-list
                           "~/.emacs.d/git-package/magit/Documentation/"))

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
            (eval-after-load "company" '(diminish 'company-mode))

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

(use-package nlinum
  :ensure t)

;; (use-package google-c-style
;;   :ensure t
;;   :config (progn
;;             (add-hook 'c-mode-common-hook 'google-set-c-style)))

(use-package fill-column-indicator
  :ensure t
  :config (progn
            (setq fci-rule-width 1)
            (setq fci-rule-color "gray71")
            (setq-default fill-column 80)
            (add-hook 'c-mode-common-hook 'fci-mode)
            (add-hook 'mail-mode-hook 'fci-mode)
            (add-hook 'js2-mode-hook 'fci-mode)))


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

;; (use-package flycheck-google-cpplint
;;   :ensure t
;;   :config (progn
;;             (custom-set-variables
;;              '(flycheck-c/c++-googlelint-executable "/usr/local/bin/cpplint.py"))))

(use-package flycheck
  :ensure t
  :config (progn
            (global-flycheck-mode)
            (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))
            ;; (flycheck-add-next-checker 'c/c++-clang 'c/c++-googlelint 'append)
            ))

(use-package wcheck-mode
  :ensure t
  :config
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

  (wcheck-activate))

;; END WCHECK

(use-package flyspell
  :disabled t
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

(use-package unbound
  :ensure t)

(use-package cc-mode
  :config
  (progn

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

    (defun onze-cmake-build ()
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
        (if (zerop (call-process "/bin/bash" nil t nil "-c" (concat (concat "find " (dirvars-find-upwards "build")) " -executable -type f | grep -v CMake")))
            (buffer-substring (point-min) (1- (point-max)))
          nil)))

    (defun onze-cmake-run ()
      (interactive)
      (call-process (my-executable-path)))

    (define-key c++-mode-map (kbd "C-c c") 'onze-cmake-build)
    (define-key c++-mode-map (kbd "C-c x") 'onze-cmake-run)
    ))

(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode))

(use-package groovy-mode
  :ensure t
  :mode (("\.groovy$" . groovy-mode)
         ("\.gradle$" . groovy-mode))
  :config
  (use-package gradle-mode
    :ensure t))

(use-package solarized-theme
  :ensure t)

(use-package material-theme
  :ensure t)

;; PACKAGE: JS2
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq-default js2-global-externs '("exports" "module" "require" "setTimeout" "THREE"))
  (setq-default js2-basic-offset 2))

;; PACKAGE: TERN
(use-package tern
  :ensure t
  :init
  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  :config
  (use-package company-tern
    :ensure t
    :init
    (add-hook 'js-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends)
                     '((company-tern company-dabbrev-code company-yasnippet)))))
    (add-hook 'js2-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends)
                     '((company-tern company-dabbrev-code company-yasnippet)))))))

(use-package adoc-mode
  :ensure t)

(use-package json-mode
  :ensure t
  :config
  (setq js-indent-level 2))

(use-package rubocop
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package jsx-mode
  :ensure t)

(use-package golden-ratio
  :commands (golden-ratio-mode)
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode nil)

  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down
                  select-window-1
                  select-window-2
                  select-window-3
                  select-window-4
                  select-window-5)))
  ;; Disable for specific modes
  (setq golden-ratio-exclude-modes '("ediff-mode"
                                     "eshell-mode"
                                     "dired-mode"
                                     "magit-status-mode"
                                     "magit-log-mode"
                                     "magit-mode"
                                     "magit-key-mode"
                                     "magit-reflog-mode"
                                     "helm-mode"
                                     "neotree-mode"
                                     "mu4e-headers-mode"
                                     "mu4e-view-mode"))

  ;; when the display width of the focused window resized by
  ;; golden ratio is larger than 160 characters, opening any
  ;; commands which call to pop-up-window will cause Emacs
  ;; to create extra window instead of jump to an already
  ;; existing window. TO prevent that, just set the variable
  ;; split-width-threshold to nil
  (setq split-width-threshold nil))

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

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)" 1 font-lock-warning-face t)))))

(defun beautify-json ()
  "Beatify json in buffer."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(defun toggle-transparency ()
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


;; Changing font size
(defun sanityinc/font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun sanityinc/increment-default-font-height (delta)
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
        (set-frame-font (sanityinc/font-name-replace-size
                         (face-font 'default)
                         new-point-height)
                        t)))
    (set-face-attribute 'default nil :height new-height)
    (message "default font size is now %d" new-point-height)))

(defun sanityinc/increase-default-font-height ()
  (interactive)
  (sanityinc/increment-default-font-height 10))

(defun sanityinc/decrease-default-font-height ()
  (interactive)
  (sanityinc/increment-default-font-height -10))

(global-set-key (kbd "s-+") 'sanityinc/increase-default-font-height)
(global-set-key (kbd "s--") 'sanityinc/decrease-default-font-height)

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
  (query-replace "sz" "ß" nil (point-min) (point-max))
  (toggle-case-fold-search))

(defun onze-insert-alphabets-az (&optional useUppercase-p)
  "Insert letters a to z vertically.
If `universal-argument' is called first, use CAPITAL letters.
Note: this command is similar to `rectangle-number-lines', starting at 65 or 97, and with a format of 「%c」."
  (interactive "P")
  (let ((startChar (if useUppercase-p 65 97 )))
    (dotimes (ii 26 )
      (insert (format "%c\n" (+ startChar ii)))
      )))

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
 '(ansi-color-names-vector
   ["#272822" "#f92672" "#a6e22e" "#f4bf75" "#66d9ef" "#ae81ff" "#66d9ef" "#f8f8f2"])
 '(ansi-term-color-vector
   [unspecified "#272822" "#f92672" "#a6e22e" "#f4bf75" "#66d9ef" "#ae81ff" "#66d9ef" "#f8f8f2"])
 '(custom-enabled-themes (quote (base16-onze-google-dark)))
 '(custom-safe-themes
   (quote
    ("113ae6902d98261317b5507e55ac6e7758af81fc4660c34130490252640224a2" "3ae2686d180c6ea92ccfe418d881b7e984be62639dea15f330d31904cd228f10" "f7799c0c6fc9e8b02853f36a6a7e2b7ce57e51f89d52ad1e25ab302c80e9dd19" "4a171c5c7386e30d16f1ba552fd652dc1c7d79c3b216ef2e9296a7025482ce58" "3270c6e92aa1dd60f8317e7913a658173ad9c953657792cd6f805bd4dcf4e476" "d3e906a019c1b18c6f091eaf05d441be8991bd57e7535a7f0d52f92f7770f37c" "d085be39ec443f01b3a581766ecc292921c76262f25268ff0786310db7351edc" "7d2f37e0de6f872c3d90801b0b8f27d03165876b82c685532d7adafaa08cc30f" "641b002f11edd63d448f1fa8b19e2b318cd5a5edfd6898de025a6754da77c02a" "ab12e149a516519a4794245d65242a13f28ffb4b86b20b9e98551a7fb1e02dd2" "c2c86325e71c2f3b70838e91dfee8f80471021824fe896106922595265cfc93e" "d362b08d053513c09741093741abff117ef807b664e9d85c2c520143e8551022" "c98ce4ee9d83b21991a1d393030cee776458e50c331be8f2911c9cf6cb6bf719" "c6b26b882659f842363a235d8ba376998811d2b73336fc48ad2e10841030cdab" "0240d45644b370b0518e8407f5990a243c769fb0150a7e74297e6f7052a04a72" "9c961e80e46c08fb6d0f71cc2654f302ef214fb69eccb390917a82a42a275f27" "ac31699d6255ef22b646ebc80e69f518929b57f2f0855a814170d060db78a5f1" "d92db4e2b227ce1506a9b95160f6ae594b9844cc685f7c67fb109f4fd6bb0388" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "e3a3b7d7fe89b5d57d40bc825ca2324875a6f37bd63da66f2a6fc68cc8b2ee95" "7b3124eec8107900b9c03eb7370e3ef6a7a86d896361c8e85bbbe4bbc952124b" "5be9916bc54fd57fcb5a3d9426ed5aec0389a4bd4ed75b497a0d4cf2bbde7a4b" "3a6d8378f18a5ba2a0c88ddcad2500675a755d056b3343b13b36911453e96c34" "4f034ec900e09a0ecfe7025d5f8c02bcf4448d131c5d0ea962b8b74b2ce4d4ea" "3994497138fa263aadde66e0f869e2c2426afc342bf2b06da4c2431473fde61c" "7e3b0c38791f8ee1d067bf7b84cd916ffea6392b428db4b375b47504a89edc6c" default)))
 '(desktop-load-locked-desktop t)
 '(fringe-mode (quote (1 . 1)) nil (fringe))
 '(magit-diff-refine-hunk t)
 '(paradox-github-token t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-search-highlight-persist-highlight-face ((t (:background "magenta" :foreground "white")))))

;; Make FlyCheck happy...
;;; init.el ends here
