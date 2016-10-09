;;; package --- Summary

;; -*- coding: utf-8; lexical-binding: t -*

;;; Commentary:

;;; Code:

;; #############################################################################
;; ################################# SOURCES ###################################
;; #############################################################################

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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

;; GC tweaking according to
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold most-positive-fixnum)

(defun my-minibuffer-setup-hook ()
  "Crank up the Garbage Collection threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Put Garbage Collection back to default value."
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)


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

;; Open zsh-files in shell-mode
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; email mode
(add-to-list 'auto-mode-alist '("\\.mail\\'" . mail-mode))
(add-to-list 'auto-mode-alist '("\\.eml\\'" . mail-mode))
(add-hook 'mail-mode-hook
          (lambda ()
            (setq fill-column 80)
            (auto-fill-mode t)))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

(electric-pair-mode)

;; check (on save) whether the file edited contains a shebang, if yes, make it executable
;; from http://mbork.pl/2015-01-10_A_few_random_Emacs_tips
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq scroll-preserve-screen-position 'always)

;; #############################################################################
;; ################################# PACKAGES ##################################
;; #############################################################################

;; load use-package module
(require-package 'use-package)
(require 'use-package)
(setq use-package-verbose t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; make emacs usable
(use-package evil
  :ensure t
  :config
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

  (define-key evil-normal-state-local-map (kbd "<DEL>") 'evil-search-highlight-persist-remove-all)

  (add-hook 'evil-normal-state-entry-hook 'add-vim-bindings)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)

    (evil-leader/set-key
      "n" 'neotree-toggle
      "f" 'onze-indent-whole-buffer
      "init" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
      "b" 'helm-mini
      "o" 'find-file
      "e" 'eval-defun
      "d" 'dictcc
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
      "a" 'align-regexp)

    (evil-leader/set-key-for-mode
      'c++-mode "f" 'clang-format-buffer)

    (evil-leader/set-key-for-mode
      'rust-mode "f" 'cargo-process-fmt))

  (use-package evil-search-highlight-persist
    :ensure t
    :config
    (global-evil-search-highlight-persist))

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
    :config
    (global-evil-surround-mode 1)))

(use-package mu4e
  :bind (([f7] . mu4e))
  :commands mu4e
  :defer 60
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
          ("/GoogleSpamMail/INBOX"                      . ?s)
          ("/Onze-io/OnzeMail/INBOX"                    . ?o)
          ("/Onze-io/AnyoneMail/INBOX"                  . ?a)
          ("/UniMail/Uni-Mails"                         . ?w)
          ("/UniMail/Uni-Mails.jugendhackt.jugendhackt" . ?j)))

  (add-to-list 'mu4e-bookmarks '("/UniMail/Uni-Mails.jugendhackt.jugendhackt"   "Jugendhackt" ?j) t)
  (add-to-list 'mu4e-bookmarks '("/UniMail/INBOX OR /GoogleMail/INBOX OR /Onze-io/OnzeMail/INBOX OR /Onze-io/AnyoneMail/INBOX OR /GoogleSpamMail/INBOX" "Combined Inbox" ?i) t)

  (setq mu4e-attachment-dir  "~/Downloads/Mail")

  ;; spell check
  (add-hook 'mu4e-compose-mode-hook
            (defun my-do-compose-stuff ()
              "My settings for message composition."
              (set-fill-column 72)
              (flyspell-mode)))

  ;; custom header, which only shows the root mail-dir like /Uni or /Google for all my mailaccounts.
  ;; the regex replaces all symbols between the LAST forward-slash and the end string.
  ;; this works, because all imap-subfolders are seperated by dots instead of slashes.
  (add-to-list 'mu4e-header-info-custom
               '(:maildir-root .
                               (:name "Root-Folder of the maildir"
                                      :shortname "MailRoot"
                                      :help "Root-Folder of the maildir"
                                      :function (lambda (msg)
                                                  (replace-regexp-in-string "\/[a-zA-Z0-9-. ]*$" "" (concat (mu4e-message-field msg :maildir) ""))))))

  ;; adjust columns of headers view
  (setq mu4e-headers-fields
        '((:human-date . 10)
          (:flags . 8)
          (:mailing-list . 15)
          (:maildir-root . 12)
          (:from . 26)
          (:subject)))

  (define-key mu4e-main-mode-map (kbd "s") 'helm-mu)
  (define-key mu4e-headers-mode-map (kbd "C-<return>") 'mu4e-headers-mark-for-something)
  (define-key mu4e-headers-mode-map (kbd "C-r") 'mu4e-mark-resolve-deferred-marks)

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
    (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)))

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
  :commands (pdf-tools-install)
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-fit-page-to-window)

  (define-key pdf-view-mode-map (kbd "C-w l") 'evil-window-right)
  (define-key pdf-view-mode-map (kbd "C-w h") 'evil-window-left)
  (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "l") 'pdf-view-next-page-command)
  (define-key pdf-view-mode-map (kbd "h") 'pdf-view-previous-page-command))

(use-package deft
  :bind (([f6] . deft))
  :config
  (setq deft-directory "~/Dropbox/Notes")
  (setq deft-extensions '("md" "org" "txt"))
  (setq deft-default-extension "org")
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
  :mode (("\\.org\\'" . org-mode)
         ("\\.org_archive\\'" . org-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

  (define-key org-mode-map (kbd "M-j") 'org-forward-heading-same-level)
  (define-key org-mode-map (kbd "M-k") 'org-backward-heading-same-level)

  (setq org-publish-project-alist
        '(("blog"
           :base-directory "/mnt/hdd/Blog/org-test/"
           :html-extension "html"
           :base-extension "org"
           :publishing-directory "/mnt/hdd/Blog/org-test/public_html/"
           :publishing-function (org-html-publish-to-html)
           :html-preamble nil
           :html-postamble nil)))


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
  (setq org-src-fontify-natively t)

  (setq org-agenda-files (quote ("~/todo.org")))

  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    (setq org-bullets-bullet-list '("●" "◼" "▶" "♦")))

  (use-package org-mu4e
    :config
    ;;store link to message if in header view, not to header query
    (setq org-mu4e-link-query-in-headers-mode nil)
    (setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/todo.org" "Tasks")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"))))

  (use-package org-vcard
    :ensure t))

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

(use-package rust-mode
  :mode("\\.rs\\'" . rust-mode)
  :config

  (use-package cargo
    :ensure t
    :config
    (add-hook 'rust-mode-hook 'cargo-minor-mode)
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Cargo " (or "Run" "Build" "Fmt") "*" eos)
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   ;; (display-buffer-reuse-window display-buffer-same-window)
                   (reuseable-frames . visible)
                   (side             . bottom)
                   (window-height    . 0.2))))

  (use-package racer
    :config
    (setq racer-cmd "/home/onze/.cargo/bin/racer")
    (setq racer-rust-src-path "/usr/src/rust/src")
    (add-hook 'rust-mode-hook 'racer-mode)
    (add-hook 'racer-mode-hook 'eldoc-mode)))

(use-package ox-latex
  :defer t
  :ensure nil
  :config

  ;; XeLaTeX customisations
  ;; remove "inputenc" from default packages as it clashes with xelatex
  (setf org-latex-default-packages-alist
        (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))

  (setq org-latex-pdf-process
        '("xelatex --shell-escape -interaction nonstopmode -output-directory %o %f"
          "biber $(basename %b)"
          "xelatex --shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex --shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; you have to install pygmentize to use minted
  (setq org-latex-packages-alist
        (quote (("" "color" t) ("" "minted" t) ("" "parskip" t))))
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("linenos")
          ("samepage")))

  ;; add emacs lisp support for minted
  (setq org-latex-custom-lang-environments '((emacs-lisp "common-lisp")))
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


(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        `(("." . ,(concat user-emacs-directory "undo"))))
  (global-undo-tree-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package yasnippet
  :init
  (setq yas-verbosity 0)
  :defer 30
  :config
  (yas-global-mode 1))

(use-package company
  :defer 10
  :diminish company-mode
  :config
  (global-company-mode)
  ;; no delay no autocomplete
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 20)

  (define-key company-active-map (kbd "M-j") 'company-select-next)
  (define-key company-active-map (kbd "M-k") 'company-select-previous)

  ;; enable yasnippet everywhere
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  ;; sub-packages
  (use-package company-cmake
    :ensure t
    :config
    (add-to-list 'company-backends 'company-cmake))

  (use-package company-racer
    :ensure t
    :config
    (add-to-list 'company-backends 'company-racer))

  (use-package company-inf-ruby
    :ensure t
    :config
    (add-to-list 'company-backends 'company-inf-ruby))

  (use-package company-restclient
    :ensure t
    :config
    (add-to-list 'company-backends 'company-restclient)))

;; emoji font
(set-fontset-font
 t 'symbol
 (font-spec :family "Symbola") nil 'prepend)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config

  (use-package elpy
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
    (elpy-use-cpython)))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :config

  (define-key ruby-mode-map (kbd "C-c r") 'onze-run-current-file)

  (use-package robe
    :ensure t
    :config
    (add-hook 'ruby-mode-hook 'robe-mode)
    (add-to-list 'company-backends (company-mode/backend-with-yas 'company-robe)))

  (use-package inf-ruby
    :ensure t
    :config
    (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
    (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)))

(use-package neotree
  :bind(([f8] . neotree-toggle))
  :config
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
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind(("C-h C-h" . helm-apropos)
        ("M-x"     . helm-M-x)
        ("C-x b"   . helm-mini)
        ("C-x C-f" . helm-find-files))
  :config
  (require 'helm-config)
  (helm-mode +1)

  ;; rebind tab to do persistens action
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make tab work in terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  ;; list actions using C-j
  (define-key helm-map (kbd "C-j") 'helm-select-action)

  (define-key helm-map (kbd "M-j") 'helm-next-line)
  (define-key helm-map (kbd "M-k") 'helm-previous-line)

  ;; fuzzy matching
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)

  (setq helm-ff-file-name-history-use-recentf t)

  (setq helm-reuse-last-window-split-state t)
  ;; Don't use full width of the frame
  (setq helm-split-window-in-side-p t)
  (helm-autoresize-mode t)

  ;; Use ack instead of grep
  (when (executable-find "ack")
    (setq helm-grep-default-command "ack -Hn --no-group --no-color %p %f"
          helm-grep-default-recurse-command "ack -H --no-group --no-color %p %f"))

  ;; Even better, use ag if it's available
  (when (executable-find "ag")
    (setq helm-grep-default-command "ag --vimgrep -z %p %f"
          helm-grep-default-recurse-command "ag --vimgrep -z %p %f"))

  (use-package helm-ag
    :ensure t)

  (use-package helm-mu
    :ensure t)

  (use-package helm-projectile
    :ensure t
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))

(use-package hydra
  :ensure t
  :config
  (global-set-key (kbd "C-x m")
                  (defhydra hydra-onze (:color teal)
                    "
      Onzes functions

     Buffers                     Blog                        Other
------------------------------------------------------------------------------------------
  _i_: indent buffer %(my-where-is-first 'onze-indent-whole-buffer)     _n_: create new blog post     _p_: switch project %(my-where-is-first 'helm-projectile-switch-project)
  _r_: rename buffer and file                               _s_: start clock
  _v_: toggle transparency                                  _f_: stop/finish cock
                                                          _t_: create report-table
                                                          _m_: compose mail
"
                    ("i" onze-indent-whole-buffer       nil)
                    ("r" onze-rename-file-and-buffer    nil)
                    ("n" onze-create-new-blog-post      nil)
                    ("p" helm-projectile-switch-project nil)
                    ("m" mu4e-compose-new               nil)
                    ("s" org-clock-in                   nil)
                    ("f" org-clock-out                  nil)
                    ("t" org-clock-report               nil)
                    ("v" toggle-transparency            nil)
                    ("q" nil                            "cancel"))))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode +1)
  ;; hide if there are no changes
  (setq git-gutter:hide-gutter t))

(use-package ycmd
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'ycmd-mode)
  :config
  (set-variable 'ycmd-server-command '("python" "/home/onze/Applications/ycmd/ycmd"))
  (set-variable 'ycmd-global-config "/home/onze/Applications/ycmd/cpp/ycm/.ycm_extra_conf.py")

  (eval-after-load "flycheck-ycmd-autoloads"
    '(add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))

  (use-package company-ycmd
    :ensure t
    :config
    (company-ycmd-setup)
    (add-to-list 'company-backends (company-mode/backend-with-yas 'company-ycmd))))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package powerline
  :ensure t
  :config

  (use-package spaceline
    :ensure t
    :config
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)
    (setq powerline-height 10)
    (setq powerline-raw "  ")
    (setq ns-use-srgb-colorspace nil)))

(use-package magit
  :ensure nil
  :load-path "/home/onze/.emacs.d/git-package/magit/lisp"
  :bind (("M-s" . magit-status))
  :config
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list
                 "~/.emacs.d/git-package/magit/Documentation/"))

  (use-package magit-gitflow
    :load-path "/home/onze/.emacs.d/git-packages/magit-gitflow"
    :disabled t
    :config
    add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(use-package org-clock
  :defer t
  :config
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate))

(use-package async
  :defer t
  :ensure t
  :config
  (require 'async-bytecomp))

(use-package dash
  :defer t)

(use-package paradox
  :commands (paradox-list-packages)
  :config
  (setq paradox-automatically-star nil)
  (setq paradox-display-star-count nil)
  (setq paradox-execute-asynchronously t))

(use-package glsl-mode
  :mode (("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)))

(use-package cmake-mode
  :ensure t)

(use-package highlight-symbol
  :ensure t)

(use-package eldoc
  :diminish eldoc-mode)

(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php\\'"   . web-mode)
         ("\\.jsp\\'"   . web-mode)
         ("\\.erb\\'"   . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package nginx-mode
  :ensure t)

(use-package dictcc
  :ensure t
  :config
  (setq dictcc-source-lang "de")
  (setq dictcc-destination-lang "en"))

(use-package gitignore-mode
  :mode ("\\.gitignore\\'" . gitignore-mode))

(use-package nlinum
  :ensure t)

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-width 1)
  (setq fci-rule-color "gray71")
  (setq-default fill-column 80)
  (add-hook 'c-mode-common-hook 'fci-mode)
  (add-hook 'mail-mode-hook 'fci-mode)
  (add-hook 'js2-mode-hook 'fci-mode))

(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mmd\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook 'orgtbl-mode)
  (add-hook 'markdown-mode-hook
            (lambda()
              (add-hook 'after-save-hook 'org-tables-to-markdown  nil 'make-it-local))))

(use-package rainbow-mode
  :ensure t
  :config
  (dolist
      (hook '(css-mode-hook
              html-mode-hook
              js-mode-hook
              emacs-lisp-mode-hook
              ;; org-mode-hook
              text-mode-hook))
    (add-hook hook 'rainbow-mode)))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))

  (use-package flycheck-rust
    :ensure t
    :config
    (add-hook 'rust-mode-hook #'flycheck-rust-setup))

  ;; (flycheck-add-next-checker 'c/c++-clang 'c/c++-googlelint 'append)
  )

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
  :commands flyspell-buffer
  :config
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell"))
   (t (setq ispell-program-name nil)))

  (defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
    "If RUN-TOGETHER is true, spell check the CamelCase words."
    (let (args)
      (cond
       ((string-match  "aspell$" ispell-program-name)
        ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
        (setq args (list "--sug-mode=ultra" "--lang=de_DE"))
        (if RUN-TOGETHER
            (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2"))))))
      args))

  ;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
  ;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
  (setq ispell-extra-args (flyspell-detect-ispell-args t))

  (defadvice ispell-word (around my-ispell-word activate)
    (let ((old-ispell-extra-args ispell-extra-args))
      (ispell-kill-ispell t)
      (setq ispell-extra-args (flyspell-detect-ispell-args))
      ad-do-it
      (setq ispell-extra-args old-ispell-extra-args)
      (ispell-kill-ispell t)))

  (eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'ispell-word)
       (define-key flyspell-mouse-map [mouse-3] #'ispell-word))))

(use-package ispell
  :defer t
  :bind (([f8]. fd-switch-dictionary))
  :config
  (defun fd-switch-dictionary()
    (interactive)
    (let* ((dic ispell-current-dictionary)
           (change (if (string= dic "english") "deutsch" "english")))
      (ispell-change-dictionary change)
      (message "Dictionary switched from %s to %s" dic change))))

(use-package unbound
  :ensure t)

(use-package cc-mode
  :ensure t

  :config
  (setq-default c-basic-offset 4))

(use-package c++-mode
  :ensure nil
  :mode (("\\.[hH]\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode))
  :config

  (add-hook 'c++-mode-hook
            (lambda ()
              (c-set-style "stroustrup")))

  (use-package clang-format
    :commands (clang-format-buffer)
    :config
    (setq clang-format-executable "/usr/bin/clang-format"))

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
  (define-key c++-mode-map (kbd "C-c x") 'onze-cmake-run))

(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode))

(use-package groovy-mode
  :mode ("\\.groovy\\'" . groovy-mode))

(use-package gradle-mode
  :mode ("\\.gradle\\'" . gradle-mode))

(use-package solarized-theme
  :ensure t)

;; (use-package material-theme
;;   :ensure t)

;; PACKAGE: JS2
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq-default js2-global-externs '("exports" "module" "require" "setTimeout" "THREE"))
  (setq-default js2-basic-offset 2)
  (define-key js2-mode-map (kbd "M-SPC") 'js2-mode-toggle-element))

;; PACKAGE: TERN
(use-package tern
  :defer t
  :init
  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  (add-hook 'web-mode-hook (lambda () (tern-mode t)))
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
  :mode ("\\.json\\'" . json-mode)
  :config
  (setq js-indent-level 2))

(use-package rubocop
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package elisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode))

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

;; keyboard shortcuts for resizing windows
(global-set-key (kbd "<C-s-left>") (lambda () (interactive) (shrink-window-horizontally 5)))
(global-set-key (kbd "<C-s-right>") (lambda () (interactive) (enlarge-window-horizontally 5)))
(global-set-key (kbd "<C-s-down>") (lambda () (interactive) (shrink-window 5)))
(global-set-key (kbd "<C-s-up>") (lambda () (interactive) (enlarge-window 5)))

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

(defun onze-run-current-file ()
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
        (when (y-or-n-p "Buffer modified. Do you want to save first?")
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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#272822" "#f92672" "#a6e22e" "#f4bf75" "#66d9ef" "#ae81ff" "#66d9ef" "#f8f8f2"])
 '(ansi-term-color-vector
   [unspecified "#272822" "#f92672" "#a6e22e" "#f4bf75" "#66d9ef" "#ae81ff" "#66d9ef" "#f8f8f2"] t)
 '(custom-enabled-themes (quote (base16-onze-google-dark)))
 '(custom-safe-themes
   (quote
    ("e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "36ead886598f0bccca6137ce39a084c6cec1439d5199072187ba156006f04ba8" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "2305decca2d6ea63a408edd4701edf5f4f5e19312114c9d1e1d5ffe3112cde58" "113ae6902d98261317b5507e55ac6e7758af81fc4660c34130490252640224a2" "3ae2686d180c6ea92ccfe418d881b7e984be62639dea15f330d31904cd228f10" "f7799c0c6fc9e8b02853f36a6a7e2b7ce57e51f89d52ad1e25ab302c80e9dd19" "4a171c5c7386e30d16f1ba552fd652dc1c7d79c3b216ef2e9296a7025482ce58" "3270c6e92aa1dd60f8317e7913a658173ad9c953657792cd6f805bd4dcf4e476" "d3e906a019c1b18c6f091eaf05d441be8991bd57e7535a7f0d52f92f7770f37c" "d085be39ec443f01b3a581766ecc292921c76262f25268ff0786310db7351edc" "7d2f37e0de6f872c3d90801b0b8f27d03165876b82c685532d7adafaa08cc30f" "641b002f11edd63d448f1fa8b19e2b318cd5a5edfd6898de025a6754da77c02a" "ab12e149a516519a4794245d65242a13f28ffb4b86b20b9e98551a7fb1e02dd2" "c2c86325e71c2f3b70838e91dfee8f80471021824fe896106922595265cfc93e" "d362b08d053513c09741093741abff117ef807b664e9d85c2c520143e8551022" "c98ce4ee9d83b21991a1d393030cee776458e50c331be8f2911c9cf6cb6bf719" "c6b26b882659f842363a235d8ba376998811d2b73336fc48ad2e10841030cdab" "0240d45644b370b0518e8407f5990a243c769fb0150a7e74297e6f7052a04a72" "9c961e80e46c08fb6d0f71cc2654f302ef214fb69eccb390917a82a42a275f27" "ac31699d6255ef22b646ebc80e69f518929b57f2f0855a814170d060db78a5f1" "d92db4e2b227ce1506a9b95160f6ae594b9844cc685f7c67fb109f4fd6bb0388" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "e3a3b7d7fe89b5d57d40bc825ca2324875a6f37bd63da66f2a6fc68cc8b2ee95" "7b3124eec8107900b9c03eb7370e3ef6a7a86d896361c8e85bbbe4bbc952124b" "5be9916bc54fd57fcb5a3d9426ed5aec0389a4bd4ed75b497a0d4cf2bbde7a4b" "3a6d8378f18a5ba2a0c88ddcad2500675a755d056b3343b13b36911453e96c34" "4f034ec900e09a0ecfe7025d5f8c02bcf4448d131c5d0ea962b8b74b2ce4d4ea" "3994497138fa263aadde66e0f869e2c2426afc342bf2b06da4c2431473fde61c" "7e3b0c38791f8ee1d067bf7b84cd916ffea6392b428db4b375b47504a89edc6c" default)))
 '(desktop-load-locked-desktop t)
 '(diary-file "/home/onze/Dropbox/LinuxBackup/diary")
 '(fci-rule-color "#ECEFF1")
 '(fringe-mode (quote (1 . 1)) nil (fringe))
 '(hl-sexp-background-color "#efebe9")
 '(magit-diff-refine-hunk t)
 '(paradox-github-token t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#B71C1C")
     (40 . "#FF5722")
     (60 . "#FFA000")
     (80 . "#558b2f")
     (100 . "#00796b")
     (120 . "#2196f3")
     (140 . "#4527A0")
     (160 . "#B71C1C")
     (180 . "#FF5722")
     (200 . "#FFA000")
     (220 . "#558b2f")
     (240 . "#00796b")
     (260 . "#2196f3")
     (280 . "#4527A0")
     (300 . "#B71C1C")
     (320 . "#FF5722")
     (340 . "#FFA000")
     (360 . "#558b2f"))))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-search-highlight-persist-highlight-face ((t (:background "magenta" :foreground "white")))))

;; Make FlyCheck happy...
;;; init.el ends here
