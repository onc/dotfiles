;;; packages.el --- vim layer packages file for Spacemacs.
;;
;; Author: Onze <onze@Amstelveen>
;;
;;; License: GPLv3

;;; Code:

(defconst onc-vim-packages
  '(evil evil-leader evil-search-highlight-persist))

(defun onc/indent-whole-buffer ()
  "Delete trailing whitespace, indent and untabify whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)))

(defun onc-vim/init-evil-leader ()
  (use-package evil-leader
    :ensure t
    :init (global-evil-leader-mode)
    :config
    (evil-leader/set-key
      "f" 'onc/indent-whole-buffer
      "1" 'highlight-symbol-at-point
      "0" 'highlight-symbol-remove-all
      "gst" 'magit-status
      "glg" 'magit-log-all
      "ci" 'evilnc-comment-or-uncomment-lines
      "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
      "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
      "a" 'align-regexp
      "s" 'helm-projectile-ag)

    (evil-leader/set-key-for-mode
      'c++-mode "f" 'clang-format-buffer)

    (evil-leader/set-key-for-mode
      'python-mode "f" 'elpy-format-code)

    (evil-leader/set-key-for-mode
      'rust-mode "f" 'cargo-process-fmt)
    ))

(defun onc-vim/init-evil-search-highlight-persist ()
  (use-package evil-search-highlight-persist
    :ensure t))

(defun onc-vim/post-init-evil ()
  (defun copy-to-end-of-line ()
    "Yank from point to end of line."
    (interactive)
    (evil-yank (point) (point-at-eol)))

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
    (define-key onc-window-map (kbd "d") 'perspeen-delete-ws)
    )

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
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (evil-set-initial-state 'pdf-annot-list-mode 'emacs)
  (evil-set-initial-state 'calendar-mode 'emacs)
  )

;;; packages.el ends here
