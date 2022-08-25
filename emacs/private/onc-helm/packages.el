;;; packages.el --- onze-helm layer packages file for Spacemacs.
;;
;; Author: Onze <onze@Amstelveen>
;;
;;; License: GPLv3

;;; Code:

(defconst onc-helm-packages
  '(helm))

(defun onc-helm/post-init-helm ()
  (use-package helm
    :ensure t
    :bind (("C-h C-h" . helm-apropos)
           ("M-x"     . helm-M-x)
           ("C-x b"   . helm-mini)
           ("C-x C-f" . helm-find-files)
           :map helm-map
           ("<tab>" . helm-execute-persistent-action)
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
    (add-to-list 'helm-boring-file-regexp-list "\\.DS_Store$")

    ;; Use ack instead of grep
    (when (executable-find "ack")
      (setq helm-grep-default-command "ack -Hn --no-group --no-color %p %f"
            helm-grep-default-recurse-command "ack -H --no-group --no-color %p %f"))

    ;; Even better, use ag if it's available
    (when (executable-find "ag")
      (setq helm-grep-default-command "ag --vimgrep -z %p %f"
            helm-grep-default-recurse-command "ag --vimgrep -z %p %f"))))

;;; packages.el ends here
