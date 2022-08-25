;;; packages.el --- onc-vue layer packages file for Spacemacs.
;;
;; Author: Onze <onze@Amstelveen>
;;
;;; License: GPLv3

;;; Code:

(defconst onc-vue-packages
  '(vue-mode flycheck company))

(defun onc-vue/post-init-company ()
  (spacemacs|add-company-hook vue-mode))

(defun onc-vue/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'vue-mode))

(defun onc-vue/init-vue-mode ()
  (use-package vue-mode
    :mode "\\.vue\\'"
    :hook (vue-mode-hook . (lambda ()
                             (setq syntax-ppss-table nil)
                             (tide-setup)
                             (flycheck-mode +1)
                             (when (configuration-layer/package-usedp 'company)
                               (company-mode-on))))
    :config
    ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
    (setq mmm-submode-decoration-level 0)))

;;; packages.el ends here
