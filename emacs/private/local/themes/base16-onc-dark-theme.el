;; Base16 onc (https://github.com/chriskempson/base16)
;; Scheme: Christian van Onzenoodt (https://github.com/onc)

;;; base16-onc-dark-theme.el

;;; Code:

(deftheme base16-onc-dark)

(let ((onc-black "#212121")
      (onc-dark-grey "#424242")
      (base02 "#616161")
      (base03 "#757575")
      (base04 "#9E9E9E")
      (base05 "#EEEEEE")
      (base06 "#F5F5F5")
      (base07 "#FAFAFA")
      (onc-red "#FF1744")
      (onc-blue "#00B0FF")
      (base0A "#D500F9")
      (onc-green "#1DE9B6")
      (onc-cyan "#18FFFF")
      (onc-orange "#FF9100")
      (onc-yellow "#FFEA00")
      (onc-brown "#8D6E63"))

  (unless (display-graphic-p)
    (setq onc-black "black"
          onc-dark-grey "color-18"
          base02 "color-19"
          base03 "brightblack"
          base04 "color-20"
          base05 "white"
          base06 "color-21"
          base07 "brightwhite"
          onc-red "red"
          onc-blue "color-16"
          base0A "yellow"
          onc-green "green"
          onc-cyan "cyan"
          onc-orange "blue"
          onc-yellow "magenta"
          onc-brown "color-17"))

  (custom-theme-set-faces
   'base16-onc-dark

   `(spaceline-highlight-face ((t (:foreground ,onc-black :background ,onc-orange))))
   `(powerline-active1 ((t (:foreground ,onc-black :background ,onc-green))))

   `(perspeen-selected-face ((t (:foreground ,onc-green))))

   ;; Built-in stuff (Emacs 23)
   `(border ((t (:background ,base03))))
   `(border-glyph ((t (nil))))
   `(cursor ((t (:background ,onc-red))))
   `(default ((t (:background ,onc-black :foreground ,base05))))
   `(fringe ((t (:background ,onc-black :foreground ,onc-dark-grey))))
   `(vertical-border ((t (:foreground ,onc-dark-grey))))
   `(gui-element ((t (:background ,base03 :foreground ,base06))))
   `(highlight ((t (:background ,onc-dark-grey))))
   `(link ((t (:foreground ,onc-orange))))
   `(link-visited ((t (:foreground ,onc-yellow))))
   `(minibuffer-prompt ((t (:foreground ,onc-orange))))
   `(mode-line ((t (:background ,onc-dark-grey :foreground ,base07 :box nil))))
   `(mode-line-buffer-id ((t (:foreground ,onc-green :background nil))))
   `(mode-line-emphasis ((t (:foreground ,base06 :slant italic))))
   `(mode-line-highlight ((t (:foreground ,onc-yellow :box nil))))
   `(mode-line-inactive ((t (:background ,onc-black :foreground ,base02 :box nil))))
   `(region ((t (:background ,base02))))
   `(secondary-selection ((t (:background ,base03))))
   `(error ((t (:foreground ,onc-red :weight bold))))
   `(warning ((t (:foreground ,onc-blue :weight bold))))
   `(success ((t (:foreground ,onc-green :weight bold))))

   `(header-line ((t (:inherit mode-line :foreground ,onc-yellow :background nil))))

   ;; Font-lock stuff
   `(font-lock-builtin-face ((t (:foreground ,onc-cyan))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,base02))))
   `(font-lock-comment-face ((t (:foreground ,base03))))
   `(font-lock-constant-face ((t (:foreground ,onc-blue))))
   `(font-lock-doc-face ((t (:foreground ,base04))))
   `(font-lock-doc-string-face ((t (:foreground ,base03))))
   `(font-lock-function-name-face ((t (:foreground ,onc-orange))))
   `(font-lock-keyword-face ((t (:foreground ,onc-yellow))))
   `(font-lock-negation-char-face ((t (:foreground ,onc-green))))
   `(font-lock-preprocessor-face ((t (:foreground ,onc-orange))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,base0A))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,onc-yellow))))
   `(font-lock-string-face ((t (:foreground ,onc-green))))
   `(font-lock-type-face ((t (:foreground ,base0A :inherit bold))))
   `(font-lock-variable-name-face ((t (:foreground ,onc-cyan))))
   `(font-lock-warning-face ((t (:foreground ,onc-red))))

   ;; linum-mode
   `(linum ((t (:background ,onc-dark-grey :foreground ,base03))))

   ;; Search
   `(match ((t (:foreground ,onc-orange :background ,onc-dark-grey :inverse-video t))))
   `(isearch ((t (:foreground ,base0A :background ,onc-dark-grey :inverse-video t))))
   `(isearch-lazy-highlight-face ((t (:foreground ,onc-cyan :background ,onc-dark-grey :inverse-video t))))
   `(isearch-fail ((t (:background ,onc-dark-grey :inherit font-lock-warning-face :inverse-video t))))
   `(evil-search-highlight-persist-highlight-face ((t (:background ,onc-dark-grey :inherit font-lock-warning-face :inverse-video t))))

   ;; Popups
   `(popup-face ((t (:foreground ,base05 :background ,base02))))
   `(popup-isearch-match ((t (:foreground ,onc-black :background ,onc-green))))
   `(popup-scroll-bar-background-face ((t (:background ,base03))))
   `(popup-scroll-bar-onc-black-face ((t (:background ,base05))))
   `(popup-summary-face ((t (:foreground ,base04))))
   `(popup-tip-face ((t (:foreground ,onc-black :background ,base0A))))
   `(popup-menu-mouse-face ((t (:foreground ,onc-black :background ,onc-orange))))
   `(popup-menu-selection-face ((t (:foreground ,onc-black :background ,onc-cyan))))

   ;; Flymake
   `(flymake-warnline ((t (:underline ,onc-blue :background ,onc-dark-grey))))
   `(flymake-errline ((t (:underline ,onc-red :background ,onc-dark-grey))))

   ;; Clojure errors
   `(clojure-test-failure-face ((t (:background nil :inherit flymake-warnline))))
   `(clojure-test-error-face ((t (:background nil :inherit flymake-errline))))
   `(clojure-test-success-face ((t (:background nil :foreground nil :underline ,onc-green))))

   ;; For Brian Carper's extended clojure syntax table
   `(clojure-keyword ((t (:foreground ,base0A))))
   `(clojure-parens ((t (:foreground ,base06))))
   `(clojure-braces ((t (:foreground ,onc-green))))
   `(clojure-brackets ((t (:foreground ,base0A))))
   `(clojure-double-quote ((t (:foreground ,onc-cyan :background nil))))
   `(clojure-special ((t (:foreground ,onc-orange))))
   `(clojure-java-call ((t (:foreground ,onc-yellow))))

   ;; MMM-mode
   `(mmm-code-submode-face ((t (:background ,base03))))
   `(mmm-comment-submode-face ((t (:inherit font-lock-comment-face))))
   `(mmm-output-submode-face ((t (:background ,base03))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,onc-yellow))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,onc-orange))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,onc-cyan))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,onc-green))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,base0A))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,onc-blue))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,onc-red))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,base03))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,base05))))

   ;; which-function
   `(which-func ((t (:foreground ,onc-orange :background nil :weight bold))))

   `(trailing-whitespace ((t (:background ,onc-cyan :foreground ,base0A))))
   `(whitespace-empty ((t (:foreground ,onc-red :background ,base0A))))
   `(whitespace-hspace ((t (:background ,base04 :foreground ,base04))))
   `(whitespace-indentation ((t (:background ,base0A :foreground ,onc-red))))
   `(whitespace-line ((t (:background ,onc-dark-grey :foreground ,onc-brown))))
   `(whitespace-newline ((t (:foreground ,base04))))
   `(whitespace-space ((t (:background ,onc-dark-grey :foreground ,base04))))
   `(whitespace-space-after-tab ((t (:background ,base0A :foreground ,onc-red))))
   `(whitespace-space-before-tab ((t (:background ,onc-blue :foreground ,onc-red))))
   `(whitespace-tab ((t (:background ,base04 :foreground ,base04))))
   `(whitespace-trailing ((t (:background ,onc-red :foreground ,base0A))))

   ;; Parenthesis matching (built-in)
   `(show-paren-match ((t (:background ,onc-orange :foreground ,base03))))
   `(show-paren-mismatch ((t (:background ,onc-blue :foreground ,base03))))

   ;; Parenthesis matching (mic-paren)
   `(paren-face-match ((t (:foreground nil :background nil :inherit show-paren-match))))
   `(paren-face-mismatch ((t (:foreground nil :background nil :inherit show-paren-mismatch))))
   `(paren-face-no-match ((t (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis dimming (parenface)
   `(paren-face ((t (:foreground ,base04 :background nil))))

   `(sh-heredoc ((t (:foreground nil :inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((t (:foreground nil :inherit font-lock-preprocessor-face))))
   `(slime-highlight-edits-face ((t (:weight bold))))
   `(slime-repl-input-face ((t (:weight normal :underline nil))))
   `(slime-repl-prompt-face ((t (:underline nil :weight bold :foreground ,onc-yellow))))
   `(slime-repl-result-face ((t (:foreground ,onc-green))))
   `(slime-repl-output-face ((t (:foreground ,onc-orange :background ,onc-dark-grey))))

   `(csv-separator-face ((t (:foreground ,onc-blue))))

   `(diff-added ((t (:foreground ,onc-green))))
   `(diff-changed ((t (:foreground ,base0A))))
   `(diff-removed ((t (:foreground ,onc-red))))
   `(diff-header ((t (:background ,onc-dark-grey))))
   `(diff-file-header ((t (:background ,base02))))
   `(diff-hunk-header ((t (:background ,onc-dark-grey :foreground ,onc-yellow))))

   `(diff-hl-change ((t (:foreground ,base0A))))
   `(diff-hl-delete ((t (:foreground ,onc-red))))
   `(diff-hl-insert ((t (:foreground ,onc-green))))

   `(ediff-even-diff-A ((t (:foreground nil :background nil :inverse-video t))))
   `(ediff-even-diff-B ((t (:foreground nil :background nil :inverse-video t))))
   `(ediff-odd-diff-A  ((t (:foreground ,base04 :background nil :inverse-video t))))
   `(ediff-odd-diff-B  ((t (:foreground ,base04 :background nil :inverse-video t))))

   `(eldoc-highlight-function-argument ((t (:foreground ,onc-green :weight bold))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((t (:foreground ,base06))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,onc-green :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,onc-red))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,base0A))))

   ;; auctex
   `(font-latex-bold-face ((t (:foreground ,onc-green))))
   `(font-latex-doctex-documentation-face ((t (:background ,base03))))
   `(font-latex-italic-face ((t (:foreground ,onc-green))))
   `(font-latex-math-face ((t (:foreground ,onc-blue))))
   `(font-latex-sectioning-0-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-1-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-2-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-3-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-4-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-5-face ((t (:foreground ,base0A))))
   `(font-latex-sedate-face ((t (:foreground ,onc-cyan))))
   `(font-latex-string-face ((t (:foreground ,base0A))))
   `(font-latex-verbatim-face ((t (:foreground ,onc-blue))))
   `(font-latex-warning-face ((t (:foreground ,onc-red))))

   ;; dired+
   `(diredp-compressed-file-suffix ((t (:foreground ,onc-orange))))
   `(diredp-dir-heading ((t (:foreground nil :background nil :inherit heading))))
   `(diredp-dir-priv ((t (:foreground ,onc-cyan :background nil))))
   `(diredp-exec-priv ((t (:foreground ,onc-orange :background nil))))
   `(diredp-executable-tag ((t (:foreground ,onc-red :background nil))))
   `(diredp-file-name ((t (:foreground ,base0A))))
   `(diredp-file-suffix ((t (:foreground ,onc-green))))
   `(diredp-flag-mark-line ((t (:background nil :inherit highlight))))
   `(diredp-ignored-file-name ((t (:foreground ,base04))))
   `(diredp-link-priv ((t (:background nil :foreground ,onc-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,onc-red))))
   `(diredp-mode-line-marked ((t (:foreground ,onc-green))))
   `(diredp-no-priv ((t (:background nil))))
   `(diredp-number ((t (:foreground ,base0A))))
   `(diredp-other-priv ((t (:background nil :foreground ,onc-yellow))))
   `(diredp-rare-priv ((t (:foreground ,onc-red :background nil))))
   `(diredp-read-priv ((t (:foreground ,onc-green :background nil))))
   `(diredp-symlink ((t (:foreground ,onc-yellow))))
   `(diredp-write-priv ((t (:foreground ,base0A :background nil))))

   ;; term and ansi-term
   `(term-color-black ((t (:foreground ,base02 :background ,onc-black))))
   `(term-color-white ((t (:foreground ,base05 :background ,base07))))
   `(term-color-red ((t (:foreground ,onc-red :background ,onc-red))))
   `(term-color-yellow ((t (:foreground ,base0A :background ,base0A))))
   `(term-color-green ((t (:foreground ,onc-green :background ,onc-green))))
   `(term-color-cyan ((t (:foreground ,onc-cyan :background ,onc-cyan))))
   `(term-color-blue ((t (:foreground ,onc-orange :background ,onc-orange))))
   `(term-color-magenta ((t (:foreground ,onc-yellow :background ,onc-yellow))))

   `(link ((t (:foreground nil :underline t))))
   `(widget-button ((t (:underline t))))
   `(widget-field ((t (:background ,base03 :box (:line-width 1 :color ,base06)))))

   ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
   `(compilation-column-number ((t (:foreground ,base0A))))
   `(compilation-line-number ((t (:foreground ,base0A))))
   `(compilation-message-face ((t (:foreground ,onc-orange))))
   `(compilation-mode-line-exit ((t (:foreground ,onc-green))))
   `(compilation-mode-line-fail ((t (:foreground ,onc-red))))
   `(compilation-mode-line-run ((t (:foreground ,onc-orange))))

   ;; Grep
   `(grep-context-face ((t (:foreground ,base04))))
   `(grep-error-face ((t (:foreground ,onc-red :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,onc-orange))))
   `(grep-match-face ((t (:foreground nil :background nil :inherit match))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,onc-black :background ,base05))))
   `(company-tooltip-annotation ((t (:foreground ,base0A))))
   `(company-tooltip-selection ((t (:foreground ,onc-orange :background ,onc-dark-grey))))
   ;; `(company-tooltip-mouse ((t (:background ,base02))))
   ;; `(company-tooltip-common ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg-1))))
   ;; `(company-tooltip-common-selection ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-gray))))
   ;; `(company-preview ((t (:background ,atom-one-dark-bg))))
   ;; `(company-preview-common ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg))))
   `(company-scrollbar-fg ((t (:background ,onc-orange))))
   `(company-scrollbar-bg ((t (:background ,onc-black))))

   `(regex-tool-matched-face ((t (:foreground nil :background nil :inherit match))))

   ;; Cscope
   `(cscope-file-face ((t (:foreground ,onc-green))))
   `(cscope-function-face ((t (:foreground ,onc-orange))))
   `(cscope-line-number-face ((t (:foreground ,base0A))))
   `(cscope-mouse-face ((t (:background ,onc-dark-grey :foreground ,base04))))
   `(cscope-separator-face ((t (:foreground ,onc-red :overline t :underline t :weight bold))))

   ;; mark-multiple
   `(mm/master-face ((t (:inherit region :foreground nil :background nil))))
   `(mm/mirror-face ((t (:inherit region :foreground nil :background nil))))

   ;; org-mode
   `(org-agenda-structure ((t (:foreground ,onc-yellow))))
   `(org-agenda-date ((t (:foreground ,onc-orange :underline nil))))
   `(org-agenda-done ((t (:foreground ,onc-green))))
   `(org-agenda-dimmed-todo-face ((t (:foreground ,base04))))
   `(org-block ((t (:foreground ,onc-blue))))
   `(org-code ((t (:foreground ,base0A))))
   `(org-column ((t (:background ,onc-dark-grey))))
   `(org-column-title ((t (:inherit org-column :weight bold :underline t))))
   `(org-date ((t (:foreground ,onc-yellow :underline t))))
   `(org-document-info ((t (:foreground ,onc-cyan))))
   `(org-document-info-keyword ((t (:foreground ,onc-green))))
   `(org-document-title ((t (:weight bold :foreground ,onc-blue :height 1.44))))
   `(org-done ((t (:foreground ,onc-green))))
   `(org-ellipsis ((t (:foreground ,base04))))
   `(org-footnote ((t (:foreground ,onc-cyan))))
   `(org-formula ((t (:foreground ,onc-red))))
   `(org-hide ((t (:foreground ,base03))))
   `(org-link ((t (:foreground ,onc-orange))))
   `(org-scheduled ((t (:foreground ,onc-green))))
   `(org-scheduled-previously ((t (:foreground ,onc-blue))))
   `(org-scheduled-today ((t (:foreground ,onc-green))))
   `(org-special-keyword ((t (:foreground ,onc-blue))))
   `(org-table ((t (:foreground ,onc-yellow))))
   `(org-todo ((t (:foreground ,onc-red))))
   `(org-upcoming-deadline ((t (:foreground ,onc-blue))))
   `(org-warning ((t (:weight bold :foreground ,onc-red))))

   `(markdown-url-face ((t (:inherit link))))
   `(markdown-link-face ((t (:foreground ,onc-orange :underline t))))

   `(hl-sexp-face ((t (:background ,base03))))
   `(highlight-80+ ((t (:background ,base03))))

   ;; Python-specific overrides
   `(py-builtins-face ((t (:foreground ,onc-blue :weight normal))))

   ;; js2-mode
   `(js2-warning-face ((t (:underline ,onc-blue))))
   `(js2-error-face ((t (:foreground nil :underline ,onc-red))))
   `(js2-external-variable-face ((t (:foreground ,onc-yellow))))
   `(js2-function-param-face ((t (:foreground ,onc-orange))))
   `(js2-instance-member-face ((t (:foreground ,onc-orange))))
   `(js2-private-function-call-face ((t (:foreground ,onc-red))))

   ;; js3-mode
   `(js3-warning-face ((t (:underline ,onc-blue))))
   `(js3-error-face ((t (:foreground nil :underline ,onc-red))))
   `(js3-external-variable-face ((t (:foreground ,onc-yellow))))
   `(js3-function-param-face ((t (:foreground ,onc-orange))))
   `(js3-jsdoc-tag-face ((t (:foreground ,onc-blue))))
   `(js3-jsdoc-type-face ((t (:foreground ,onc-cyan))))
   `(js3-jsdoc-value-face ((t (:foreground ,base0A))))
   `(js3-jsdoc-html-tag-name-face ((t (:foreground ,onc-orange))))
   `(js3-jsdoc-html-tag-delimiter-face ((t (:foreground ,onc-green))))
   `(js3-instance-member-face ((t (:foreground ,onc-orange))))
   `(js3-private-function-call-face ((t (:foreground ,onc-red))))

   ;; nxml
   `(nxml-name-face ((t (:foreground unspecified :inherit font-lock-constant-face))))
   `(nxml-attribute-local-name-face ((t (:foreground unspecified :inherit font-lock-variable-name-face))))
   `(nxml-ref-face ((t (:foreground unspecified :inherit font-lock-preprocessor-face))))
   `(nxml-delimiter-face ((t (:foreground unspecified :inherit font-lock-keyword-face))))
   `(nxml-delimited-data-face ((t (:foreground unspecified :inherit font-lock-string-face))))
   `(rng-error-face ((t (:underline ,onc-red))))

   ;; RHTML
   `(erb-delim-face ((t (:background ,base03))))
   `(erb-exec-face ((t (:background ,base03 :weight bold))))
   `(erb-exec-delim-face ((t (:background ,base03))))
   `(erb-out-face ((t (:background ,base03 :weight bold))))
   `(erb-out-delim-face ((t (:background ,base03))))
   `(erb-comment-face ((t (:background ,base03 :weight bold :slant italic))))
   `(erb-comment-delim-face ((t (:background ,base03))))

   ;; Message-mode
   `(message-header-other ((t (:foreground nil :background nil :weight normal))))
   `(message-header-subject ((t (:inherit message-header-other :weight bold :foreground ,base0A))))
   `(message-header-to ((t (:inherit message-header-other :weight bold :foreground ,onc-blue))))
   `(message-header-cc ((t (:inherit message-header-to :foreground nil))))
   `(message-header-name ((t (:foreground ,onc-orange :background nil))))
   `(message-header-newsgroups ((t (:foreground ,onc-cyan :background nil :slant normal))))
   `(message-separator ((t (:foreground ,onc-yellow))))

   ;; Jabber
   `(jabber-chat-prompt-local ((t (:foreground ,base0A))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,onc-blue))))
   `(jabber-chat-prompt-system ((t (:foreground ,base0A :weight bold))))
   `(jabber-chat-text-local ((t (:foreground ,base0A))))
   `(jabber-chat-text-foreign ((t (:foreground ,onc-blue))))
   `(jabber-chat-text-error ((t (:foreground ,onc-red))))

   `(jabber-roster-user-online ((t (:foreground ,onc-green))))
   `(jabber-roster-user-xa ((t :foreground ,base04)))
   `(jabber-roster-user-dnd ((t :foreground ,base0A)))
   `(jabber-roster-user-away ((t (:foreground ,onc-blue))))
   `(jabber-roster-user-chatty ((t (:foreground ,onc-yellow))))
   `(jabber-roster-user-error ((t (:foreground ,onc-red))))
   `(jabber-roster-user-offline ((t (:foreground ,base04))))

   `(jabber-rare-time-face ((t (:foreground ,base04))))
   `(jabber-activity-face ((t (:foreground ,onc-yellow))))
   `(jabber-activity-personal-face ((t (:foreground ,onc-cyan))))

   ;; Gnus
   `(gnus-cite-1 ((t (:inherit outline-1 :foreground nil))))
   `(gnus-cite-2 ((t (:inherit outline-2 :foreground nil))))
   `(gnus-cite-3 ((t (:inherit outline-3 :foreground nil))))
   `(gnus-cite-4 ((t (:inherit outline-4 :foreground nil))))
   `(gnus-cite-5 ((t (:inherit outline-5 :foreground nil))))
   `(gnus-cite-6 ((t (:inherit outline-6 :foreground nil))))
   `(gnus-cite-7 ((t (:inherit outline-7 :foreground nil))))
   `(gnus-cite-8 ((t (:inherit outline-8 :foreground nil))))
   ;; there are several more -cite- faces...
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-header-from ((t (:inherit message-header-other-face :weight bold :foreground ,onc-blue))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-button ((t (:inherit link :foreground nil))))
   `(gnus-signature ((t (:inherit font-lock-comment-face))))

   `(gnus-summary-normal-unread ((t (:foreground ,onc-orange :weight normal))))
   `(gnus-summary-normal-read ((t (:foreground ,base06 :weight normal))))
   `(gnus-summary-normal-ancient ((t (:foreground ,onc-cyan :weight normal))))
   `(gnus-summary-normal-ticked ((t (:foreground ,onc-blue :weight normal))))
   `(gnus-summary-low-unread ((t (:foreground ,base04 :weight normal))))
   `(gnus-summary-low-read ((t (:foreground ,base04 :weight normal))))
   `(gnus-summary-low-ancient ((t (:foreground ,base04 :weight normal))))
   `(gnus-summary-high-unread ((t (:foreground ,base0A :weight normal))))
   `(gnus-summary-high-read ((t (:foreground ,onc-green :weight normal))))
   `(gnus-summary-high-ancient ((t (:foreground ,onc-green :weight normal))))
   `(gnus-summary-high-ticked ((t (:foreground ,onc-blue :weight normal))))
   `(gnus-summary-cancelled ((t (:foreground ,onc-red :background nil :weight normal))))

   `(gnus-group-mail-low ((t (:foreground ,base04))))
   `(gnus-group-mail-low-empty ((t (:foreground ,base04))))
   `(gnus-group-mail-1 ((t (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-mail-2 ((t (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-mail-3 ((t (:foreground nil :weight normal :inherit outline-3))))
   `(gnus-group-mail-4 ((t (:foreground nil :weight normal :inherit outline-4))))
   `(gnus-group-mail-5 ((t (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-mail-6 ((t (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-mail-1 :foreground ,base04))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-mail-2 :foreground ,base04))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-mail-3 :foreground ,base04))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-mail-4 :foreground ,base04))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-mail-5 :foreground ,base04))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-mail-6 :foreground ,base04))))
   `(gnus-group-news-1 ((t (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-news-2 ((t (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-news-3 ((t (:foreground nil :weight normal :inherit outline-7))))
   `(gnus-group-news-4 ((t (:foreground nil :weight normal :inherit outline-8))))
   `(gnus-group-news-5 ((t (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-news-6 ((t (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-news-1-empty ((t (:inherit gnus-group-news-1 :foreground ,base04))))
   `(gnus-group-news-2-empty ((t (:inherit gnus-group-news-2 :foreground ,base04))))
   `(gnus-group-news-3-empty ((t (:inherit gnus-group-news-3 :foreground ,base04))))
   `(gnus-group-news-4-empty ((t (:inherit gnus-group-news-4 :foreground ,base04))))
   `(gnus-group-news-5-empty ((t (:inherit gnus-group-news-5 :foreground ,base04))))
   `(gnus-group-news-6-empty ((t (:inherit gnus-group-news-6 :foreground ,base04))))

   `(erc-direct-msg-face ((t (:foreground ,onc-blue))))
   `(erc-error-face ((t (:foreground ,onc-red))))
   `(erc-header-face ((t (:foreground ,base06 :background ,base04))))
   `(erc-input-face ((t (:foreground ,onc-green))))
   `(erc-keyword-face ((t (:foreground ,base0A))))
   `(erc-current-nick-face ((t (:foreground ,onc-green))))
   `(erc-my-nick-face ((t (:foreground ,onc-green))))
   `(erc-nick-default-face ((t (:weight normal :foreground ,onc-yellow))))
   `(erc-nick-msg-face ((t (:weight normal :foreground ,base0A))))
   `(erc-notice-face ((t (:foreground ,base04))))
   `(erc-pal-face ((t (:foreground ,onc-blue))))
   `(erc-prompt-face ((t (:foreground ,onc-orange))))
   `(erc-timestamp-face ((t (:foreground ,onc-cyan))))

   ;; helm
   `(helm-M-x-key ((t (:foreground ,onc-cyan))))
   `(helm-action ((t (:foreground ,base05))))
   `(helm-buffer-directory ((t (:foreground ,base04 :background nil :weight bold))))
   `(helm-buffer-file ((t (:foreground ,onc-cyan))))
   `(helm-buffer-not-saved ((t (:foreground ,onc-red))))
   `(helm-buffer-process ((t (:foreground ,base03))))
   `(helm-buffer-saved-out ((t (:foreground ,onc-brown))))
   `(helm-buffer-size ((t (:foreground ,onc-blue))))
   `(helm-candidate-number ((t (:foreground ,onc-black :background ,onc-blue))))
   `(helm-ff-directory ((t (:foreground ,base04 :background nil :weight bold))))
   `(helm-ff-executable ((t (:foreground ,onc-green))))
   `(helm-ff-file ((t (:foreground ,onc-cyan))))
   `(helm-ff-invalid-symlink ((t (:foreground ,onc-black :background ,onc-red))))
   `(helm-ff-prefix ((t (:foreground nil :background nil))))
   `(helm-ff-symlink ((t (:foreground ,onc-black :background ,onc-cyan))))
   `(helm-grep-cmd-line ((t (:foreground ,onc-green))))
   `(helm-grep-file ((t (:foreground ,onc-cyan))))
   `(helm-grep-finish ((t (:foreground ,onc-black :background ,onc-blue))))
   `(helm-grep-lineno ((t (:foreground ,base03))))
   `(helm-grep-match ((t (:foreground ,base0A))))
   `(helm-grep-running ((t (:foreground ,onc-blue))))
   `(helm-header ((t (:foreground ,base0A :background ,onc-black :underline nil))))
   `(helm-match ((t (:foreground ,base0A))))
   `(helm-moccur-buffer ((t (:foreground ,onc-cyan))))
   `(helm-selection ((t (:foreground nil :background ,base02 :underline nil))))
   `(helm-selection-line ((t (:foreground nil :background ,base02))))
   `(helm-separator ((t (:foreground ,base02))))
   `(helm-source-header ((t (:foreground ,base05 :background ,onc-dark-grey :weight bold))))
   `(helm-visible-mark ((t (:foreground ,onc-black :background ,onc-green))))

   `(custom-variable-tag ((t (:foreground ,onc-orange))))
   `(custom-group-tag ((t (:foreground ,onc-orange))))
   `(custom-state ((t (:foreground ,onc-green)))))


  (custom-theme-set-variables
   'base16-onc-dark

   `(ansi-color-names-vector
     ;; black, onc-red, onc-green, base0A, onc-orange, magenta, cyan, white
     [,onc-black ,onc-red ,onc-green ,base0A ,onc-orange ,onc-yellow ,onc-orange ,base05])
   `(ansi-term-color-vector
     ;; black, onc-red, onc-green, base0A, onc-orange, magenta, cyan, white
     [unspecified ,onc-black ,onc-red ,onc-green ,base0A ,onc-orange ,onc-yellow ,onc-orange ,base05])))

(provide-theme 'base16-onc-dark)

;;; base16-onc-dark-theme.el ends here
