(deftheme ample-flat
  "Created 2017-12-10.")

(custom-theme-set-variables
 'ample-flat
 '(ansi-color-names-vector ["#504545" "#ad8572" "#a9df90" "#aaca86" "#91a0b3" "#ab85a3" "#afcfef" "#bdbdb3"]))

(custom-theme-set-faces
 'ample-flat
 '(cursor ((t (:foreground "gray15" :background "#afffef"))))
 '(fringe ((t (:background "#262424"))))
 '(link ((t (:foreground "#afcfef" :underline t))))
 '(region ((t (:background "#343030"))))
 '(font-lock-builtin-face ((t (:foreground "#9fbfdf"))))
 '(font-lock-comment-face ((t (:foreground "#857575"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#706565"))))
 '(font-lock-function-name-face ((t (:foreground "#a9df90"))))
 '(font-lock-keyword-face ((t (:foreground "#91a0b3"))))
 '(font-lock-string-face ((t (:foreground "#ddbc91"))))
 '(font-lock-preprocessor-face ((t (:foreground "#cF9570"))))
 '(font-lock-type-face ((t (:foreground "#ad8572"))))
 '(font-lock-constant-face ((t (:foreground "#ab85a3"))))
 '(font-lock-warning-face ((t (:foreground "red" :bold t))))
 '(font-lock-variable-name-face ((t (:foreground "#aaca86"))))
 '(font-lock-doc-face ((t (:foreground "#7c7565"))))
 '(mode-line-inactive ((t (:background "#504545" :foreground "cornsilk4"))))
 '(mode-line ((t (:background "cornsilk4" :foreground "#302525"))))
 '(header-line ((t (:background "#bdbdb3" :foreground "gray15"))))
 '(button ((t (:foreground "#afcfef" :background nil :underline t))))
 '(isearch ((t (:background "#91a0b3" :foreground "gray15"))))
 '(lazy-highlight ((t (:background "gray15" :foreground "#ab85a3" :underline t))))
 '(ace-jump-face-background ((t (:inherit font-lock-comment-face))))
 '(ace-jump-face-foreground ((t (:foreground "#cF9570"))))
 '(vertical-border ((t (:background "#504545" :foreground "#302525"))))
 '(minibuffer-prompt ((t (:foreground "#caca86" :bold t :background nil))))
 '(compilation-error ((t (:foreground "#ad8572" :bold t))))
 '(compilation-warning ((t (:foreground "#cF9570" :bold t))))
 '(compilation-info ((t (:foreground "#a9df90" :bold t))))
 '(comint-highlight-prompt ((t (:foreground "#a9df90"))))
 '(show-paren-match ((t (:foreground nil :background "#706565"))))
 '(show-paren-mismatch ((t (:inherit error))))
 '(error ((t (:foreground "red"))))
 '(ido-only-match ((t (:foreground "#a9df90"))))
 '(ido-first-match ((t (:foreground "#91a0b3"))))
 '(ido-incomplete-regexp ((t (:foreground "#ad8572"))))
 '(ido-subdir ((t (:foreground "#aaca86"))))
 '(js2-external-variable ((t (:foreground "#cF9570" :background nil))))
 '(js2-function-param ((t (:foreground "#596f50" :background nil))))
 '(js2-instance-member ((t (:foreground "#ab85a3" :background nil))))
 '(js2-jsdoc-html-tag-delimiter ((t (:foreground "#706565" :background nil))))
 '(js2-jsdoc-html-tag-name ((t (:foreground "#302525" :background nil))))
 '(js2-jsdoc-tag ((t (:foreground "#8d6552" :background nil))))
 '(js2-jsdoc-type ((t (:foreground "#ad8572" :background nil))))
 '(js2-jsdoc-value ((t (:foreground "#ddbc91" :background nil))))
 '(js2-private-function-call ((t (:foreground "#596f50" :background nil))))
 '(js2-private-member ((t (:foreground "#7c7565" :background nil))))
 '(js2-warning ((t (:foreground nil :background nil :underline "#cF9570"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#b1b0e3" :background nil))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#a58585" :background nil))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#9190a3" :background nil))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#b59585" :background nil))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#7180a3" :background nil))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#957565" :background nil))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#b1b0e3" :background nil))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#a58585" :background nil))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#9190a3" :background nil))))
 '(rainbow-delimiters-unmatched-face ((t (:inherit error))))
 '(company-preview-common ((t (:inherit font-lock-comment-face))))
 '(company-scrollbar-bg ((t (:foreground nil :background "#302525"))))
 '(company-scrollbar-fg ((t (:foreground nil :background "#706565"))))
 '(company-tooltip ((t (:foreground "gray15" :background "#bdbdb3"))))
 '(company-tooltip-common ((t (:foreground "#8d6552" :background "#bdbdb3"))))
 '(company-tooltip-common-selection ((t (:foreground "gray15" :background "#91a0b3"))))
 '(company-tooltip-mouse ((t (:foreground nil :background "#9fbfdf"))))
 '(company-tooltip-selection ((t (:foreground "#706565" :background "#91a0b3"))))
 '(diff-added ((t (:background nil :foreground "#a9df90"))))
 '(diff-changed ((t (:background nil :foreground "#aaca86"))))
 '(diff-removed ((t (:background nil :foreground "#ad8572"))))
 '(diff-context ((t (:foreground "#857575" :background nil))))
 '(diff-file-header ((t (:foreground "gray15" :background "grey60" :bold t))))
 '(diff-function ((t (:foreground "gray15" :background "grey50"))))
 '(diff-header ((t (:foreground "gray15" :background "grey50"))))
 '(diff-hunk-header ((t (:foreground "gray15" :background "grey50"))))
 '(diff-index ((t (:foreground "gray15" :background "grey50"))))
 '(diff-indicator-added ((t (:inherit diff-added))))
 '(diff-indicator-changed ((t (:inherit diff-changed))))
 '(diff-indicator-removed ((t (:inherit diff-removed))))
 '(diff-nonexistent ((t (:foreground nil :background "grey70"))))
 '(diff-refine-added ((t (:foreground nil :background "#649694"))))
 '(diff-refine-changed ((t (:foreground nil :background "#8f8f40"))))
 '(diff-refine-removed ((t (:foreground nil :background "#694949"))))
 '(org-done ((t (:foreground "#a9df90" :background nil))))
 '(org-todo ((t (:foreground "#ad8572" :background nil))))
 '(org-hide ((t (:foreground "gray15" :background nil))))
 '(message-cited-text ((t (:inherit font-lock-comment-face))))
 '(message-header-cc ((t (:foreground "#9fbfdf" :background nil :bold t))))
 '(message-header-name ((t (:foreground "#cF9570" :background nil))))
 '(message-header-newsgroups ((t (:foreground "#7c7565" :background nil :bold t))))
 '(message-header-other ((t (:foreground "#91a0b3" :background nil))))
 '(message-header-subject ((t (:foreground "#ddbc91" :background nil))))
 '(message-header-to ((t (:foreground "#aaca86" :background nil :bold t))))
 '(message-header-xheader ((t (:foreground "#ab85a3" :background nil))))
 '(message-mml ((t (:foreground "#7c7565" :background nil))))
 '(magit-section-heading ((t (:foreground "#91a0b3" :background nil))))
 '(magit-hash ((t (:foreground "#ab85a3" :background nil))))
 '(magit-branch-local ((t (:foreground "#cF9570" :background nil))))
 '(magit-branch-remote ((t (:foreground "#aaca86" :background nil))))
 '(magit-diff-added-highlight ((t (:background "#343030" :foreground "#a9df90"))))
 '(magit-diff-removed-highlight ((t (:background "#343030" :foreground "#ad8572"))))
 '(magit-diff-added ((t (:background nil :foreground "#a9df90"))))
 '(magit-diff-removed ((t (:background nil :foreground "#ad8572"))))
 '(magit-blame-date ((t (:foreground "#ab85a3" :background "grey25"))))
 '(magit-blame-hash ((t (:foreground "#ab85a3" :background "grey25"))))
 '(magit-blame-heading ((t (:foreground "#91a0b3" :background "grey25"))))
 '(magit-blame-name ((t (:foreground "#a9df90" :background "grey25"))))
 '(magit-blame-summary ((t (:foreground "#91a0b3" :background "grey25"))))
 '(magit-log-author ((t (:foreground "#ad8572" :background nil))))
 '(magit-log-date ((t (:foreground nil :background nil))))
 '(magit-log-graph ((t (:foreground "grey80" :background nil))))
 '(magit-tag ((t (:foreground "#91a0b3" :background nil))))
 '(highlight-indentation-current-column-face ((t (:foreground nil :background "#857575"))))
 '(highlight-indentation-face ((t (:foreground nil :background "gray30"))))
 '(trailing-whitespace ((t (:background "white" :bold t))))
 '(custom-button ((t (:foreground nil :background nil))))
 '(custom-button-mouse ((t (:foreground nil :background nil))))
 '(custom-button-pressed ((t (:foreground nil :background nil))))
 '(custom-button-pressed-unraised ((t (:foreground "#ab85a3" :background nil))))
 '(custom-button-unraised ((t (:foreground nil :background nil))))
 '(custom-changed ((t (:foreground "#ad8572" :background nil))))
 '(custom-comment ((t (:foreground "gray15" :background "#aaca86"))))
 '(custom-comment-tag ((t (:foreground "#bdbdb3" :background nil))))
 '(custom-documentation ((t (:foreground nil :background nil))))
 '(custom-face-tag ((t (:foreground "#9fbfdf" :background nil))))
 '(custom-group-subtitle ((t (:foreground nil :background nil :bold t))))
 '(custom-group-tag ((t (:foreground "#9fbfdf" :background nil :bold t))))
 '(custom-group-tag-1 ((t (:foreground "#aaca86" :background nil :bold t))))
 '(custom-invalid ((t (:foreground "gray15" :background "#ad8572"))))
 '(custom-link ((t (:inherit button))))
 '(custom-modified ((t (:foreground "#ad8572" :background nil))))
 '(custom-rogue ((t (:foreground "#aaca86" :background "gray15"))))
 '(custom-saved ((t (:foreground nil :background nil :underline t))))
 '(custom-set ((t (:foreground "#bdbdb3" :background "#706565"))))
 '(custom-state ((t (:foreground "#a9df90" :background nil))))
 '(custom-themed ((t (:foreground "#ad8572" :background nil))))
 '(custom-variable-button ((t (:foreground nil :background nil :underline t :bold t))))
 '(custom-variable-tag ((t (:foreground "#9fbfdf" :background nil :bold t))))
 '(custom-visibility ((t (:inherit button))))
 '(widget-field ((t (:foreground "#bdbdb3" :background "#706565"))))
 '(default ((t (:foreground "#bdbdb3" :background "gray15")))))

(provide-theme 'ample-flat)
