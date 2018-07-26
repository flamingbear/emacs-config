(deftheme ample-flat-plus
  "Created 2018-02-25.")

(custom-theme-set-variables
 'ample-flat-plus
 '(ansi-color-names-vector ["#505050" "#cd5c5c" "#a9df90" "#cdad00" "#91a0b3" "#ab85a3" "#afcfef" "#bdbdb3"]))

(custom-theme-set-faces
 'ample-flat-plus
 '(cursor ((t (:foreground "gray15" :background "#afffef"))))
 '(fringe ((t (:background "#262424"))))
 '(link ((t (:foreground "#afcfef" :underline t))))
 '(region ((t (:background "#505050"))))
 '(font-lock-builtin-face ((t (:foreground "#9fbfdf"))))
 '(font-lock-comment-face ((t (:foreground "#857575"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#706565"))))
 '(font-lock-function-name-face ((t (:foreground "#a9df90"))))
 '(font-lock-keyword-face ((t (:foreground "#91a0b3"))))
 '(font-lock-string-face ((t (:foreground "#ddbc91"))))
 '(font-lock-preprocessor-face ((t (:foreground "#cF9570"))))
 '(font-lock-type-face ((t (:foreground "#cd5c5c"))))
 '(font-lock-constant-face ((t (:foreground "#ab85a3"))))
 '(font-lock-warning-face ((t (:foreground "red" :bold t))))
 '(font-lock-variable-name-face ((t (:foreground "#cdad00"))))
 '(font-lock-doc-face ((t (:foreground "#7c7565"))))
 '(mode-line-inactive ((t (:background "#504545" :foreground "cornsilk4"))))
 '(mode-line ((t (:background "cornsilk4" :foreground "#302525"))))
 '(popup-tip-face ((t (:background "#bdbdb3" :foreground "gray15"))))
 '(header-line ((t (:background "#bdbdb3" :foreground "gray15"))))
 '(button ((t (:foreground "#afcfef" :background nil :underline t))))
 '(isearch ((t (:background "#91a0b3" :foreground "gray15"))))
 '(lazy-highlight ((t (:background "gray15" :foreground "#ab85a3" :underline t))))
 '(ace-jump-face-background ((t (:inherit font-lock-comment-face))))
 '(ace-jump-face-foreground ((t (:foreground "#cF9570"))))
 '(vertical-border ((t (:background "#504545" :foreground "#302525"))))
 '(minibuffer-prompt ((t (:foreground "#caca86" :bold t :background nil))))
 '(compilation-error ((t (:foreground "#cd5c5c" :bold t))))
 '(compilation-warning ((t (:foreground "#cF9570" :bold t))))
 '(compilation-info ((t (:foreground "#a9df90" :bold t))))
 '(comint-highlight-prompt ((t (:foreground "#a9df90"))))
 '(show-paren-match ((t (:foreground nil :background "#706565"))))
 '(show-paren-mismatch ((t (:inherit error))))
 '(error ((t (:foreground "red"))))
 '(ido-only-match ((t (:foreground "#a9df90"))))
 '(ido-first-match ((t (:foreground "#91a0b3"))))
 '(ido-incomplete-regexp ((t (:foreground "#cd5c5c"))))
 '(ido-subdir ((t (:foreground "#cdad00"))))
 '(js2-external-variable ((t (:foreground "#cF9570" :background nil))))
 '(js2-function-param ((t (:foreground "#7ccd7c" :background nil))))
 '(js2-instance-member ((t (:foreground "#ab85a3" :background nil))))
 '(js2-jsdoc-html-tag-delimiter ((t (:foreground "#706565" :background nil))))
 '(js2-jsdoc-html-tag-name ((t (:foreground "#801545" :background nil))))
 '(js2-jsdoc-tag ((t (:foreground "#8a0f00" :background nil))))
 '(js2-jsdoc-type ((t (:foreground "#cd5c5c" :background nil))))
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
 '(company-tooltip-common ((t (:foreground "#8a0f00" :background "#bdbdb3"))))
 '(company-tooltip-common-selection ((t (:foreground "gray15" :background "#91a0b3"))))
 '(company-tooltip-mouse ((t (:foreground nil :background "#9fbfdf"))))
 '(company-tooltip-selection ((t (:foreground "#706565" :background "#91a0b3"))))
 '(diff-added ((t (:background nil :foreground "#a9df90"))))
 '(diff-changed ((t (:background nil :foreground "#cdad00"))))
 '(diff-removed ((t (:background nil :foreground "#cd5c5c"))))
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
 '(org-todo ((t (:foreground "#cd5c5c" :background nil))))
 '(org-hide ((t (:foreground "gray15" :background nil))))
 '(message-cited-text ((t (:inherit font-lock-comment-face))))
 '(message-header-cc ((t (:foreground "#9fbfdf" :background nil :bold t))))
 '(message-header-name ((t (:foreground "#cF9570" :background nil))))
 '(message-header-newsgroups ((t (:foreground "#7c7565" :background nil :bold t))))
 '(message-header-other ((t (:foreground "#91a0b3" :background nil))))
 '(message-header-subject ((t (:foreground "#ddbc91" :background nil))))
 '(message-header-to ((t (:foreground "#cdad00" :background nil :bold t))))
 '(message-header-xheader ((t (:foreground "#ab85a3" :background nil))))
 '(message-mml ((t (:foreground "#7c7565" :background nil))))
 '(gnus-group-mail-1 ((t (:foreground "#91a0b3" :background nil :bold t))))
 '(gnus-group-mail-1-empty ((t (:foreground "#91a0b3" :background nil))))
 '(gnus-group-mail-2 ((t (:foreground "#afcfef" :background nil :bold t))))
 '(gnus-group-mail-2-empty ((t (:foreground "#afcfef" :background nil))))
 '(gnus-group-mail-3 ((t (:foreground "#9fbfdf" :background nil :bold t))))
 '(gnus-group-mail-3-empty ((t (:foreground "#9fbfdf" :background nil))))
 '(gnus-group-mail-low ((t (:foreground "#cdad00" :background nil :bold t))))
 '(gnus-group-mail-low-empty ((t (:foreground "#cdad00" :background nil))))
 '(gnus-group-news-1 ((t (:foreground "PaleTurquoise" :background nil :bold t))))
 '(gnus-group-news-1-empty ((t (:foreground "PaleTurquoise" :background nil))))
 '(gnus-group-news-2 ((t (:foreground "turquoise" :background nil :bold t))))
 '(gnus-group-news-2-empty ((t (:foreground "turquoise" :background nil))))
 '(gnus-group-news-3 ((t (:foreground nil :background nil :bold t))))
 '(gnus-group-news-3-empty ((t (:foreground nil :background nil))))
 '(gnus-group-news-4 ((t (:foreground nil :background nil :bold t))))
 '(gnus-group-news-4-empty ((t (:foreground nil :background nil))))
 '(gnus-group-news-5 ((t (:foreground nil :background nil :bold t))))
 '(gnus-group-news-5-empty ((t (:foreground nil :background nil))))
 '(gnus-group-news-6 ((t (:foreground nil :background nil :bold t))))
 '(gnus-group-news-6-empty ((t (:foreground nil :background nil))))
 '(gnus-group-news-low ((t (:foreground "DarkTurquoise" :background nil :bold t))))
 '(gnus-group-news-low-empty ((t (:foreground "DarkTurquoise" :background nil))))
 '(gnus-splash ((t (:foreground "#cccccc" :background nil))))
 '(gnus-summary-cancelled ((t (:foreground "yellow" :background "black"))))
 '(gnus-summary-high-ancient ((t (:foreground "SkyBlue" :background nil :bold t))))
 '(gnus-summary-high-read ((t (:foreground "PaleGreen" :background nil :bold t))))
 '(gnus-summary-high-ticked ((t (:foreground "pink" :background nil :bold t))))
 '(gnus-summary-high-undownloaded ((t (:foreground "LightGray" :background nil :bold t))))
 '(gnus-summary-high-unread ((t (:foreground nil :background nil :bold t))))
 '(gnus-summary-low-ancient ((t (:foreground "SkyBlue" :background nil))))
 '(gnus-summary-low-read ((t (:foreground "PaleGreen" :background nil))))
 '(gnus-summary-low-ticked ((t (:foreground "pink" :background nil))))
 '(gnus-summary-low-undownloaded ((t (:foreground "LightGray" :background nil))))
 '(gnus-summary-low-unread ((t (:foreground nil :background nil))))
 '(gnus-summary-normal-ancient ((t (:inherit default))))
 '(gnus-summary-normal-read ((t (:foreground "#a9df90" :background nil))))
 '(gnus-summary-normal-ticked ((t (:foreground "#cF9570" :background nil))))
 '(gnus-summary-normal-undownloaded ((t (:foreground "#706565" :background nil))))
 '(gnus-summary-normal-unread ((t (:foreground "#9fbfdf" :background nil))))
 '(gnus-summary-selected ((t (:foreground nil :background nil :underline t))))
 '(magit-section-heading ((t (:foreground "#91a0b3" :background nil))))
 '(magit-hash ((t (:foreground "#ab85a3" :background nil))))
 '(magit-branch-local ((t (:foreground "#cF9570" :background nil))))
 '(magit-branch-remote ((t (:foreground "#cdad00" :background nil))))
 '(magit-diff-added-highlight ((t (:background "#343030" :foreground "#a9df90"))))
 '(magit-diff-removed-highlight ((t (:background "#343030" :foreground "#cd5c5c"))))
 '(magit-diff-added ((t (:background nil :foreground "#a9df90"))))
 '(magit-diff-removed ((t (:background nil :foreground "#cd5c5c"))))
 '(magit-blame-date ((t (:foreground "#ab85a3" :background "grey25"))))
 '(magit-blame-hash ((t (:foreground "#ab85a3" :background "grey25"))))
 '(magit-blame-heading ((t (:foreground "#91a0b3" :background "grey25"))))
 '(magit-blame-name ((t (:foreground "#a9df90" :background "grey25"))))
 '(magit-blame-summary ((t (:foreground "#91a0b3" :background "grey25"))))
 '(magit-log-author ((t (:foreground "#cd5c5c" :background nil))))
 '(magit-log-date ((t (:foreground nil :background nil))))
 '(magit-log-graph ((t (:foreground "grey80" :background nil))))
 '(magit-tag ((t (:foreground "#91a0b3" :background nil))))
 '(git-gutter:deleted ((t (:foreground "#cd5c5c" :background nil :bold t))))
 '(git-gutter:modified ((t (:foreground "#ab85a3" :background nil :bold t))))
 '(git-gutter:separator ((t (:foreground "#cF9570" :background nil :bold t))))
 '(git-gutter:unchanged ((t (:foreground "#cdad00" :background nil))))
 '(highlight-indentation-current-column-face ((t (:foreground nil :background "#505050"))))
 '(highlight-indentation-face ((t (:foreground nil :background "#303030"))))
 '(trailing-whitespace ((t (:background "white" :bold t))))
 '(custom-button ((t (:foreground nil :background nil))))
 '(custom-button-mouse ((t (:foreground nil :background nil))))
 '(custom-button-pressed ((t (:foreground nil :background nil))))
 '(custom-button-pressed-unraised ((t (:foreground "#ab85a3" :background nil))))
 '(custom-button-unraised ((t (:foreground nil :background nil))))
 '(custom-changed ((t (:foreground "#cd5c5c" :background nil))))
 '(custom-comment ((t (:foreground "gray15" :background "#cdad00"))))
 '(custom-comment-tag ((t (:foreground "#bdbdb3" :background nil))))
 '(custom-documentation ((t (:foreground nil :background nil))))
 '(custom-face-tag ((t (:foreground "#9fbfdf" :background nil))))
 '(custom-group-subtitle ((t (:foreground nil :background nil :bold t))))
 '(custom-group-tag ((t (:foreground "#9fbfdf" :background nil :bold t))))
 '(custom-group-tag-1 ((t (:foreground "#cdad00" :background nil :bold t))))
 '(custom-invalid ((t (:foreground "gray15" :background "#cd5c5c"))))
 '(custom-link ((t (:inherit button))))
 '(custom-modified ((t (:foreground "#cd5c5c" :background nil))))
 '(custom-rogue ((t (:foreground "#cdad00" :background "gray15"))))
 '(custom-saved ((t (:foreground nil :background nil :underline t))))
 '(custom-set ((t (:foreground "#bdbdb3" :background "#706565"))))
 '(custom-state ((t (:foreground "#a9df90" :background nil))))
 '(custom-themed ((t (:foreground "#cd5c5c" :background nil))))
 '(custom-variable-button ((t (:foreground nil :background nil :underline t :bold t))))
 '(custom-variable-tag ((t (:foreground "#9fbfdf" :background nil :bold t))))
 '(custom-visibility ((t (:inherit button))))
 '(widget-field ((t (:foreground "#bdbdb3" :background "#706565"))))
 '(default ((t (:foreground "#bdbdb3" :background "gray15"))))
 '(org-level-1 ((t (:inherit (outline-1)))))
 '(org-level-2 ((t (:inherit (outline-2)))))
 '(org-level-3 ((t (:inherit (outline-3)))))
 '(org-level-4 ((t (:inherit (outline-4)))))
 '(org-level-5 ((t (:inherit (outline-5)))))
 '(org-level-6 ((t (:inherit (outline-6)))))
 '(org-level-7 ((t (:inherit (outline-7))))))

(provide-theme 'ample-flat-plus)
