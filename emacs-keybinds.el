;; File for keybindings for emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [M-left]  'backward-word)
(global-set-key [M-right] 'forward-word)
(global-set-key [M-up]    'beginning-of-line)
(global-set-key [M-down]  'end-of-line)

(global-set-key [home]       'beginning-of-line)
(global-set-key [end]              'end-of-line)
(global-set-key [\C-home]  'beginning-of-buffer)
(global-set-key [\C-end]         'end-of-buffer)


(defun mhs-scroll-right-slowly()
  (interactive)
  (scroll-right '4))

(defun mhs-scroll-left-slowly()
  (interactive)
  (scroll-left '4))


(global-set-key [f1]          'mhs-insert-filename); F1
(global-set-key [f2]                       'shell); F2
(global-set-key [f3]                     'speedbar); F3
(global-set-key [s-f2]               'scroll-right); Windows-F2
(global-set-key [s-f3]                'scroll-left); Windows-F3
(global-set-key [f4]      'mhs-scroll-right-slowly); F4
(global-set-key [f5]       'mhs-scroll-left-slowly); F5


(global-set-key [f6]              'gud-break) ; F7
(global-set-key [M-f6]            'gud-remove) ; F7




;; My Mapping Stuff, this is a prefix key that allows me access to more
;; functions.  These are defined in ~/lisp/mhs-map.el
(global-set-key [f7]              'mhs-map) ; F7

;; This is so that I can get used to copy-paste in eclipse with mulgaSoft emacs+
(global-set-key (kbd "C-x x") 'copy-to-register)
(global-set-key (kbd "C-x g") 'insert-register)



(global-set-key [f8]              'force-space) ; F8
(global-set-key [f9]              'gud-step) ; F9
(global-set-key [f10]             'gud-refresh) ; F10
(global-set-key [f11]             'gud-print) ; Stop
(global-set-key [f12]             'repeat-complex-command) ; F12

;; These are pretty hard to reach function keys
(global-set-key [f13]             'other-frame) ; props
(global-set-key [f20]             'split-window-vertically) ; cut
(global-set-key [f18]             'delete-other-windows) ; paste
(global-set-key [f19]             'switch-to-buffer) ; find

;; save your fingers again
(global-set-key [f16] 'execute-extended-command)   ;Copy

(global-unset-key [mouse-2])

;; C-c [a-zA-Z] are saved for user's options so use these to define
;; shortcuts.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-cc"                         'compile); C-c c
(global-set-key "\C-c;"                  'comment-region); C-c;
(global-set-key "\C-co"                     'other-frame); C-c o
(global-set-key "\C-cL"                      'mhs-lineup); C-c l
(global-set-key "\C-cs"                 'mhs-shift-lines); C-c s
(global-set-key "\C-cu"     'mhs-underline-comment-maybe); C-c u
(global-set-key "\C-cw"  'mhs-close-whitespace-rectangle); C-c w
(global-set-key "\C-cd"                    'mhs-dblstuff); C-c d
(global-set-key "\C-ct"                 'mhs-trunc-lines); C-c t
(global-set-key "\C-ck"                'mhs-kill-current); C-c k
(global-set-key "\C-cg"                       'goto-line); C-c g
(global-set-key "\C-ci"              'mhs-include-guards); C-c i

(global-set-key "\C-c\'"               'mhs-squote-region)
(global-set-key "\C-c\""               'mhs-double-quote-around-region)
(global-set-key "\C-c("                'mhs-paren-region)
(global-set-key "\C-c)"                'mhs-paren-region)
(global-set-key "\C-c{"                'mhs-bracket-region)
(global-set-key "\C-c}"                'mhs-bracket-region)
(global-set-key "\C-c["                'mhs-sqbracket-region)
(global-set-key "\C-c]"                'mhs-sqbracket-region)
(global-set-key "\C-c<"                'mhs-anglebracket-region)
(global-set-key "\C-c>"                'mhs-anglebracket-region)
(global-set-key (kbd "C-c p") 'perldb)
(global-set-key (kbd "C-x SPC") 'push-mark-command)





;; Stuff from EmacsRocks guy Magnars.

;; Use the fancy rgrep if available. from magnars
(if (try-require 'setup-rgrep)
    (global-set-key (kbd "M-s s") 'rgrep-fullscreen)
  (global-set-key (kbd "M-s s") 'rgrep))


(when (try-require 'inline-string-rectangle)
  (global-set-key (kbd "C-x r t") 'inline-string-rectangle))

;; TODO [MHS, 2012-10-14] Not using mark-multiple anymore.  But will keep this here for a while.
;; I haven't decided if inline-string-rectangle is still useful and I need mark-multiple to have it work
;; (when (try-require 'mark-more-like-this)
;;   (global-set-key (kbd "C-<") 'mark-previous-like-this)
;;   (global-set-key (kbd "C->") 'mark-next-like-this)
;;   (global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
;;   (global-set-key (kbd "C-*") 'mark-all-like-this))

(when (try-require 'multiple-cursors)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

(when (boundp 'mhs-searchmap)
  (define-key mhs-searchmap "s" 'rgrep-fullscreen))


;; Ace Jumping mode.
(when (try-require 'ace-jump-mode)
  (define-key global-map (kbd "C-c x") 'ace-jump-mode))


;; Find-name-dired
(global-set-key (kbd "M-s f") 'find-name-dired)


;; Cyclical Marking
;------------------
(when (try-require 'thing-cmds)
  (global-set-key (kbd "C-M-?") 'mark-thing) ; vs `mark-sexp'
  (global-set-key (kbd "M-@") 'cycle-thing-region)) ; vs `mark-word'


;; Options for Macintosh Laptop
;;------------------------------
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch C-h a from command-apropos to apropos This is so you have every
;; option that matches your string when you call it.
(define-key help-map "a" 'apropos)
