;; File for keybindings for emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-unset-key (kbd "C-z"))    	;; suspend-frame is crashy on osx.
;;;;;;;;
;; try swiper/ivy/counsel
;;;;;;;

;; [MHS, 04/12/2023] These are muscle memoried.
(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "C-c F") 'projectile-find-file-in-known-projects)
(global-set-key (kbd "C-c j") 'consult-git-grep)


;; This seems way more natural to me. (isearch)
(global-set-key "\C-s" 'isearch-forward)
(global-set-key "\C-r" 'isearch-backward)
;; https://oremacs.com/2019/04/07/swiper-isearch/
;; (global-set-key "\C-s" 'modi/swiper-isearch)
;; (global-set-key "\C-r" 'modi/swiper-isearch)
;; (define-key isearch-mode-map (kbd "M-i") 'swiper-from-isearch)

(global-set-key (kbd "M-/") 'dabbrev-expand)

(global-set-key [f1]          'mhs-insert-filename); F1
(global-set-key [f2]          'visit-vterm-buffer)  ; F2


;; My Mapping Stuff, this is a prefix key that allows me access to more
;; functions.  These are defined in ~/lisp/mhs-map.el
(global-set-key [f8]              'mhs-map) ; F7

;; This is so that I can get used to copy-paste in eclipse with mulgaSoft emacs+
;; (global-set-key (kbd "C-x x") 'copy-to-register)
;; (global-set-key (kbd "C-x g") 'insert-register)

;; This adds the function of being able to paste into other windows.
(global-set-key (kbd "M-w") 'clipboard-kill-ring-save)


(global-set-key [f9]              'gud-step) ; F9
(global-set-key [f10]             'gud-refresh) ; F10
(global-set-key [f11]             'gud-print) ; Stop
(global-set-key [f12]             'repeat-complex-command) ; F12


;; Don't like zap to character.
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

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
(global-set-key "\C-cg"                    'magit-status); C-c g


(global-set-key "\C-c\`"               'mhs-backtick-around-region)
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

(global-set-key (kbd "C-x SPC") 'push-mark-command)


;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; https://www.reddit.com/r/emacs/comments/gf64oq/comment/fprm9nn/?utm_source=share&utm_medium=web2x&context=3
(defun isearch-mark-and-exit+ ()
  (interactive)
  (isearch-done)
  (push-mark isearch-other-end 'no-message 'activate))
(define-key isearch-mode-map (kbd "M-RET") 'isearch-mark-and-exit+)

(use-package hydra :ensure t)
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (defhydra multiple-cursors-hydra (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil))
  (define-key mhs-map [(f9)] 'multiple-cursors-hydra/body))

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-c x") 'avy-goto-word-or-subword-1))

(use-package ace-window
  ;; https://github.com/abo-abo/ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (global-set-key (kbd "M-O") 'aw-flip-window)
  (global-set-key (kbd "C-x o") 'other-window) ;; not ready for all acey yet.
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; aw-dispatch-always Must be true if you want to use fancy changing actions midway.
  (setq aw-dispatch-always 'nil)
  )




;; Options for Macintosh Laptop
;;------------------------------
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch C-h a from command-apropos to apropos This is so you have every
;; option that matches your string when you call it.
(define-key help-map "a" 'apropos)



(provide 'emacs-keybinds)
