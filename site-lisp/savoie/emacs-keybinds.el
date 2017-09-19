;; File for keybindings for emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-unset-key (kbd "C-z"))    	;; suspend-frame is crashy on osx.
;;;;;;;;
;; try swiper/ivy/counsel
;;;;;;;
(use-package swiper  :ensure t)
(use-package counsel
  :ensure t
  :bind (("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("M-x" . counsel-M-x)
         ("C-c f" . counsel-git)
         ("C-c j" . counsel-git-grep)))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
	 :map ivy-minibuffer-map
	 ("C-w" . ivy-yank-word)))


;;; Get symbol at point
;; https://github.com/Wilfred/ag.el
(defun modi/get-symbol-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))


;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-search.el
(defun modi/swiper (arg)
  "Start swiper with input as the selected region.
If a region is not selected and,
  - If ARG is nil, start swiper with the symbol at point as input.
  - Elseswiper without any arguments (stock behavior)."
  (interactive "P")
  (if (use-region-p)
      (let ((b (region-beginning))
            (e (region-end)))
        (deactivate-mark)
        (swiper (buffer-substring-no-properties b e)))
    (if arg
        (swiper (modi/get-symbol-at-point)) ; C-u
        (swiper))))

;; This seems way more natural to me. (isearch)
(global-set-key "\C-s" 'isearch-forward)
;(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'isearch-backward)
;(global-set-key "\C-r" 'swiper)
(define-key isearch-mode-map (kbd "M-i") 'swiper-from-isearch)

(global-set-key (kbd "M-/") 'dabbrev-expand)


(global-set-key [f1]          'mhs-insert-filename); F1
(global-set-key [f2]                       'shell); F2
;;(global-set-key [f3]                     'speedbar); F3
;;(global-set-key [f4]      '); F4
;;(global-set-key [f5]       '); F5
;;(global-set-key [f6]              'gud-break) ; F7
;;(global-set-key [M-f6]            'gud-remove) ; F7


;; My Mapping Stuff, this is a prefix key that allows me access to more
;; functions.  These are defined in ~/lisp/mhs-map.el

;; let's break this habit
;; (global-set-key [f7]              ') ; F7
(global-set-key [f8]              'mhs-map) ; F7

;; This is so that I can get used to copy-paste in eclipse with mulgaSoft emacs+
(global-set-key (kbd "C-x x") 'copy-to-register)
(global-set-key (kbd "C-x g") 'insert-register)

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
(global-set-key "\C-ci"              'mhs-include-guards); C-c i

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
; (global-set-key (kbd "C-c p") 'perldb)
(global-set-key (kbd "C-x SPC") 'push-mark-command)


;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)


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



;; Ace Jumping mode.
(use-package ace-jump-mode
  :ensure t
  :config
  (define-key global-map (kbd "C-c x") 'ace-jump-mode))

;; (use-package ace-window
;;   ;; https://github.com/abo-abo/ace-window
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "M-o") 'ace-window)
;;   (global-set-key (kbd "C-x o") 'ace-window)
;;   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;   ;; aw-dispatch-always Must be true if you want to use fancy changing actions midway.
;;   (setq aw-dispatch-always 'nil)
;;   )




;; Options for Macintosh Laptop
;;------------------------------
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch C-h a from command-apropos to apropos This is so you have every
;; option that matches your string when you call it.
(define-key help-map "a" 'apropos)



(provide 'emacs-keybinds)
