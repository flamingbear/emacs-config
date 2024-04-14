;;; package --- Summary
;;;   Just standard extra things to load when I start emacs.

(defun start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))


;; Override having to type Yes or No with just Y or N
(fset 'yes-or-no-p 'y-or-n-p)
(put 'scroll-left 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
;; To determine when to split horizontally
(setq split-width-threshold 1600)

;; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq gc-cons-threshold (* 100 1024 1024))
(run-with-idle-timer 4 t (lambda () (garbage-collect)))

;; (setq gc-cons-threshold (* 80  1024))

(setq garbage-collection-messages 't)


(setq line-number-display-limit-width 1000)

(setq indent-tabs-mode nil)
(setq inhibit-eol-conversion t)
(setq inhibit-startup-screen t)

;; Some magnar's Sane Defaults.
;;------------------------------
;; Show keystrokes in progress
(setq echo-keystrokes 0.3)

;; Remove text in active region if inserting text in non-nil
(setq delete-selection-mode 'nil)

;; Easily NavigateSillyCased words when set to 0
(global-subword-mode 0)

;; Don't break lines for me, please
;;  But use mhs-trunc-lines to toggle bound to [C-c t]
(setq-default truncate-lines t)

;; Toggle selective display based on current column
;; https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159
;; Use C-u C-u to set selective display to current line's indentation.
(defun set-selective-display-current (arg)
  "Toggle fold all lines larger than indentation on current line."
  (interactive "P")
  (if (eq (nth 0 arg) 16)
      (let ((col 1))
	(save-excursion
	  (back-to-indentation)
	  (setq col (+ 1 (current-column)))
	  (set-selective-display
	   (if selective-display nil (or col 1)))))
    (set-selective-display arg)))
(global-set-key (kbd "C-x $") 'set-selective-display-current)


(use-package highlight-indentation
  :ensure t
  :hook ((yaml-mode . highlight-indentation-mode)
	 (python-mode . highlight-indentation-mode)
	 (yaml-mode . highlight-indentation-current-column-mode))
  :config
  (highlight-indentation-mode)
  (diminish 'highlight-indentation-mode)
  (custom-theme-set-faces
   'user
   '(highlight-indentation-current-column-face ((t (:foreground nil :background "#505050"))))
   '(highlight-indentation-face ((t (:foreground nil :background "#303030")))))
  )

;; C-c <arrow> moves region
(use-package smart-shift
  :ensure t
  :config
(global-smart-shift-mode 1))


(setq tab-always-indent 'complete)
(tool-bar-mode -1)
(setq transient-mark-mode 't)


;; Bug in El Capitain with visible bell and emacs 24.x
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21662
(defun my-zoidberg-bell ()
  "Ring visible bell with text in minibuffer."
  (message "*woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop* *woop*")
  (sleep-for .15))

(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell 't)
(if running-macos (setq ring-bell-function #'my-terminal-visible-bell) (setq ring-bell-function #'my-zoidberg-bell))


;; auto wrap, but only in comments
(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))


(defvar dev-hook nil
  "Hook that gets run on activation of any programming mode.")
(add-hook 'dev-hook 'display-line-numbers-mode)
(add-hook 'dev-hook 'local-comment-auto-fill)
;; (add-hook 'dev-hook 'company-mode)

(defun run-dev-hook ()
  "Enable things that are convenient across all dev buffers."
  (run-hooks 'dev-hook))


;; show empty lines at the end of the file
(set-default 'indicate-empty-lines t)
(setq show-trailing-whitespace t)

;; automatically sync up external changes to files
;;(global-auto-revert-mode nil)
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))
(defalias 'auto-revert-tail-mode 'tail-mode)


;; delete trailing whitespace
;; TODO [MHS, 2013-01-23] You should learn how to make this only for modes you want.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'text-mode-hook #'(lambda ()
			     (setq indent-tabs-mode nil)
			     (setq tab-width 4)))
;; To edit binary files
;; (remove-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode 't))



;;----------------------------------------------------------------
;; Personal Lisp directory
;;----------------------------------------------------------------

;; Is builtin package "project" good enough?
(with-eval-after-load 'project
  (setq project-switch-commands
	'((project-find-file "Find file")
	  (project-find-dir "Find directory")
	  (magit-project-status "Magit")))
  (keymap-set project-prefix-map "m" #'magit-project-status)
  ;; Try not to use these.
  (global-set-key (kbd "C-c p p") 'project-switch-project)
  (global-set-key (kbd "C-c p k") 'project-kill-buffers)
  (global-set-key (kbd "C-c f") 'project-find-file))

(use-package async :ensure t)

(use-package grep
  :defer 3
  :config
  ;; (setq grep-find-command
  ;;       "find . -name \".svn\" -prune -o -type f  -exec grep -nH \"\" {} \\;"))
  )

(use-package wgrep :ensure t)

(use-package puppet-mode :ensure t)
(use-package gist :ensure t)

(use-package so-long :ensure t
  :config
  (global-so-long-mode 1))


;; IDLWAVE Customizations
;; Load this before ruby because we want the jds history search.
;; TODO [MHS, 03/25/2022] Someday I'm going to need this again...
(use-package emacs-idlwave-support)

;; Use jira information
(use-package mhs-jira)

;; Python environment
(use-package mhs-python)

;; Now we're a docker house.
(use-package dockerfile-mode
  :ensure t)

(use-package docker
  :ensure t
  :pin melpa
  :diminish docker-mode)

(use-package docker-compose-mode
  :ensure t)

(use-package kubel
  ;; kubernetes k9s clone (sort of)
  :ensure t
  :after (vterm)
  :config (kubel-vterm-setup))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package markdown-mode
  :ensure t
  :config
  (setq auto-mode-alist
        (cons '("\\.md" . markdown-mode) auto-mode-alist))
  )

;; No longer maintained.  let's try not using it.
;; (use-package paradox
;;   :defer t
;;   :ensure t
;;   :config
;;   (setq paradox-automatically-star t)
;;   (setq paradox-execute-asynchronously t)
;;   (load (expand-file-name "private/paradox-secrets.el.gpg" user-emacs-directory))
;;   )

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook
            #'(lambda ()
		(define-key yaml-mode-map "\C-m" 'newline-and-indent))))


;; Try company-mode instead of auto-complete
;;--------------------------
;; (use-package company-jedi :ensure t) ;; https://pypi.org/project/jedi/
;; Try Corfu
;; (use-package company
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-tern)
;;   ;; (add-to-list 'company-backends 'company-jedi)
;;   (add-to-list 'company-backends 'company-terraform)
;;   (setq company-idle-delay 0.005)
;;   (require 'company-dabbrev-code)
;;   (add-to-list 'company-dabbrev-code-modes 'js2-mode)
;;   (add-to-list 'company-dabbrev-code-modes 'yaml-mode)
;;   (add-to-list 'company-dabbrev-code-modes 'markdown-mode)
;;   (add-to-list 'company-dabbrev-code-modes 'text-mode)
;;   (setq company-dabbrev-downcase 'nil)
;;   (global-set-key (kbd "<C-tab>") 'company-complete)
;;   )

;; ;; popups with company completion
;; (use-package company-quickhelp
;;   :ensure t
;;   :config
;;   (company-quickhelp-mode 1)
;;   (add-hook 'after-init-hook 'global-company-mode)
;;   )

(use-package paren
  :defer 3
  :config
  (show-paren-mode t))

(use-package restclient
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))
  )

;; insert pairs of parens when coding.
(use-package smartparens
  :ensure t
  :diminish
  :config
  (require 'smartparens-config))

;; Show different colors on parens so you can visually identify the different hunks.
(use-package rainbow-delimiters
  :ensure t
  :diminish
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; Javascript stuff
(use-package mhs-javascript)

;; if you are debugging emacs completely: open this file and it records keystrokes.
;;(open-dribble-file "~/dribble")

;; For orgmode and others start emacsclient
(server-start)

(use-package emacs-everywhere :ensure t)

;; Do binary diff in hexl-mode if files are binary format
(use-package binary-diff)

;; Use system trash
(setq delete-by-moving-to-trash 't)
(setq trash-directory "~/.Trash/")

;;--------------------------------------------------------------------------
;; GIT has some quirks and normally, I don't want to do a regular diff when
;; looking at revisions.
(eval-after-load "vc-hooks"
  '(define-key vc-prefix-map "=" 'ediff-revision))


;; ediff cutomizations
;;;;;;;;;;;;;;;;;;;;;;;;
;; found on reddit to make ediff work in one window.
;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read personal abbreviations each time
(setq abbrev-file-name (locate-user-emacs-file "abbrev_defs"))
(setq-default abbrev-mode t)

;; Set the bookmark file in the right location.
(setq bookmark-default-file (locate-user-emacs-file ".emacs.bmk"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Text mode (hook also run by related modes, e.g. Tex/LaTeX & outline)
(setq-default fill-column 79) ; Change it locally with [C-x f]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Makefile mode stuff
;;  you can comment this out if you don't like the "features" of this mode by
;;  placing a semicolin in front of the line.
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Emacs-Lisp stuff
;;
(add-to-list 'auto-mode-alist '("\\.emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("_emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.gnus" . emacs-lisp-mode))

;; Sh mode for some common files
(add-to-list 'auto-mode-alist '("\\.alias$" . sh-mode))

;; unique buffer names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;; ------------------------------------------------------------
;; auto-insert madness
;; ------------------------------------------------------------
(use-package autoinsert
  :config
  (setq auto-insert-path '("~savoie/.emacs.d/autoinsert/insert/"))
  (auto-insert-mode 1)
  (add-hook 'find-file-hook 'auto-insert)
  (setq auto-insert-directory (concat (file-name-as-directory user-emacs-directory) "autoinsert/"))
  (add-to-list 'auto-insert-alist '("\\.pro$" . "idlwave-insert.pro")))

(use-package bookmark
  :config
  (setq bookmark-save-flag 1))

(autoload 'css-mode "css-mode")
(setq css-indent-offset 2)

(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require the gnus stuff that I wrote...
(use-package mhs-map)
(use-package mhs-magit)
(use-package mhs-extends)
;; (use-package mhs-perl)
;; (use-package mhs-grep)
;; (use-package mhs-cmode)


;; Handle multiple locations for aspell.
(defvar ispell-program-name)
(let ((locations '("/usr/bin/aspell"
                   "/usr/local/bin/aspell"
                   "/opt/local/bin/aspell"
                   "/opt/homebrew/bin/aspell")))
  (setq ispell-program-name (cl-find-if #'file-exists-p locations)
        ispell-program-name (or ispell-program-name "~/local/bin/aspell")))


;; Use these to override stupid defaults for the ! command in dired.
;; Dired extra commands
(use-package dired
  :config
  (setq dired-listing-switches "-al"))

(use-package dired-x
  :config
  (setq dired-guess-shell-alist-user
	(list
	 '("\\.gif$" "open")
	 '("\\.pdf" "open")
	 )))

(use-package dired-subtree
  :ensure t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package react-snippets
  :ensure t
  :after yasnippet)

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package pinentry
  :ensure t
  :config
  (pinentry-start)
  (setq epa-pinentry-mode 'loopback)
   ;;(setq epg-gpg-program "/usr/local/bin/gpg")
  )

;; possible packages     (pinentry anything auto-complete)

(use-package helpful
  ;; Helpful is an alternative to the built-in Emacs help that provides much
  ;; more contextual information.
  :ensure t
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  ;; I also recommend you configure helpful-at-point to a convenient keybinding:
  (global-set-key (kbd "C-c C-.") #'helpful-at-point)
  (global-set-key (kbd "C-h C") #'helpful-command))


;; Instead of:
;; ((python-mode
;;   (pyvenv-activate . "/Users/savoie/.pyenv/versions/harmony-regression-tests")))


(defun mhs-create-dir-locals-file ()
  "Create a new .dir-locals.el file in the current directory."
  (interactive)
  (let ((file-path (expand-file-name ".dir-locals.el" default-directory))
        (content "((python-mode\n  (eval . (let* ((python-version-file (locate-dominating-file default-directory \".python-version\"))\n                 (python-env (when python-version-file\n                               (with-temp-buffer\n                                 (insert-file-contents (expand-file-name \".python-version\" python-version-file))\n                                 (string-trim (buffer-string))))))\n             (when python-env\n               `(pyvenv-activate . ,(concat \"/Users/savoie/.pyenv/versions/\" python-env)))))))\n"))
    (with-temp-file file-path
      (insert content))
    (message "Created .dir-locals.el file in current directory.")))


;; Set registers to things that I type zillions of times.
;; ------------------------------------------------------


(fset 'mhs-reformat-xml
   [?\C-u escape ?| ?x ?m ?l ?l ?i ?n ?t ?  ?- ?- ?f ?o ?r ?m ?a ?t ?  ?- ?  return])

(defun mhs-next-error ()
  "find the next ✖ in the file."
  (interactive)
  (search-forward "✖"))

;; Stuff out of my custom

(setq scroll-bar-mode 'right)
(setq select-active-regions 't)

(setq large-file-warning-threshold 20000000)

(use-package terraform-mode :ensure t)
(use-package hcl-mode :ensure t)
;; (use-package company-terraform :ensure t)

(use-package vterm :ensure t
  :init
  (defun visit-vterm-buffer ()
    "Create or visit a terminal buffer."
    (interactive)
    (if (not (get-buffer "vterm"))
	(progn
          (split-window-sensibly (selected-window))
          (vterm-other-window))
      (switch-to-buffer-other-window "vterm")))
  :config
  (setq vterm-max-scrollback (* 32 1024))
  )

;; Found in docs for use-package
(use-package unfill
  :ensure t
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  :config
  ;; Added this without testing when I was getting errors in org mode and #begin_src bash blocks
  (setq flycheck-emacs-lisp-load-path 'inherit)
  )

(use-package atomic-chrome
  ;; https://github.com/alpha22jp/atomic-chrome
  :ensure t
  :init
  (atomic-chrome-start-server)
  ;; Possible to set default mode (python if you're using the jupyter thing a lot
  ;; (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-url-major-mode-alist
	'(
	  ("github\\.com" . gfm-mode)
	  ("localhost:888." . python-mode)
          )
	)
  )

;; TODO [MHS, 03/22/2023] This doesn't do anything yet
;; (use-package tree-sitter :ensure t)
;; (use-package tree-sitter-langs :ensure t)


(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(provide 'emacs-extras)
;;; emacs-extras.el ends here
