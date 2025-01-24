;;;
;;; This is a file where I extend my current emacs into separate little
;;; functions.
;;;

;;;  http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; This function is old and people shouldn't use it, mhs-dblstuff replaces it.
(use-package mhs-dblstuff)

(defun mhs-base-name ()
  "* inserts the current buffer's name where the mark is."
  (interactive)
  (setq my-str (file-name-sans-suffix (file-name-nondirectory (buffer-file-name))))
  (insert my-str))


;; (defun mhs-browse-buffer-in-firefox ()
;;   "* load the current file into firefox pre 28?"
;;   (interactive)
;;   (browse-url (concat "file://" (buffer-file-name))))

(defun mhs-browse-buffer-in-firefox ()
  "* load the current file into firefox"
  (interactive)
  (browse-url (buffer-file-name) 't))


(defun mhs-insert-todo ()
  (interactive)
  (insert (concat "TODO [MHS, " (format-time-string "%m/%d/%Y") "] ")))

(defun mhs-insert-date (arg)
  (interactive "p")
  (let ((fmt '"%F"))
    (if (not (= arg 1)) (setq fmt '"%FT%R" ))
    (insert (format-time-string fmt))))

(defun mhs-bracket-comment()
  (interactive)
  (mhs-insert-date)
  (insert ": <mhs> ")
  (save-excursion
    (insert "</mhs>")))



(defun mhs/save-dired-filename-and-dir ()
  "Save the current filename and directory in Dired mode."
  (interactive)
  (set-register ?f (dired-get-filename "no-dir"))
  (set-register ?d (dired-current-directory))
  (let ((file-name  (dired-get-filename)))
  (set-register ?n file-name)
  (kill-new file-name)
  (message file-name)))

;; Something lead me to this:
;; https://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/ But since
;; I've got decades of my old hacky stuff in muscle memory. Let's fix this and
;; make a unified version that does the right thing.
(defun mhs/save-filename-and-dir ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
	  (set-register ?f (file-name-nondirectory file-name))
	  (set-register ?d (file-name-directory file-name))
	  (set-register ?a file-name)
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun mhs/filename-and-dir ()
  (interactive)
  (if (eq major-mode 'dired-mode)
      (mhs/save-dired-filename-and-dir)
    (mhs/save-filename-and-dir)))


(defun mhs-browse-current-filename ()
  "* Fires up firefox on the filename currently stored in register a"
  (interactive)
  (browse-url (get-register ?a) 't))

(defun mhs-insert-filename ()
  "*creates a filename from register d and register f for dir and file"
  (interactive)
  (push-mark)
  (save-restriction (narrow-to-region (mark 't) (point))
                    (insert-register ?d t)
                    (insert-register ?f t)))

(defun load-register-values ()
  "Load register values."
  (interactive)
  (set-register ?x "import xarray as xr")
  (set-register ?y "from xarray.core.datatree import *")
  (set-register ?z "from xarray.core.treenode import *"))

(load-register-values)

(defun mhs-backtick-around-region ()
  "*Places Double quotes around the region"
  (interactive)
  (mhs-dblstuff ?`))

(defun mhs-double-quote-around-region ()
  "*Places Double quotes around the region"
  (interactive)
  (mhs-dblstuff ?\"))

(defun mhs-paren-region ()
  "*places parentheses around the current region"
  (interactive)
  (mhs-dblstuff ?\)))

(defun mhs-anglebracket-region ()
  "*places angle brackets around the current region"
  (interactive)
  (mhs-dblstuff ?>))

(defun mhs-bracket-region ()
  "*places parentheses around the current region"
  (interactive)
  (mhs-dblstuff ?}))

(defun mhs-sqbracket-region ()
  "*places square brackets around the current region"
  (interactive)
  (mhs-dblstuff ?\]))

(defun mhs-squote-region ()
  "*places single quotes around the current region"
  (interactive)
  (mhs-dblstuff ?'))

(defun mhs-kill-current () "\
  Kills the current buffer if it has not been modified."
  (interactive)
  (kill-buffer (current-buffer)))

(defun clipboard-copy-region (beg end)
  "copy the region, and save it in the X clipboard."
  (interactive "r")
  (clipboard-kill-ring-save beg end))


(defun mhs-lineup ()
  "*aligns lines of code according to the character after the point"
  (interactive)
  (save-excursion
    (let ((tok (buffer-substring (point) (+ 1 (point))))
	  (cur (current-column))
	  (dif nil))
      (forward-line)
      (while (search-forward tok (save-excursion
				   (end-of-line)
				   (point)) 't)
	(progn
	  (move-to-column (1+ cur))
	  (if (search-backward tok (save-excursion
				     (beginning-of-line)
				     (point)) 't)
	      (progn
		(goto-char (point) )
		(setq dif (- cur (current-column) ))
		(insert (make-string dif (string-to-char " ")))))
	  (forward-line)))
      "Completed Lineup")))

(defun mhs-shift-left-to-the-right ()
  (interactive)
  (save-excursion
    (let ((insstr nil)
	  (fail-p 't))
      (backward-char)
      (while  (looking-at "[ \t\n]")
	(progn
	  (setq fail-p nil)
	  (delete-char 1)
	  (setq insstr (concat insstr " "))
	  (backward-char)))
      (if (not fail-p)
	  (progn
	    (while (not (looking-at "[ \t\n]"))
	      (backward-char))
	    (insert insstr))))))


(defun mhs-shift-lines ()
  "Shifts a word (all non whitespace characters) to the left replacing
  their original location with whitespace.  This descends from the
  point until the character after the point is different from the
  initial character after the point"
  (interactive)
  (save-excursion
    (let ((tok (buffer-substring (point) (+ 1 (point))))
	  (cur (current-column))
	  (dif nil))
      (while (looking-at tok)
	(progn
	  (mhs-shift-left-to-the-right)
	  (forward-line)
	  (move-to-column cur)
	  )))))


(defun mhs-close-whitespace-rectangle () "\t
 remove excess whitespace within a rectangle set by point and mark"
  (interactive)
  (let ((top-corner (min (point) (mark)))
	(bottom-corner (max (point) (mark)))
	(start-col nil)
	(finish-col nil)
	(finished nil))
    (save-restriction
      (save-excursion
	(goto-char top-corner)
	(beginning-of-line)
	(push-mark)
	(goto-char bottom-corner)
	(end-of-line)
	(narrow-to-region (mark) (point))
	)
      (setq start-col
	    (min (current-column)
		 (save-excursion (goto-char (mark))
				 (current-column))))
      (setq finish-col
	    (max (current-column)
		 (save-excursion (goto-char (mark))
				 (current-column))))
      (goto-char (point-min))
      (while (not finished)
	(progn
	  (move-to-column finish-col)
	  (search-backward " " (save-excursion (beginning-of-line) (point)) 't)
	  (fixup-whitespace)
	  (forward-line)
	  (if (eobp) (setq finished 't))
	  )))))


(defun mhs-occur-this-word-maybe (n) "\t
    If the mark is active, do Occur on the region.
    If the mark is inactive, do Occur on the word under point.
    A prefix argument calls the normal occur."
  (interactive "p")
  (if (not (= n 1)) (command-execute 'occur)
    (save-excursion
      (let ((start nil)
	    (finish nil)
	    (tok nil))
	(if (not mark-active)
	    ;; mark the current word
	    (progn (forward-word -1)
		   (set-mark (point))
		   (forward-word 1)))

	(progn
	  ;; set the start and finish of the region
	  (setq start (min (mark) (point)))
	  (setq finish (max (mark) (point))))

	(setq tok (buffer-substring start finish))
	(set-text-properties 0 (length tok) nil tok)
	(occur tok)))))

(defun mhs-fixup-whitespace (event)
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (fixup-whitespace)))

(defun mhs-under-this-line (tok)
  (let (beg end-com white first end line comstart comend comfinish ul)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (setq comstart comment-start)
      (setq comend comment-end)
      (setq comlength (+ (length comstart) (length comend)))
      (setq white (point))
      (while (looking-at "[ \t]") (forward-char))
      (setq first (point))
      (setq white (make-string (- first beg) ? ))
      (end-of-line)
      (while (looking-at "[ \t\n]") (forward-char -1))
      (setq end (1+ (point)))
      (setq ul (make-string (max  0 (- (- end first) comlength ) ) tok))
      (forward-line)
      (beginning-of-line)
      (insert (concat white comment-start ul comment-end "\n")))))

(defun mhs-underline-comment-maybe (j)
"underine the current line putting the character in column 1 in column
1 and changing the other characters to be either `-' or the character
following the prefix character"
(interactive "p")
(let (setq tok 'nil)
  (if (null current-prefix-arg)
      (setq tok '?-)
    (setq tok (read-char)))
  (mhs-under-this-line tok)))

(defun mhs-trunc-lines ()
  (interactive)
  (set-variable 'truncate-lines (null truncate-lines)))

(defun mhs-ediff-ignore-dollars ()
  "Set the correct variable to ignore stuff between dollar signs during ediff"
  (interactive)
  (set-variable 'ediff-diff-options "--ignore-matching-lines=^.*\\$.*\\$.*$"))

(defun mhs-ediff-normal ()
  "Set ediff variable back to normal operations."
  (interactive)
  (set-variable 'ediff-diff-options ""))

(defun mhs-use-inconsolata ()
  (interactive)
  (progn (set-face-attribute 'default nil :height 200 :family "Inconsolata")))

(defun mhs-use-meslo ()
  (interactive)
  (progn (set-face-attribute 'default nil  :height 170 :font "MesloLGS NF")))

(defun mhs-increase-font-size ()
  "Increase the font height by 10"
  (interactive)
  (progn
    (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) 10) )))


(defun mhs-decrease-font-size ()
  "Decrease the font height by 10"
  (interactive)
  (progn
    (set-face-attribute 'default nil :height (- (face-attribute 'default :height) 10) )))


(defun mhs-use-normal-face ()
  (interactive)
  (progn (set-face-attribute 'default nil :font "10x20" :inherit nil :stipple nil
                             :inverse-video nil :box nil :strike-through nil
                             :overline nil :underline nil )))



(defun un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
    Default for SEP is a hyphen \"-\".
    If third argument START is non-nil, convert words after that
    index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "_")
                                     (downcase (match-string 0 s)))
                             t nil s)))
    (downcase s)))

;;Sample EmacsLisp code to convert a string from underscore  CamelCase:
(defun mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defun camelize (s)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

(defun camelize-method (s)
  "Convert under_score string S to camelCase string."
  (mapconcat 'identity (mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))


(defun snake-previous-camel (&optional beg end)
  "unCamelize the previous camelCased string.
    If transient-mark-mode is active and a region is activated,
    snakify the region."
  (interactive "r")
  (unless (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
    (setq end (point)
          beg (+ (point) (skip-chars-backward "[:alnum:]_"))))
  (save-excursion
    (let ((c (un-camelcase-string (buffer-substring-no-properties beg end))))
      (delete-region beg end)
      (goto-char beg)
      (insert c))))


(defun camelize-previous-snake (&optional beg end)
  "Camelize the previous snake cased string.
    If transient-mark-mode is active and a region is activated,
    camelize the region."
  (interactive "r")
  (unless (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
    (setq end (point)
          beg (+ (point) (skip-chars-backward "[:alnum:]_"))))
  (save-excursion
    (let ((c (camelize-method (buffer-substring-no-properties beg end))))
      (delete-region beg end)
      (goto-char beg)
      (insert c))))

(use-package json-mode
  :ensure t
  :config
  (setq json-reformat:indent-width 2)
  (setq js2-indent-level 2)
  (setq js-indent-level 2)
  :hook  (json-mode . hs-minor-mode)
  :bind (:map json-mode-map
              ("C-c C-f" . json-mode-beautify)
              ("C-c C-h" . hs-hide-block)
              ("C-c C-s" . hs-show-block)
              ("C-c C-t" . hs-toggle-hiding))
  )

(use-package json-navigator
  :ensure t)

(setq column-number-mode t)
(setq comment-column 32)

(use-package ansi-color
  :defer 2
  :config
  ;; Use these colors for solarized iterm2 windows
  (setq ansi-color-names-vector  ["#454545" "#cd5542" "#6aaf50" "#baba36" "#5180b3" "#ab75c3" "#bdbc61" "#bdbdb3"])
  (ansi-color-for-comint-mode-on)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  (defun destructive-display-ansi-colors ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))
  )


;; found here https://stackoverflow.com/a/33456622/66100
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)



(provide 'mhs-extends)
