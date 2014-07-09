;;;
;;; This is a file where I extend my current emacs into separate little
;;; functions.
;;;


;; This function is old and people shouldn't use it, mhs-dblstuff replaces it.
(require 'mhs-dblstuff)

(defun mhs-base-name ()
  "* inserts the current buffer's name where the mark is."
  (interactive)
  (setq str (file-name-sans-suffix (file-name-nondirectory (buffer-file-name))))
  (insert str))


(defun mhs-browse-buffer-in-firefox ()
  "* load the current file into firefox"
  (interactive)
  (browse-url (concat "file://" (buffer-file-name))))


(defun mhs-home-font ()
  (interactive)
  (set-default-font
   "-*-fixed-*-r-normal-*-20-*-*-*-*-*-*-*" nil))

(defun mhs-nusnow-font ()
  (interactive)
  (set-default-font
   "-Misc-Fixed-Medium-R-Normal--20-200-75-75-C-100-ISO8859-1" nil))

(defun mhs-insert-todo ()
  (interactive)
  (insert (concat "TODO [MHS, " (format-time-string "%Y-%m-%d") "] ")))

(defun mhs-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun mhs-bracket-comment()
  (interactive)
  (mhs-insert-date)
  (insert ": <mhs> ")
  (save-excursion
    (insert "</mhs>")))


(defun mhs-save-filename ()
  "* takes current filename in dired mode and stores the file and directory name"
  (interactive)
  (set-register ?f (dired-get-filename "no-dir"))
  (set-register ?d (dired-current-directory))
  (set-register ?a (dired-get-filename))
  (dired-copy-filename-as-kill '0))

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

(defun mhs-double-quote-around-region ()
  "*Places Double quotes around the region"
  (interactive)
  (mhs-dblstuff (string-to-char "\"")))

(defun mhs-paren-region ()
  "*places parentheses around the current region"
  (interactive)
  (mhs-dblstuff (string-to-char ")")))

(defun mhs-anglebracket-region ()
  "*places angle brackets around the current region"
  (interactive)
  (mhs-dblstuff (string-to-char ">")))

(defun mhs-bracket-region ()
  "*places parentheses around the current region"
  (interactive)
  (mhs-dblstuff (string-to-char "}")))

(defun mhs-sqbracket-region ()
  "*places square brackets around the current region"
  (interactive)
  (mhs-dblstuff (string-to-char "]")))

(defun mhs-squote-region ()
  "*places single quotes around the current region"
  (interactive)
  (mhs-dblstuff (string-to-char "'")))


(defun mhs-kill-current () "\
  Kills the current buffer if it has not been modified."
  (interactive)
  (kill-buffer (current-buffer)))



(defun clipboard-copy-region (beg end)
  "copy the region, and save it in the X clipboard."
  (interactive "r")
  (let ((x-select-enable-clipboard t))
    (copy-region-as-kill beg end)))



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



(defun mhs-underline-c++-comment (j)
"underine the current line putting the character in column 1 in column
1 and changing the other characters to be either `-' or the character
following the prefix character"
(interactive "p")
(let (setq tok 'nil)
  (if (null current-prefix-arg)
      (setq tok '?-)
    (setq tok (read-char)))
  (mhs-under-this-c++-comment tok)))

(defun mhs-under-this-c++-comment (tok)
  ;;   // this is the comment.
  (let (beg end-com white first end line comval ul)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (while (looking-at "[^/]") (forward-char))
      (setq white (point))
      (setq comval (buffer-substring (point) (+ 2 (point))))
      (while (looking-at "[ \t]") (forward-char))
      (setq first (point))
      (setq white (make-string (- white beg) ? ))
      (end-of-line)
      (while (looking-at "[ \t\n]") (forward-char -1))
      (setq end (1+ (point)))
      (setq ul (make-string (-(- end first)2) tok))
      (forward-line)
      (beginning-of-line)
      (insert (concat  white comval ul "\n")))))


(defun jao-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column                          ; disable toggle if column was supplied
       (unless selective-display 1))))


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


(defun mhs-convert-to-ftp-loc ()
(interactive)
(save-excursion)
(replace-
 "/disks/sidads_incoming/savoie/"
 "ftp://sidads.colorado.edu/pub/incoming/savoie/"))


(defun mhs-use-inconsolata ()
  (interactive)
  (progn (set-face-attribute 'default nil :inherit nil :stipple nil
                               :background "gray12" :foreground "gray89"
                               :inverse-video nil :box nil
                               :strike-through nil :overline nil
                               :underline nil  :height 150
                               :foundry "unknown" :family "Inconsolata")))


(defun mhs-use-monaco ()
  "Switch from the current face to a nice coding font for macintosh machines"
  (interactive)
  (progn (set-face-attribute 'default nil :inherit nil :stipple nil
                             :background "gray12" :foreground "gray89"
                             :inverse-video nil :box nil
                             :strike-through nil :overline nil
                             :underline nil  :height 145
                             :foundry "apple" :family "Monaco")))


(defun mhs-use-menlo ()
  "Switch from the current face to a nice coding font for macintosh machines"
  (interactive)
  (progn (set-face-attribute 'default nil :inherit nil :stipple nil
                             :background "gray12" :foreground "gray89"
                             :inverse-video nil :box nil
                             :strike-through nil :overline nil
                             :underline nil  :height 145
                             :foundry "apple" :family "Menlo")))


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
                             :background "gray12" :foreground "gray89"
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
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

(defun camelize-method (s)
  "Convert under_score string S to camelCase string."
  (mapconcat 'identity (mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))


(defun underscore-previous-camel (&optional beg end)
  "unCamelize the previous camelCased string.
    If transient-mark-mode is active and a region is activated,
    camelize the region."
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


(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))



(provide 'mhs-extends)
