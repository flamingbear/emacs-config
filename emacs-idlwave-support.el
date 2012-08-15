;;; EMACS-IDLWAVE-SUPPORT.el --- Quick Extraction of stuff I use for IDLWAVE and emacs at NSIDC.

;; Copyright (C) 2008 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 14 May 2008
;; Version: 1.0
;; Keywords:

 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to <savoie@nsidc.org>) or from the
;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; |Matt Savoie|<savoie@nsidc.org>
;; |Quick Extraction of stuff I use for IDLWAVE and emacs at NSIDC.
;; |$Date: 2011-10-20 14:59:03 -0600 (Thu, 20 Oct 2011) $|$Revision: 19610 $|~/packages/emacs-idlwave-support.el


;;; Code:

(defconst -version (substring "$Revision: 19610 $" 11 -2)
  "$Id: emacs-idlwave-support.el 19610 2011-10-20 20:59:03Z savoie $ Report bugs to: Matt Savoie <savoie@nsidc.org>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDLWAVE Customizations
;; Change the indentation preferences

;; Changes to this in `idlwave-mode-hook' will have no effect.  Instead a user
;; must set it directly using `setq' in the .emacs file before idlwave.el
;; is loaded.


;; This changes the default abbreviation character to a forward slash.  It
;; works great for me, but you might be using / for something else.
(setq idlwave-abbrev-start-char "/")

;; Load the idlwave mode and shell, and associate the .pro files with that mode.
(autoload 'idlwave-mode "idlwave" "IDLWAVE Mode" t)
(autoload 'idlwave-shell "idlw-shell" "IDLWAVE Shell" t)
(add-to-list 'auto-mode-alist '("\\.pro$\\|.PRO$\\'" . idlwave-mode))



;; Set up some functions that we need to define before we can use them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the actual author is 
;;  JD Smith <jdsmith@as.arizona.edu>
;; I'm just putting it together for anyone to use.

;; These commands make the up/down arrows do regular expression searches that
;; include whatever is already on the idlwave-shell's command line.

;; So if you did a plot command ten commands ago, you would put plot on the
;; shell line.
;;  IDL> plot, 
;; and then hit the up arrow* to get to the commane
;; *(if you bind this commmand to it like we do later in this file)
(defvar jds-search-regexp nil)

(defun jds-search-pre-command-hook ()
  (unless (or (eq this-command 'jds-history-search-up)
	      (eq this-command 'jds-history-search-down))
    (remove-hook 'pre-command-hook 'jds-search-pre-command-hook t)
    (setq jds-search-regexp nil)))

(defun jds-history-search (dir)
  (when (null jds-search-regexp)
    (let ((str (buffer-substring (point) (save-excursion (comint-bol nil)
							 (point)))))
      (if (or (string= str "") (not (looking-at "\\s-*$")))
	  (setq jds-search-regexp ".")
	;; This is what JDS had.
	;; (setq jds-search-regexp (concat "^" (regexp-quote str)))
	(setq jds-search-regexp (regexp-quote str)))
      (add-hook 'pre-command-hook 'jds-search-pre-command-hook nil t)))
  (comint-previous-matching-input jds-search-regexp dir))

(defun jds-history-search-up ()
  (interactive)
  (jds-history-search 1))

(defun jds-history-search-down ()
  (interactive)
  (jds-history-search -1))


;; This inserts the normal fanning enhanced function template into the buffer
(defun mhs-idlwave-function ()
  (interactive)
  (idlwave-template 
   (idlwave-rw-case "function" )
   (idlwave-rw-case "\ncompile_opt idl2, logical_predicate\n\n \nreturn\n\nend")
   "Function name"))

(defun mhs-idlwave-function-with-error ()
  (interactive)
  (idlwave-template 
   (idlwave-rw-case "function")
   (idlwave-rw-case "\ncompile_opt idl2, logical_predicate\n\n ;;Error Handling\ncatch,  theError\nif theError ne 0 then begin\nCatch, /cancel\nok =  Error_Message(!Error_State.Msg)\nreturn,  0\nendif\nreturn\n\nend")
   "Function name"))


;; This inserts the normal fanning enhanced procedure template into the buffer
(defun mhs-idlwave-procedure-with-error ()
  (interactive)
  (idlwave-template 
   (idlwave-rw-case "pro")
   (idlwave-rw-case "\ncompile_opt idl2, logical_predicate\n\n ;;Error Handling\ncatch,  theError\nif theError ne 0 then begin\nCatch, /cancel\nok =  Error_Message(!Error_State.Msg)\nreturn\nendif\n\nend")
   "Procedure name"))

(defun mhs-idlwave-procedure ()
  (interactive)
  (idlwave-template 
   (idlwave-rw-case "pro")
   (idlwave-rw-case "\ncompile_opt idl2, logical_predicate\n\n\nend")
   "Procedure name"))



(setq comint-input-ring-size 1024) 


;; I like to use alt-s to complete anything I'm typing.
(defun my-common-idlwave-hook ()
  (local-set-key [(meta .)] 'idlwave-find-module)
  (local-set-key [(super z)] 'mhs-idlwave-shell-reset)
  (local-set-key [(meta s)] 'idlwave-complete)  )


;; set up commands whenever you enter and idlwave buffer
(add-hook 'idlwave-mode-hook
	  (lambda ()

            ;; Skip over Underbars when word forward
            ;; (modify-syntax-entry ?_ "w")
            ;; 2011-09-16: <mhs>  I didn't like that. </mhs>
            ;; These insert spaces around the character.
            ;; alpha[beta] --> alpha[ beta ]
	    (idlwave-action-and-binding "["  '(idlwave-surround 'nil 1 1))
	    (idlwave-action-and-binding "]"  '(idlwave-surround 1 'nil 1))
	    (idlwave-action-and-binding "("  '(idlwave-surround 'nil 1 1))
	    (idlwave-action-and-binding ")"  '(idlwave-surround 1 'nil 1))
	    (idlwave-action-and-binding "*"  '(idlwave-surround -1 'nil 1))

            ;;  I stopped using these because of operator conflicts.
	    ;; (idlwave-action-and-binding "+"  '(idlwave-surround -1 'nil 1))
	    ;;(idlwave-action-and-binding "-"  '(idlwave-surround -1 'nil 1))

	    
            ;; Set a couple of different bindings for the idlwave-complete.
            ;; *everyone* should learn how to complete things they are typing.
	    (local-set-key [s-tab] 'idlwave-complete)
            (local-set-key [ESC-TAB] 'idlwave-complete)


            ;; Set up some abbreviations.  In idlwave mode. if you put your
            ;; abbrevation prefix and then abbrev it expands into t the full
            ;; form.
            ;; e.g.:  /fu -> (the entire fanning function expansion.)
            ;; The most useful to me are /fu, /pr, and /fn1

            ;; There are a bunch of other predefined versions, that you can
            ;; discover with "M-x list-abbrevs" in a buffer in idlwave mode
            
	    (idlwave-define-abbrev "wb" "Widget_Base()"
				   (idlwave-keyword-abbrev 1))
	    (idlwave-define-abbrev "pn" "Ptr_New()"
				   (idlwave-keyword-abbrev 1))
	    (idlwave-define-abbrev "fun"  "" 
				   (idlwave-code-abbrev mhs-idlwave-function-with-error))
	    (idlwave-define-abbrev "prn"  "" 
				   (idlwave-code-abbrev mhs-idlwave-procedure-with-error))
            (idlwave-define-abbrev "fu"  "" 
				   (idlwave-code-abbrev mhs-idlwave-function))
	    (idlwave-define-abbrev "pr"  "" 
				   (idlwave-code-abbrev mhs-idlwave-procedure))
	    (idlwave-define-abbrev "on" "Obj_New()"
				   (idlwave-keyword-abbrev 1))
	    (idlwave-define-abbrev "od" "Obj_Destroy,"
				   (idlwave-keyword-abbrev 0))
            (idlwave-define-abbrev "fn1" "for  = 0, n_elements(  ) - 1 do begin\nendfor"
                                   (idlwave-keyword-abbrev 40))
            )
          )


;;  I set comments to start on line 1 because idlwave will refuse to move them
;;  if they are in the 0th column
(add-hook 'idlwave-mode-hook 
          (lambda () 
            (set (make-local-variable 'comment-add) 1)))


;;  Here's where we bind the up and down arrows to the regular expression search from JD.
(add-hook 'idlwave-shell-mode-hook
          (lambda ()
            ;; A cheat for quick function-only lookup in the shell.
            (local-set-key [up]   'jds-history-search-up)
	    (local-set-key [down] 'jds-history-search-down)
            (local-set-key [M-up] 'comint-previous-matching-input-from-input)
            (local-set-key [M-down] 'comint-next-matching-input-from-input)
	    (local-set-key [f1]  'mhs-insert-filename)
	    ))

;; Pink everything after 80 columms
(add-hook 'idlwave-mode-hook
          (lambda ()
            (font-lock-add-keywords 
             nil '(("^[^\n]\\{90\\}\\(.*\\)$" 1 font-lock-warning-face t)))))


(add-hook 'idlwave-mode-hook
          (lambda ()
            (font-lock-add-keywords 
             nil '(("\\(<mhs>.*</mhs>\\)" 1 font-lock-warning-face t)))))
;;  Add my common hooks to both modes shell and mode.
(add-hook 'idlwave-mode-hook 'my-common-idlwave-hook 't)
(add-hook 'idlwave-shell-mode-hook 'my-common-idlwave-hook 't)


(defvar mhs-idlwave-header
  (list nil
        ";+
;  <Desciption>
;
; :Params:
;    var : in|out, required|optional, type=type
;       Description of parameter
;
; :Keywords:
;    keyword1 : in|out, required|optional, type=type
;       Description of keyword
;   
; :Returns: <return info>
;
;-
")
  "*A string to insert for putting in headers for idlwave")


;; Create a simple function that inserts the comments for a function.
(defun mhs-idlwave-insert-comment ()
  "Build Skeleton for comments of routines and procedures"
  (interactive)
  (insert (car (cdr mhs-idlwave-header))))



(defvar mhs-idlwave-do_ps-keyword 
";    do_ps : in, optional, type=boolean, default=true()
;        if true(), then generate the images in postscript and use
;        imagemagick to convert after the fact")

(defun mhs-idlwave-insert-do_ps-keyword ()
  "* Insert the same tired comment for every do_ps keyword."
  (interactive)
  (insert mhs-idlwave-do_ps-keyword))


;; This takes the place of what is below and does the same thing
;; If you surround  a word on a blank line with the point and mark
;;  <<point>>keyword<<mark>> 

;; and type mhs-setdefaultvalue, the following will be replaced and the point will be
;; ready to take the default value.  I use this for generating default values
;; in idl subroutines.

;;   SetDefaultValue,keyword, <<point>>
(defun mhs-setdefaultvalue (beg end)
  "does the typing for SetDefaultValue and positions the cursor where it belongs"
  (interactive "r")
  (let (input-string)
    (setq input-string (buffer-substring beg end))
    (kill-region beg end)
    (push-mark)
    (insert (copy-sequence  "SetDefaultValue, "))
    (end-of-line)
    (insert input-string)
    (end-of-line)
    (insert (copy-sequence ", "))
    (push-mark)
    (exchange-point-and-mark)))



;; This is a way to get my keyword expansion.
;; I would bind this to a key that you can use easily.

;; If you surround  a word on a blank line with the point and mark
;;  <<point>>keyword<<mark>> 

;; and type mhs-keywords, the following will be replaced and the point will be
;; ready to take the default value.  I use this for generating default values
;; in idl subroutines.

;;   keyword = n_elements( keyword ) eq 0 ? <<point>> : keyword

(defun mhs-keywords (beg end)
  "Inserts keyword initialization for idl"
  (interactive "r")
  (let (input-string) 
    (setq input-string (buffer-substring beg end))
    (kill-region beg end)
    (push-mark)
    (insert (copy-sequence  "item = n_elements( item ) eq 0 ? "))
    (exchange-point-and-mark)
    (while (search-forward "item" nil t)
      (replace-match input-string))
    (end-of-line)
    (push-mark)
    (insert (copy-sequence ": "))
    (insert input-string)
    (exchange-point-and-mark) ))



;; I put this here to override the new .full_reset_session behavior.
(defun mhs-idlwave-shell-reset (&optional hidden)
  "Reset IDL.  Return to main level and destroy the leftover variables.
This issues the following commands:  
RETALL
WIDGET_CONTROL,/RESET
CLOSE, /ALL
HEAP_GC, /VERBOSE"
  ;; OBJ_DESTROY, OBJ_VALID()  FIXME: should this be added?
  (interactive "P")
  (when (or idlwave-shell-reset-no-prompt 
	    (yes-or-no-p "Really Reset IDL and discard current session? "))
    (message "Resetting IDL")
    (setq idlwave-shell-calling-stack-index 0)
    ;; Give widget exit handlers a chance
    (idlwave-shell-send-command "retall" nil hidden)
    (idlwave-shell-send-command "widget_control,/reset" nil hidden)
    (idlwave-shell-send-command "close,/all" nil hidden)
    ;; (idlwave-shell-send-command "obj_destroy, obj_valid()" nil hidden)
    (idlwave-shell-send-command "heap_gc,/verbose" nil hidden)
    (idlwave-shell-display-line nil)))

(provide 'emacs-idlwave-support)
;;; EMACS-IDLWAVE-SUPPORT.el ends here
