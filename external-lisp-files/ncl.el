;********************************************
; Lisp code for an NCL major mode
;********************************************
; Revision 0.2 
; Changes between 0.1 and 0.2 Provided by Heiko Klein of Norway

; August 19 2003 Sylvia Murphy 
; National Center for Atmospheric Research
; Does text highlighting for NCL reserved words, built-in functions, 
; gsn* functions, contributed and shea-util functions, text, and comments.
; Does automatic indenting between begin and end statments, and within
; do loops and if statements.
; 
; Emacs has a lot more power that these functions. I do not use that
; functionality, so i did not spend any more time trying to add abbreviations,
; special keymaps etc.
;
; KNOWN PROBLEMS THAT VERSION 0.2 fixed
; 1) Works with xemacs 21.*
; 2) Works with emacs 20.*

; KNOWN PROBLEMS in Version 0.1
; 1) Only partially works with emacs version 20.3.2
;    a) highlights only comments and text, and only after tabs
;    b) indentation appears to work
; 2) Does not work with xemacs
; 3) Not all NCL built-in functions are highlighted. I listed MY favorite 
;    ones.
; 4) Have not demonstrated how to change the indentation value in .emacs
; 5) The ncl-in-comment function does not work. Its calls are commented out.
;********************************************
; HOW TO USE
;********************************************
; 1) place this file somewhere on your local system e.g. ~your_home/bin
; 2) in your .emacs file add the following (without the comments):
  ;(setq auto-mode-alist (cons '("\.ncl$" . ncl-mode) auto-mode-alist))
  ;(autoload 'ncl-mode "LOCATION/ncl.el") 
  ;(add-hook 'ncl-mode-hook
;      (lambda ()  
      ;;setup display colors for font-lock
;        (set-face-foreground font-lock-comment-face "FireBrick")
;        (set-face-foreground font-lock-string-face  "Salmon")
;        (set-face-foreground font-lock-keyword-face "Purple")
; select built-in functions
;        (set-face-foreground font-lock-function-name-face "Blue") 
; gsn* functions
;        (set-face-foreground font-lock-variable-name-face "SteelBlue")
; shea_util and contributed functions
;        (set-face-foreground font-lock-reference-face  "CadetBlue")
;       )
;   )
;*******************************************************************
; comment to self: functions in lisp are appears as an array within parethesis.
; e.g. x+y would be (+ x y) with the function name first and then the
; arguments. e.g. (global-set-key keysequence command) 
;********************************************************************
(defvar ncl-mode-hook nil
  "*List of functions to call when entering ncl mode.")

(defvar ncl-font-lock-keywords
  ;;keywords
  '(
    ;; comments. the period (.) means a ; and any character after it except a 
    ;; newline while the asterix (*) means repeated all occurances.
    ("\\(;.*\\)" 1 font-lock-comment-face )   

    ("\\<\\(begin\\|end\\|if\\|then\\|else\\|do\\)\\>" 1 font-lock-keyword-face)
    ;; gsn csm plot templates and special gsn functions
    ("\\<\\(gsn_polyline\\|gsn_csm_xy\\|gsn_csm_map\\|gsn_csm_map_ce\\|gsn_csm_map_polar\\|gsn_csm_contour_map_ce\\|gsn_csm_contour_map_polar\\|gsn_csm_contour_map_overlay\\|gsn_add_polygon\\|gsn_add_polyline\\|gsn_add_polymarker\\|gsn_add_text\\|gsn_add_text_ndc\\|gsn_legend_ndc\\|gsn_polygon\\|gsn_polygon_ndc\\|gsn_polyline\\|gsn_polyline_ndc\\|gsn_polymarker\\|gsn_polymarker_ndc\\|gsn_text_ndc\\|gsn_text\\|gsn_csm_xy\\|gsn_csm_hov\\|gsn_csm_lat_time\\|gsn_csm_time_lat\\|gsn_csm_pres_hgt\\|gsn_histogram\\|gsn_csm_streamline\\|gsn_csm_streamline_map_ce||\gsn_csm_streamline_map_polar\\|gsn_csm_streamline_map\\|gsn_csm_streamline_contour_map_ce\\|gsn_csm_streameline_contour_map_polar\\|gsn_csm_streamline_contour_map\\|gsn_csm_pres_hgt_streamline\\|gsn_csm_vector\\|gsn_csm_vector_map_ce\\|gsn_csm_vector_map_polar\\|gsn_csm_vector_map\\|gsn_csm_pres_hgt_vector\\|gsn_csm_vector_scalar_map_ce\\|gsn_csm_vector_scalar_map_polar\\|gsn_csm_vector_scalar_map\\|gsn_open_wks\\|gsn_panel\\|gsn_define_colormap\\|ngezlogo\\|nglogo\\|NhlNewColor\\|gsn_reverse_colormap\\|gsn_csm_y\\|gsn_csm_contour_map\\|gsn_csm_contour\\|\\)\\>" 1 font-lock-variable-name-face)

    ;; contributed functions
    ("\\<\\(addfiles_GetVar\\|assignFillValue\\|calcMonAnom\\|changeCase\\|changeCaseChar\\|climMonLLT\\|climMonLLLT\\|clmMonTLL\\|clmMonTLLL\\|closet_val\\|copy_VarAtts\\|copy_VarCoords\\|copy_VarCoords_1\\|copy_VarCoords_2\\|copy_VarMeta\\|copyatt\\|cshstringtolist\\|decimalPlaces\\|epsZero\\|fbinseqSwap1\\|fbinseqSwap2\\|ftl2dble\\|get1Dindex\\|get1Dindex_Collapse\\|GetFillColor\\|getFillValue\\|lonFlip\\|lonPivot\\|month_to_season\\|month_to_seaonN\\|month_to_season12\\|RGBtoCmap\\|rmInsufData\\|rmAnnCycle1D\\|rmMonAnncyc\\|short2flt\\|stdMon\\|SqrtCosWeight\\|transpose\\|uv2vrG_Wrap\\|zonalAve\\|\\)\\>" 1 font-lock-reference-face)

    ;;shea_util functions
    ("\\<\\(ZeroLineContour\\|ZeroNegDashLineContour\\|ZeroNegDAShLineContourOverlay\\|ZeroGoneNegDashLineContour\\|NegDashLineContour\\|SetZeroLineThickness\\|ColorNegDashZeroPosContour\\|ColorShadeLeGeContour\\|ShadeLtContour\\|ShadeGtContour\\|ShadeLtGtContour\\|ShadeGeLeContour\\|genCmapManualRes\\|genCmapMnMxCnInt\\|genCmapMnMxSpan\\|genCmapCnLvl\\|infoTimeStamp\\|msgValOutline\\|printVarInfo\\|specx_ci\\|specsy_ci\\|add90LatX\\|add90LatY\\|plot_xy2\\|drawNDCGrid\\|\\)\\>" 1 font-lock-reference-face )

    ;; favorite ncl built-in functions
    ("\\<\\(new\\|addfile\\|addfiles\\|all\\|any\\|asciiread\\|asciiwrite\\|avg\\|cbinread\\|cbinwrite\\|chartofloat\\|chartointeger\\|chartostring\\|conform\\|craybinnumrec\\|craybinrecread\\|cz2ccsm\\|delete\\|dim_avg\\|dim_max\\|dim_min\\|dim_product\\|dim_sum\\|dim_variance\\|dimsizes\\|doubletofloat\\|dpres_hybrid_ccm\\|eof_varimax\\|eofcor\\|eofcor_pcmsg\\|eofcor_ts\\|eofcov\\|eofcov_pcmsg\\|eofcov_ts\\|esacr\\|esacv\\|esccr\\|esccv\\|escorc\\|exp\\|f2fosh\\|f2foshv\\|f2fsh\\|f2fshv\\|f2gsh\\|f2gshv\\|fbindirread\\|finbindirwrite\\|fbinnumred\\|fbinread\\|fbinrecread\\|fbinrecwrite\\|fbinwrite\\|fileattdef\\|filedimdef\\|flevarattdef\\|filevardef\\|filevardimsizes\\|fo2fsh\\|fo2fshv\\|fspan\\|ftest\\|g2gsh\\|g2gshv\\|gc_latlon\\|getenv\\|getfilevaratts\\|getfilevardims\\|getfilevarnames\\|getvaratts\\|ind\\|int2p\\|isatt\\|ismissing\\|ispan\\|linint1\\|linint2\\|linint2_points\\|local_min\\|mask\\|max\\|min\\|natgrid\\|ndtooned\\|onedtond\\|overlay\\|paleo_outline\\|pop_remap\\|pres_hybrid_ccm\\|pres_sigma\\|print\\|printVarSummary\\|qsort\\|rand\\|rcm2points\\|rcm2grid\\|relhum\\|rtest\\|runave\\|sigma2hybrid\\|sizeof\\|smth9\\|specx_anal\\|specxy_anal\\|sprintf\\|sprinti\\|sqrt\\|sqsort\\|stddev\\|stringtochar\\|stringtointeger\\|sum\\|svdcov\\|svdcov_sv\\|svdstd\\|svdstd_sv\\|system\\|systemfunc\\|ttest\\|typeof\\|undef\\|uv2dvF\\|uv2dvG\\|uv2sfvpf\\|uv2sfvpg\\|uv2vr_cfd\\|uv2vrdvf\\|uv2vrdvg\\|uv2vrF\\|uv2vrf\\|uv2vrG\\|uv2vrg\\|variance\\|vinth2p\\|vinth2p_ecmwf\\|vr2uvf\\|vr2uvF\\|vr2uvg\\|vr2uvG\\|wavelet\\|wgt_areaave\\|wgt_arearmse\\|wgt_runave\\|wgt_volave\\|wgt_volave_ccm\\|wgt_volrmse\\|wgt_volrmse_ccm\\|zonal_mpsi\\|\\)\\>" 1 font-lock-function-name-face)

    )
  "words used in ncl-mode highlighting"
  )

(put 'ncl-mode 'font-lock-defaults 'ncl-font-lock-keywords)
;;************************************************
;; some variables used in the creation of ncl-mode
;;************************************************
(defvar ncl-mode-map nil
  "Keymap used in NCL mode.")
(defvar ncl-startup-message t
  "*Non-nil displays a startup message when `ncl-mode' is first called.")
(defconst ncl-mode-version "$Revision: 19505 $")
;;************************************************
;; syntax table
;;************************************************
;; characters are preceeded by a ?.  The "." indicates
;; the symbol is punctuation, "_" indicates a symbol
;; "" indicates a string, "<" means a comment

(defvar ncl-mode-syntax-table nil
  "Syntax table in use in `ncl-mode' buffers.")
(if ncl-mode-syntax-table ()
  (setq ncl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\;  "<"  ncl-mode-syntax-table)
  (modify-syntax-entry ?+   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?-   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?*   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?/   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?^   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?#   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?=   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?%   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?<   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?>   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?\'  "\"" ncl-mode-syntax-table)
  (modify-syntax-entry ?\"  "\"" ncl-mode-syntax-table)
  (modify-syntax-entry ?\\  "." ncl-mode-syntax-table)
  (modify-syntax-entry ?_   "_"  ncl-mode-syntax-table)
  (modify-syntax-entry ?{   "\(}"  ncl-mode-syntax-table)
  (modify-syntax-entry ?}   "\){"  ncl-mode-syntax-table)
  (modify-syntax-entry ?$   "_"  ncl-mode-syntax-table)
  (modify-syntax-entry ?.   "."  ncl-mode-syntax-table)
  (modify-syntax-entry ?\n  ">"  ncl-mode-syntax-table)
  (modify-syntax-entry ?\f  ">"  ncl-mode-syntax-table))

(defvar ncl-find-symbol-syntax-table nil
  "Syntax table that treats symbol characters as word characters.")

(if ncl-find-symbol-syntax-table ()
  (setq ncl-find-symbol-syntax-table
        (copy-syntax-table ncl-mode-syntax-table))
  )
;;****************************************************************************
;; keymap
;;****************************************************************************
(defvar ncl-mode-map nil
  "Keymap used in NCL mode.")
(if ncl-mode-map ()
  (setq ncl-mode-map (make-sparse-keymap))
  (define-key ncl-mode-map "\t"       'ncl-indent-line))
;;****************************************************************************
;; indenting variables
;;****************************************************************************
(defvar ncl-main-block-indent 2
  "*Extra indentation for the main block of code. That is the block between 
the begin statement and the end statement.")

(defvar ncl-main-block-end -2
  "*The offset that places the end statement back on the left margin. This is
the negative of `ncl-main-block-indent`")

(defvar ncl-block-indent 2
  "*Extra indentation for do loops.")

(defvar ncl-block-end -2
  "*The offset that places the `end do` statement back to it origin.")

(defconst ncl-comment-line-start-skip "^[ \t]*;"
  "Regexp to match the start of a full-line comment. That is the 
_beginning_ of a line containing a comment delmiter `\;' preceded 
only by whitespace.")

;; defconst are constants that never change 
;; the \` matches only those at the beginning of the buffer and no other
(defconst ncl-begin "\\<\\(begin\\)\\>\\|\\`" 
  "Regular expression to find the begin statement.")

;; the \' matches only those at the end of the buffer and no other
(defconst ncl-end "\\<\\(^end$\\)\\>\\|\\'" 
  "Regular expression to find the line that indicates the end of a
script.")

(defconst ncl-begin-do "^[ /t]*do" 
  "Regular expression to find the beginning of a do loop.")

(defconst ncl-else "^[ /t]*else" 
  "Regular expression to find an else statment.")

(defconst ncl-begin-if "^[ /t]*if" 
  "Regular expression to find the beginning of a if statment.")

(defconst ncl-enddo "end[ ]do"
  "Regular expression to find the end of a do loop")

(defconst ncl-endif "end[ ]if"
  "Regular expression to find the end of a if statement")

(defconst ncl-identifier "[a-zA-Z][a-zA-Z0-9$_]+"
  "Regular expression matching an NCL identifier.")

(defconst ncl-label (concat ncl-identifier ":")
  "Regular expression matching NCL labels.")

(defvar ncl-no-change-comment ";;"
  "*The indentation of a comment that starts with this regular
expression will not be changed. Note that the indentation of a comment
at the beginning of a line is never changed.")

(defvar ncl-begin-line-comment nil
  "*A comment anchored at the beginning of line.
A comment matching this regular expression will not have its
indentation changed.  If nil the default is \"^\;\", i.e., any line
beginning with a \"\;\".  Expressions for comments at the beginning of
the line should begin with \"^\".")

(defvar ncl-code-comment ";;[^;]"
  "*A comment that starts with this regular expression on a line by
itself is indented as if it is a part of NCL code.  As a result if
the comment is not preceded by whitespace it is unchanged.")
;;****************************************************************************
;; indenting functions
;;****************************************************************************
(defun ncl-beginning-of-statement ()
  "Move to beginning of the current statement. Skips back past statement 
continuations. Point is placed at the beginning of the line whether or not 
this is an actual statement."
  (if (save-excursion (forward-line -1) (ncl-is-continuation-line))
      (ncl-previous-statement)
    (beginning-of-line)))

(defun ncl-end-of-statement ()
  "Moves point to the end of the current NCL statement. If not in a statement 
just moves to end of line. Returns position."
  (interactive)
  (while (and (ncl-is-continuation-line)
              (= (forward-line 1) 0)))
  (end-of-line) (point))

(defun ncl-previous-statement ()
  "Moves point to beginning of the previous statement. Returns t if the 
current line before moving is the beginning of the first non-comment 
statement in the file, and nil otherwise."
  (interactive)
  (let (first-statement)
    (if (not (= (forward-line -1) 0))
        ;; first line in file
        t
      ;; skip blank lines, label lines, include lines and line comments
      (while (and 
              ;; The current statement is the first statement until we
              ;; reach another statement.
              (setq first-statement
                    (or 
                     (looking-at ncl-comment-line-start-skip)
                     (looking-at "[ \t]*$")
                     (looking-at (concat "[ \t]*" ncl-label "[ \t]*$"))
                     (looking-at "^@")))
              (= (forward-line -1) 0)))
      ;; skip continuation lines
      (while (and 
              (save-excursion
                (forward-line -1)
                (ncl-is-continuation-line))
              (= (forward-line -1) 0)))
      first-statement)))

(defun ncl-is-continuation-line ()
  "Tests if current line is continuation line."
  (save-excursion
    (ncl-look-at "\\<\\$")))

(defun ncl-look-at (regexp &optional cont beg)
  "Searches current line from current point for the regular expression REGEXP.
If optional argument CONT is non-nil, searches to the end of the current 
statement. If optional arg BEG is non-nil, search starts from the beginning 
of the current statement. Ignores matches that end in a comment or inside a 
string expression. Returns point if successful, nil otherwise.  This function 
produces unexpected results if REGEXP contains quotes or a comment delimiter. 
The search is case insensitive.  If successful leaves point after the match, 
otherwise, does not move point."
  (let ((here (point))
        (old-syntax-table (syntax-table))
        (case-fold-search t)
        eos
        found)
    (set-syntax-table ncl-find-symbol-syntax-table)
    (setq eos
          (if cont
              (save-excursion (ncl-end-of-statement) (point))
            (save-excursion (end-of-line) (point))))
    (if beg (ncl-beginning-of-statement))
    (while (and (setq found (re-search-forward regexp eos t))
                (ncl-quoted)))
    (set-syntax-table old-syntax-table)
    (if (not found) (goto-char here))
    found))
 
(defun ncl-in-quote ()
  "Returns location of the opening quote if point is in a NCL string constant,
nil otherwise. Ignores comment delimiters on the current line. Properly 
handles nested quotation marks and octal constants - a double quote followed 
by an octal digit."
;;; Treat an octal inside an apostrophe to be a normal string. Treat a
;;; double quote followed by an octal digit to be an octal constant
;;; rather than a string. Therefore, there is no terminating double
;;; quote.
  (save-excursion
    ;; Because single and double quotes can quote each other we must
    ;; search for the string start from the beginning of line.
    (let* ((start (point))
           (eol (progn (end-of-line) (point)))
           (bq (progn (beginning-of-line) (point)))
           (endq (point))
           (data (match-data))
           delim
           found)
          (while  (< endq start)
            ;; Find string start
            ;; Don't find an octal constant beginning with a double quote
            (if (re-search-forward "\"[^0-7]\\|'\\|\"$" eol 'lim)
                ;; Find the string end. In NCL, two consecutive delimiters 
		;; after the start of a string act as an escape for the 
                ;; delimiter in the string. Two consecutive delimiters alone 
		;; (i.e., not after the start of a string) is the the 
		;; null string.
                (progn 
                  ;; Move to position after quote
                  (goto-char (1+ (match-beginning 0)))
                  (setq bq (1- (point)))
                  ;; Get the string delimiter
                  (setq delim (char-to-string (preceding-char)))
                  ;; Check for null string
                  (if (looking-at delim)
                      (progn (setq endq (point)) (forward-char 1))
                    ;; Look for next unpaired delimiter
                    (setq found (search-forward delim eol 'lim))
                    (while (looking-at delim)
                      (forward-char 1)
                      (setq found (search-forward delim eol 'lim)))
                    (if found
                        (setq endq (- (point) 1))
                      (setq endq (point)))
                    ))
              (progn (setq bq (point)) (setq endq (point)))))
          (store-match-data data)
      ;; return string beginning position or nil
      (if (> start bq) bq))))


(defun ncl-quoted ()
  "Returns t if point is in a comment or quoted string. nil otherwise."
;  (or (ncl-in-comment) (ncl-in-quote)))
  (or (ncl-in-quote)))

(defun ncl-in-comment ()
  "Returns t if point is inside a comment, nil otherwise."
  (save-excursion
    (let ((here (point)))
      (and (ncl-goto-comment) (> here (point))))))

(defun ncl-goto-comment ()
  "Move to start of comment delimiter on current line. Moves to end of line if
there is no comment delimiter. Ignores comment delimiters in strings. Returns 
point if comment found and nil otherwise."
  (let ((eos (progn (end-of-line) (point)))
        (data (match-data))
        found)
    ;; Look for first comment delimiter not in a string
    (beginning-of-line)
    (setq found (search-forward comment-start eos 'lim))
    (while (and found (ncl-in-quote))
      (setq found (search-forward comment-start eos 'lim)))
    (store-match-data data)
    (and found (not (ncl-in-quote))
         (progn
           (backward-char 1)
           (point)))))

(defun ncl-current-statement-indent ()
  "Return indentation of the current statement. If in a statement, moves to 
beginning of statement before finding indent."
  (ncl-beginning-of-statement)
  (ncl-current-indent))

(defun ncl-current-indent ()
  "Return the column of the indentation of the current line.  Skips any 
whitespace. Returns 0 if the end-of-line follows the whitespace."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    ;; if we are at the end of blank line return 0
    (cond ((eolp) 0)
          ((current-column)))))

(defun ncl-calculate-indent ()
  "Return appropriate indentation for current line as NCL code."
  (save-excursion
    (beginning-of-line)
    (cond 
     ;; if line is "begin" do nothing and exit
     ((ncl-look-at ncl-begin) 0)
     ;; calculate indent based on previous and current statements
     (t (let ((the-indent
	      ;; calculate indent based on previous statement
	      (save-excursion
		(cond
		 ;; retreive the previous statement
		 ( (ncl-previous-statement) 0)

		 ;; indent if previous statment is begin
		 ((ncl-look-at ncl-begin t)
		  (+ (ncl-current-statement-indent) ncl-main-block-indent))
		 
		 ;; indent if previous statment is do 
		 ((ncl-look-at ncl-begin-do t)
		  (+ (ncl-current-statement-indent) ncl-block-indent))

		 ;; indent if previous statment is if 
		 ((ncl-look-at ncl-begin-if t)
		  (+ (ncl-current-statement-indent) ncl-block-indent))

		 ;; indent if previous statment is else
		 ((ncl-look-at ncl-else t)
		  (+ (ncl-current-statement-indent) ncl-block-indent))

		 ((ncl-current-statement-indent))))))
	  ;; adjust the indentation based on the current statement
	  (cond
	   ;; do loop
	   ((ncl-look-at ncl-enddo t)
	    (+ the-indent ncl-block-end))
	   ;; if statement
	   ((ncl-look-at ncl-endif t)
	    (+ the-indent ncl-block-end))
	   ;; else statement
	   ((ncl-look-at ncl-else t)
	    (+ the-indent ncl-block-end))

	   ;; End block
	   ((ncl-look-at ncl-end t)
	    (+ the-indent ncl-main-block-end)) ;; end gets negative indent
	   (the-indent))

	  )))))

(defun ncl-indent-to (col &optional min)
  "Indent from point with spaces until column COL. Inserts space before 
markers at point."
  (if (not min) (setq min 0))
  (insert-before-markers
   (make-string (max min (- col (current-column))) ? )))

(defun ncl-indent-left-margin (col)
  "Indent the current line to column COL. Indents such that first 
non-whitespace character is at column COL. Inserts spaces before markers at 
point."
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (ncl-indent-to col)))

(defun ncl-comment-hook ()
  "Compute indent for the beginning of the NCL comment delimiter."
  (if (or (looking-at ncl-no-change-comment)
          (if ncl-begin-line-comment
              (looking-at ncl-begin-line-comment)
              (looking-at "^\;")))
      (current-column)
    (if (looking-at ncl-code-comment)
        (if (save-excursion (skip-chars-backward " \t") (bolp))
            ;; On line by itself, indent as code
            (let ((tem (ncl-calculate-indent)))
              (if (listp tem) (car tem) tem))
          ;; after code - do not change
          (current-column))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           comment-column))))

(defun ncl-indent-line ()
  "Indents current NCL line as code or as a comment."
  (interactive)
  ;; Move point out of left margin.
  (if (save-excursion
        (skip-chars-backward " \t")
        (bolp))
      (skip-chars-forward " \t"))
  (let ((mloc (point-marker)))
    (save-excursion
      (beginning-of-line)
      (if (looking-at ncl-comment-line-start-skip)
          ;; Indentation for a line comment
          (progn
            (skip-chars-forward " \t")
            (ncl-indent-left-margin (ncl-comment-hook)))
        ;; Indent for code line
        (beginning-of-line)
        (if (or
             ;; a label line
             (looking-at (concat "^" ncl-label "[ \t]*$"))
             ;; a batch command
             (looking-at "^[ \t]*@"))
            ;; leave flush left
            nil
          ;; indent the line
          (ncl-indent-left-margin (ncl-calculate-indent)))
        ;; Adjust parallel comment
;        (end-of-line) 
;        (if (ncl-in-comment)
;            (indent-for-comment))
	))
    (goto-char mloc)
    ;; Get rid of marker
    (set-marker mloc nil)
    ))
;;****************************************************************************
;; define ncl mode
;;****************************************************************************
(defun ncl-mode ()
  "Major mode for editing NCL .ncl files"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'ncl-mode)
  (setq mode-name "NCL")

  (if ncl-startup-message
      (message "Emacs NCL mode version %s." ncl-mode-version)
    ) 
;**************************
;; indentation
;**************************
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ncl-indent-line)
  (use-local-map ncl-mode-map)
;**************************
;; these ensure syntax hightlighting
;**************************
;; font-lock setup for various emacs: XEmacs, Emacs 19.29+, Emacs <19.29.
;; taken from html-helper-mode, adapted to ncl
  (cond	((string-match "XEmacs\\|Lucid" (emacs-version)) ; XEmacs/Lucid
	 (put major-mode 'font-lock-keywords-case-fold-search t)
	 )
	;; XEmacs (19.13, at least) guesses the rest correctly.
	;; If any older XEmacs don't, then tell me.
	;;
	((string-lessp "19.28.89" emacs-version) ; Emacs 19.29 and later
	 (make-local-variable 'font-lock-defaults)
	 (setq font-lock-defaults '(ncl-font-lock-keywords t t)))
	;;
	(t ; Emacs 19.28 and older
	 (make-local-variable 'font-lock-keywords-case-fold-search)
	 (make-local-variable 'font-lock-keywords)
	 (make-local-variable 'font-lock-no-comments)
	 (setq font-lock-keywords-case-fold-search t)
	 (setq font-lock-keywords ncl-font-lock-keywords)
	 (setq font-lock-no-comments t)))

  (font-lock-mode 1)
  (setq font-lock-maximum-decoration t)
;  (make-local-variable 'font-lock-defaults)
;  (setq font-lock-defaults 'ncl-keywords)
;  (make-local-variable 'comment-start)
;  (setq comment-start ";")
; turn this on if debuging this code
;   (setq debug_on_error t)
  (set-syntax-table ncl-mode-syntax-table)
  (run-hooks 'ncl-mode-hook)
  )
;;************************************************************************
  (provide 'ncl)
