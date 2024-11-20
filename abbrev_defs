;;-*-coding: utf-8;-*-
(define-abbrev-table 'awk-mode-abbrev-table
  '(
    ("else" "else" c-electric-continued-statement :count 0)
    ("while" "while" c-electric-continued-statement :count 0)
   ))

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("catch" "catch" c-electric-continued-statement :count 0)
    ("else" "else" c-electric-continued-statement :count 0)
    ("while" "while" c-electric-continued-statement :count 0)
   ))

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ("else" "else" c-electric-continued-statement :count 0)
    ("while" "while" c-electric-continued-statement :count 0)
   ))

(define-abbrev-table 'cperl-mode-abbrev-table
  '(
    ("=head1" "=head1" cperl-electric-pod :count 0)
    ("=head2" "=head2" cperl-electric-pod :count 0)
    ("=over" "=over" cperl-electric-pod :count 0)
    ("=pod" "=pod" cperl-electric-pod :count 0)
    ("continue" "continue" cperl-electric-else :count 0)
    ("do" "do" cperl-electric-keyword :count 0)
    ("else" "else" cperl-electric-else :count 2)
    ("elsif" "elsif" cperl-electric-keyword :count 0)
    ("for" "for" cperl-electric-keyword :count 7)
    ("foreach" "foreach" cperl-electric-keyword :count 0)
    ("foreachmy" "foreachmy" cperl-electric-keyword :count 0)
    ("formy" "formy" cperl-electric-keyword :count 0)
    ("head1" "head1" cperl-electric-pod :count 0)
    ("head2" "head2" cperl-electric-pod :count 0)
    ("if" "if" cperl-electric-keyword :count 26)
    ("over" "over" cperl-electric-pod :count 1)
    ("pod" "pod" cperl-electric-pod :count 0)
    ("unless" "unless" cperl-electric-keyword :count 0)
    ("until" "until" cperl-electric-keyword :count 0)
    ("while" "while" cperl-electric-keyword :count 0)
   ))

(define-abbrev-table 'f90-mode-abbrev-table
  '(
    ("`ab" "allocatable" nil :count 0)
    ("`al" "allocate" nil :count 0)
    ("`as" "assignment" nil :count 0)
    ("`ba" "backspace" nil :count 0)
    ("`bd" "block data" nil :count 0)
    ("`c" "character" nil :count 0)
    ("`cl" "close" nil :count 0)
    ("`cm" "common" nil :count 0)
    ("`cn" "contains" nil :count 0)
    ("`cx" "complex" nil :count 0)
    ("`cy" "cycle" nil :count 0)
    ("`de" "deallocate" nil :count 0)
    ("`df" "define" nil :count 0)
    ("`di" "dimension" nil :count 0)
    ("`dw" "do while" nil :count 0)
    ("`el" "else" nil :count 0)
    ("`eli" "else if" nil :count 0)
    ("`elw" "elsewhere" nil :count 0)
    ("`eq" "equivalence" nil :count 0)
    ("`ex" "external" nil :count 0)
    ("`ey" "entry" nil :count 0)
    ("`fa" ".false." nil :count 0)
    ("`fl" "forall" nil :count 0)
    ("`fo" "format" nil :count 0)
    ("`fu" "function" nil :count 0)
    ("`i" "integer" nil :count 0)
    ("`if" "interface" nil :count 0)
    ("`im" "implicit none" nil :count 0)
    ("`in " "include" nil :count 0)
    ("`it" "intent" nil :count 0)
    ("`lo" "logical" nil :count 0)
    ("`mo" "module" nil :count 0)
    ("`na" "namelist" nil :count 0)
    ("`nu" "nullify" nil :count 0)
    ("`op" "optional" nil :count 0)
    ("`pa" "parameter" nil :count 0)
    ("`pi" "private" nil :count 0)
    ("`pm" "program" nil :count 0)
    ("`po" "pointer" nil :count 0)
    ("`pr" "print" nil :count 0)
    ("`pu" "public" nil :count 0)
    ("`r" "real" nil :count 0)
    ("`rc" "recursive" nil :count 0)
    ("`rt" "return" nil :count 0)
    ("`rw" "rewind" nil :count 0)
    ("`se" "select" nil :count 0)
    ("`sq" "sequence" nil :count 0)
    ("`su" "subroutine" nil :count 0)
    ("`t" "type" nil :count 0)
    ("`ta" "target" nil :count 0)
    ("`tr" ".true." nil :count 0)
    ("`wh" "where" nil :count 0)
    ("`wr" "write" nil :count 0)
   ))

(define-abbrev-table 'fortran-mode-abbrev-table
  '(
    (";au" "automatic" nil :count 0)
    (";b" "byte" nil :count 0)
    (";bd" "block data" nil :count 0)
    (";c" "continue" nil :count 0)
    (";ch" "character" nil :count 0)
    (";cl" "close" nil :count 0)
    (";cm" "common" nil :count 0)
    (";cx" "complex" nil :count 0)
    (";dc" "double complex" nil :count 0)
    (";df" "define" nil :count 0)
    (";di" "dimension" nil :count 0)
    (";do" "double" nil :count 0)
    (";dp" "double precision" nil :count 0)
    (";dw" "do while" nil :count 0)
    (";e" "else" nil :count 0)
    (";ed" "enddo" nil :count 0)
    (";el" "elseif" nil :count 0)
    (";en" "endif" nil :count 0)
    (";eq" "equivalence" nil :count 0)
    (";ew" "endwhere" nil :count 0)
    (";ex" "external" nil :count 0)
    (";ey" "entry" nil :count 0)
    (";f" "format" nil :count 0)
    (";fa" ".false." nil :count 0)
    (";fu" "function" nil :count 0)
    (";g" "goto" nil :count 0)
    (";ib" "implicit byte" nil :count 0)
    (";ic" "implicit complex" nil :count 0)
    (";ich" "implicit character" nil :count 0)
    (";ii" "implicit integer" nil :count 0)
    (";il" "implicit logical" nil :count 0)
    (";im" "implicit" nil :count 0)
    (";in" "integer" nil :count 0)
    (";inc" "include" nil :count 0)
    (";intr" "intrinsic" nil :count 0)
    (";ir" "implicit real" nil :count 0)
    (";l" "logical" nil :count 0)
    (";n" "namelist" nil :count 0)
    (";o" "open" nil :count 0)
    (";p" "print" nil :count 0)
    (";pa" "parameter" nil :count 0)
    (";pr" "program" nil :count 0)
    (";ps" "pause" nil :count 0)
    (";r" "read" nil :count 0)
    (";rc" "record" nil :count 0)
    (";re" "real" nil :count 0)
    (";rt" "return" nil :count 0)
    (";rw" "rewind" nil :count 0)
    (";s" "stop" nil :count 0)
    (";sa" "save" nil :count 0)
    (";sc" "static" nil :count 0)
    (";st" "structure" nil :count 0)
    (";su" "subroutine" nil :count 0)
    (";tr" ".true." nil :count 0)
    (";ty" "type" nil :count 0)
    (";vo" "volatile" nil :count 0)
    (";w" "write" nil :count 0)
    (";wh" "where" nil :count 0)
   ))

(define-abbrev-table 'global-abbrev-table
  '(
    ("geotiff" "GeoTIFF" nil :count 2)
    ("gh" "GitHub" nil :count 2)
    ("github" "GitHub" nil :count 1)
    ("gt" "GeoTIFF" nil :count 4)
    ("hb" "HyBIG" nil :count 26)
    ("hiab" "Harmony-In-A-Box" nil :count 7)
    ("hrt" "Harmony-Regression-Test" nil :count 1)
    ("hsl" "harmony-service-lib" nil :count 1)
    ("l2g" "L2G-Gridding-Service" nil :count 4)
    ("netcdf" "NetCDF" nil :count 7)
    ("sl2gr" "SMAP-L2-Gridder" nil :count 2)
    ("sl2gs" "SMAP-L2-Gridding-Service" nil :count 2)
    ("tjs" "Trajectory Subsetter" nil :count 2)
    ("varaible" "variable" nil :count 1)
   ))

(define-abbrev-table 'gnus-article-edit-mode-abbrev-table
  '(
    ("qcnw" "QC Navigation Window" nil :count 0)
   ))

(define-abbrev-table 'idlwave-mode-abbrev-table
  '(
    (".ap" "arg_present()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".b" "begin" (lambda nil (idlwave-check-abbrev 0 t)) :count 0)
    (".c" "" (lambda nil "(idlwave-case)" (if (idlwave-quoted) (progn (unexpand-abbrev) nil) (idlwave-case))) :count 0)
    (".cb" "byte()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".cc" "complex()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".cd" "double()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".cf" "float()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".cl" "long()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".co" "common" (lambda nil (idlwave-check-abbrev 0 t)) :count 0)
    (".cs" "string()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".cx" "fix()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".e" "else" (lambda nil (idlwave-check-abbrev 0 t)) :count 0)
    (".ec" "endcase" idlwave-show-begin :count 0)
    (".ee" "endelse" idlwave-show-begin :count 0)
    (".ef" "endfor" idlwave-show-begin :count 0)
    (".ei" "endif else if" idlwave-show-begin :count 0)
    (".el" "endif else" idlwave-show-begin :count 0)
    (".elif" "" (lambda nil "(idlwave-elif)" (if (idlwave-quoted) (progn (unexpand-abbrev) nil) (idlwave-elif))) :count 0)
    (".en" "endif" idlwave-show-begin :count 0)
    (".er" "endrep" idlwave-show-begin :count 0)
    (".es" "endswitch" idlwave-show-begin :count 0)
    (".ew" "endwhile" idlwave-show-begin :count 0)
    (".f" "" (lambda nil "(idlwave-for)" (if (idlwave-quoted) (progn (unexpand-abbrev) nil) (idlwave-for))) :count 0)
    (".fu" "" (lambda nil "(idlwave-function)" (if (idlwave-quoted) (progn (unexpand-abbrev) nil) (idlwave-function))) :count 0)
    (".g" "goto," (lambda nil (idlwave-check-abbrev 0 t)) :count 1)
    (".gu" "Get_UValue=" nil :count 5)
    (".gv" "Get_Value=" nil :count 1)
    (".h" "help," (lambda nil (idlwave-check-abbrev 0)) :count 0)
    (".i" "" (lambda nil "(idlwave-if)" (if (idlwave-quoted) (progn (unexpand-abbrev) nil) (idlwave-if))) :count 0)
    (".iap" "if arg_present() then" (lambda nil (idlwave-check-abbrev 6)) :count 0)
    (".ik" "if keyword_set() then" (lambda nil (idlwave-check-abbrev 6)) :count 0)
    (".ine" "if n_elements() eq 0 then" (lambda nil (idlwave-check-abbrev 11)) :count 0)
    (".inn" "if n_elements() ne 0 then" (lambda nil (idlwave-check-abbrev 11)) :count 0)
    (".k" "keyword_set()" (lambda nil (idlwave-check-abbrev 1)) :count 1)
    (".n" "n_elements()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".np" "n_params()" (lambda nil (idlwave-check-abbrev 0)) :count 0)
    (".od" "obj_destroy," (lambda nil (idlwave-check-abbrev 0)) :count 0)
    (".oi" "on_ioerror," (lambda nil (idlwave-check-abbrev 0 1)) :count 0)
    (".on" "Obj_New()" (lambda nil (idlwave-check-abbrev 1)) :count 3)
    (".or" "openr," (lambda nil (idlwave-check-abbrev 0)) :count 0)
    (".ou" "openu," (lambda nil (idlwave-check-abbrev 0)) :count 0)
    (".ow" "openw," (lambda nil (idlwave-check-abbrev 0)) :count 0)
    (".p" "print," (lambda nil (idlwave-check-abbrev 0)) :count 0)
    (".pn" "Ptr_New()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".pr" "" (lambda nil "(idlwave-procedure)" (if (idlwave-quoted) (progn (unexpand-abbrev) nil) (idlwave-procedure))) :count 0)
    (".pt" "plot," (lambda nil (idlwave-check-abbrev 0)) :count 0)
    (".r" "" (lambda nil "(idlwave-repeat)" (if (idlwave-quoted) (progn (unexpand-abbrev) nil) (idlwave-repeat))) :count 0)
    (".re" "read," (lambda nil (idlwave-check-abbrev 0)) :count 0)
    (".rf" "readf," (lambda nil (idlwave-check-abbrev 0)) :count 0)
    (".rt" "return" (lambda nil (idlwave-check-abbrev 0)) :count 0)
    (".ru" "readu," (lambda nil (idlwave-check-abbrev 0)) :count 0)
    (".s" "size()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".sc" "strcompress()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".sl" "strlowcase()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".sm" "strmid()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".sn" "strlen()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".sp" "strpos()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".sr" "strtrim()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".st" "strput()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".su" "Set_UValue =" nil :count 1)
    (".sw" "" (lambda nil "(idlwave-switch)" (if (idlwave-quoted) (progn (unexpand-abbrev) nil) (idlwave-switch))) :count 0)
    (".t" "then" (lambda nil (idlwave-check-abbrev 0 t)) :count 0)
    (".u" "until" (lambda nil (idlwave-check-abbrev 0 t)) :count 0)
    (".w" "" (lambda nil "(idlwave-while)" (if (idlwave-quoted) (progn (unexpand-abbrev) nil) (idlwave-while))) :count 0)
    (".wb" "Widget_Base()" (lambda nil (idlwave-check-abbrev 1)) :count 0)
    (".wc" "widget_control," (lambda nil (idlwave-check-abbrev 0)) :count 9)
    (".wi" "widget_info()" (lambda nil (idlwave-check-abbrev 1)) :count 4)
    (".wu" "writeu," (lambda nil (idlwave-check-abbrev 0)) :count 0)
   ))

(define-abbrev-table 'java-mode-abbrev-table
  '(
    ("catch" "catch" c-electric-continued-statement :count 0)
    ("else" "else" c-electric-continued-statement :count 0)
    ("finally" "finally" c-electric-continued-statement :count 0)
    ("while" "while" c-electric-continued-statement :count 0)
   ))

(define-abbrev-table 'jde-mode-abbrev-table
  '(
    ("catch" "catch" c-electric-continued-statement :count 0)
    ("else" "else" c-electric-continued-statement :count 0)
    ("finally" "finally" c-electric-continued-statement :count 0)
    ("while" "while" c-electric-continued-statement :count 0)
   ))

(define-abbrev-table 'latex-mode-abbrev-table
  '(
    ("qcnw" "QC Navigation Window" nil :count 0)
   ))

(define-abbrev-table 'log-edit-mode-abbrev-table
  '(
    ("qcnw" "QC Navigation Window" nil :count 0)
   ))

(define-abbrev-table 'message-mode-abbrev-table
  '(
    ("qcnw" "QC Navigation Window" nil :count 0)
   ))

(define-abbrev-table 'mh-letter-mode-abbrev-table
  '(
    ("qcnw" "QC Navigation Window" nil :count 0)
   ))

(define-abbrev-table 'objc-mode-abbrev-table
  '(
    ("else" "else" c-electric-continued-statement :count 0)
    ("while" "while" c-electric-continued-statement :count 0)
   ))

(define-abbrev-table 'octave-abbrev-table
  '(
    ("`a" "all_va_args" nil :count 0)
    ("`b" "break" nil :count 0)
    ("`c" "continue" nil :count 0)
    ("`ca" "catch" nil :count 0)
    ("`cs" "case" nil :count 0)
    ("`ef" "endfor" nil :count 0)
    ("`efu" "endfunction" nil :count 0)
    ("`ei" "endif" nil :count 0)
    ("`el" "else" nil :count 0)
    ("`eli" "elseif" nil :count 0)
    ("`es" "endswitch" nil :count 0)
    ("`et" "end_try_catch" nil :count 0)
    ("`eu" "end_unwind_protect" nil :count 0)
    ("`ew" "endwhile" nil :count 0)
    ("`f" "for" nil :count 0)
    ("`fu" "function" nil :count 0)
    ("`gl" "global" nil :count 0)
    ("`gp" "gplot" nil :count 0)
    ("`gs" "gsplot" nil :count 0)
    ("`if" "if ()" nil :count 0)
    ("`o" "otherwise" nil :count 0)
    ("`r" "return" nil :count 0)
    ("`rp" "replot" nil :count 0)
    ("`s" "switch" nil :count 0)
    ("`t" "try" nil :count 0)
    ("`up" "unwind_protect" nil :count 0)
    ("`upc" "unwind_protect_cleanup" nil :count 0)
    ("`w" "while ()" nil :count 0)
   ))

(define-abbrev-table 'outline-mode-abbrev-table
  '(
    ("qcnw" "QC Navigation Window" nil :count 0)
   ))

(define-abbrev-table 'pike-mode-abbrev-table
  '(
    ("else" "else" c-electric-continued-statement :count 0)
    ("while" "while" c-electric-continued-statement :count 0)
   ))

(define-abbrev-table 'plain-tex-mode-abbrev-table
  '(
    ("qcnw" "QC Navigation Window" nil :count 0)
   ))

(define-abbrev-table 'texinfo-mode-abbrev-table
  '(
    ("qcnw" "QC Navigation Window" nil :count 0)
   ))

(define-abbrev-table 'text-mode-abbrev-table
  '(
    ("qcnw" "QC Navigation Window" nil :count 4)
   ))

(define-abbrev-table 'vhdl-mode-abbrev-table
  '(
    ("--" "" vhdl-template-display-comment-hook :count 0)
    ("abs" "" vhdl-template-default-hook :count 0)
    ("access" "" vhdl-template-default-hook :count 0)
    ("after" "" vhdl-template-default-hook :count 0)
    ("alias" "" vhdl-template-alias-hook :count 0)
    ("all" "" vhdl-template-default-hook :count 0)
    ("and" "" vhdl-template-default-hook :count 0)
    ("arch" "" vhdl-template-architecture-hook :count 0)
    ("architecture" "" vhdl-template-architecture-hook :count 0)
    ("array" "" vhdl-template-default-hook :count 0)
    ("assert" "" vhdl-template-assert-hook :count 0)
    ("attr" "" vhdl-template-attribute-hook :count 0)
    ("attribute" "" vhdl-template-attribute-hook :count 0)
    ("begin" "" vhdl-template-default-indent-hook :count 0)
    ("block" "" vhdl-template-block-hook :count 0)
    ("body" "" vhdl-template-default-hook :count 0)
    ("buffer" "" vhdl-template-default-hook :count 0)
    ("bus" "" vhdl-template-default-hook :count 0)
    ("case" "" vhdl-template-case-hook :count 0)
    ("comp" "" vhdl-template-component-hook :count 0)
    ("component" "" vhdl-template-component-hook :count 0)
    ("cond" "" vhdl-template-conditional-signal-asst-hook :count 0)
    ("conditional" "" vhdl-template-conditional-signal-asst-hook :count 0)
    ("conf" "" vhdl-template-configuration-hook :count 0)
    ("configuration" "" vhdl-template-configuration-hook :count 0)
    ("cons" "" vhdl-template-constant-hook :count 0)
    ("constant" "" vhdl-template-constant-hook :count 0)
    ("disconnect" "" vhdl-template-disconnect-hook :count 0)
    ("downto" "" vhdl-template-default-hook :count 0)
    ("else" "" vhdl-template-else-hook :count 0)
    ("elseif" "" vhdl-template-elsif-hook :count 0)
    ("elsif" "" vhdl-template-elsif-hook :count 0)
    ("end" "" vhdl-template-default-indent-hook :count 0)
    ("entity" "" vhdl-template-entity-hook :count 0)
    ("exit" "" vhdl-template-exit-hook :count 0)
    ("file" "" vhdl-template-file-hook :count 0)
    ("for" "" vhdl-template-for-hook :count 0)
    ("func" "" vhdl-template-function-hook :count 0)
    ("function" "" vhdl-template-function-hook :count 0)
    ("generic" "" vhdl-template-generic-hook :count 0)
    ("group" "" vhdl-template-group-hook :count 0)
    ("guarded" "" vhdl-template-default-hook :count 0)
    ("if" "" vhdl-template-if-hook :count 0)
    ("impure" "" vhdl-template-default-hook :count 0)
    ("in" "" vhdl-template-default-hook :count 0)
    ("inertial" "" vhdl-template-default-hook :count 0)
    ("inout" "" vhdl-template-default-hook :count 0)
    ("inst" "" vhdl-template-instance-hook :count 0)
    ("instance" "" vhdl-template-instance-hook :count 0)
    ("is" "" vhdl-template-default-hook :count 0)
    ("label" "" vhdl-template-default-hook :count 0)
    ("library" "" vhdl-template-library-hook :count 0)
    ("linkage" "" vhdl-template-default-hook :count 0)
    ("literal" "" vhdl-template-default-hook :count 0)
    ("loop" "" vhdl-template-bare-loop-hook :count 0)
    ("map" "" vhdl-template-map-hook :count 0)
    ("mod" "" vhdl-template-default-hook :count 0)
    ("nand" "" vhdl-template-default-hook :count 0)
    ("new" "" vhdl-template-default-hook :count 0)
    ("next" "" vhdl-template-next-hook :count 0)
    ("nor" "" vhdl-template-default-hook :count 0)
    ("not" "" vhdl-template-default-hook :count 0)
    ("null" "" vhdl-template-default-hook :count 0)
    ("of" "" vhdl-template-default-hook :count 0)
    ("on" "" vhdl-template-default-hook :count 0)
    ("open" "" vhdl-template-default-hook :count 0)
    ("or" "" vhdl-template-default-hook :count 0)
    ("others" "" vhdl-template-default-hook :count 0)
    ("out" "" vhdl-template-default-hook :count 0)
    ("pack" "" vhdl-template-package-hook :count 0)
    ("package" "" vhdl-template-package-hook :count 0)
    ("port" "" vhdl-template-port-hook :count 0)
    ("postponed" "" vhdl-template-default-hook :count 0)
    ("procedure" "" vhdl-template-procedure-hook :count 0)
    ("process" "" vhdl-template-process-hook :count 0)
    ("pure" "" vhdl-template-default-hook :count 0)
    ("range" "" vhdl-template-default-hook :count 0)
    ("record" "" vhdl-template-default-hook :count 0)
    ("register" "" vhdl-template-default-hook :count 0)
    ("reject" "" vhdl-template-default-hook :count 0)
    ("rem" "" vhdl-template-default-hook :count 0)
    ("report" "" vhdl-template-report-hook :count 0)
    ("return" "" vhdl-template-return-hook :count 0)
    ("rol" "" vhdl-template-default-hook :count 0)
    ("ror" "" vhdl-template-default-hook :count 0)
    ("select" "" vhdl-template-selected-signal-asst-hook :count 0)
    ("severity" "" vhdl-template-default-hook :count 0)
    ("shared" "" vhdl-template-default-hook :count 0)
    ("sig" "" vhdl-template-signal-hook :count 0)
    ("signal" "" vhdl-template-signal-hook :count 0)
    ("sla" "" vhdl-template-default-hook :count 0)
    ("sll" "" vhdl-template-default-hook :count 0)
    ("sra" "" vhdl-template-default-hook :count 0)
    ("srl" "" vhdl-template-default-hook :count 0)
    ("subtype" "" vhdl-template-subtype-hook :count 0)
    ("then" "" vhdl-template-default-hook :count 0)
    ("to" "" vhdl-template-default-hook :count 0)
    ("transport" "" vhdl-template-default-hook :count 0)
    ("type" "" vhdl-template-type-hook :count 0)
    ("unaffected" "" vhdl-template-default-hook :count 0)
    ("units" "" vhdl-template-default-hook :count 0)
    ("until" "" vhdl-template-default-hook :count 0)
    ("use" "" vhdl-template-use-hook :count 0)
    ("var" "" vhdl-template-variable-hook :count 0)
    ("variable" "" vhdl-template-variable-hook :count 0)
    ("wait" "" vhdl-template-wait-hook :count 0)
    ("when" "" vhdl-template-when-hook :count 0)
    ("while" "" vhdl-template-while-loop-hook :count 0)
    ("with" "" vhdl-template-with-hook :count 0)
    ("xnor" "" vhdl-template-default-hook :count 0)
    ("xor" "" vhdl-template-default-hook :count 0)
   ))

