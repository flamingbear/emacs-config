
;;;
;;;  This will define a keymap for my functions as I see fit.  So when I get
;;;  enough functions to fill out all of the C-c ? prefixes I can do my own
;;;  thing.
;;;

(defvar mhs-map (make-keymap)
"Make A keymap for Matt's personal functions")


(define-prefix-command 'mhs-map)


;;;
;;; mhs-map key definitions
;;;

(define-key mhs-map [(f8)]                'consult-register-store)
(define-key mhs-map [(f7)]                'consult-register-load)
(define-key mhs-map [(control c)]         'clipboard-copy-region)
(define-key mhs-map ";"                   'mhs-idlwave-insert-comment)
(define-key mhs-map "B"                   'mhs-browse-buffer-in-firefox)
(define-key mhs-map "D"                   'mhs-bracket-comment)
(define-key mhs-map "J"                   'mhs-remove-jshint-lines)
(define-key mhs-map "P"                   'perltidy-region)
(define-key mhs-map "U"                   'camelize-previous-snake)

(define-key mhs-map "b"                   'browse-url)

(define-key mhs-map "d"                   'mhs-insert-date)
(define-key mhs-map "e"                   'mhs-ediff)
(define-key mhs-map "f"                   'mhs/filename-and-dir)
(define-key mhs-map "i"                   'idlwave-shell)


(define-key mhs-map "k"                         'mhs-setdefaultvalue)
(define-key mhs-map "l"                                  'mhs-lineup)
(define-key mhs-map "m"                             'gnus-group-mail)
(define-key mhs-map "o"                              'org-clock-goto)
(define-key mhs-map "p"            'mhs-idlwave-insert-do_ps-keyword)
(define-key mhs-map "q"                             'gnus-group-exit)
(define-key mhs-map "t"                             'mhs-insert-todo)
(define-key mhs-map "u"                        'snake-previous-camel)
(define-key mhs-map "w"                  'delete-trailing-whitespace)
(define-key mhs-map "y"                              'clipboard-yank)


(provide 'mhs-map)
