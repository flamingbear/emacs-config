;;; MHS-PIVOTAL.EL ---


 (defun file-string (file)
    "Read the contents of a file and return as a string."
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string)))
;; Set up api tolken for pivotal tracket
(set 'pivotal-api-token
     (car (split-string (file-string
                         (concat emacs-top "private/pivotal-api-key.txt")) "\n")))

(provide 'mhs-pivotal)
;;; MHS-PIVOTAL.EL ends here
