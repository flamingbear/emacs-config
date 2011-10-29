
(defvar eclim-path (expand-file-name (concat mhs-external-lisp-dir "emacs-eclim/")))
(when (file-accessible-directory-p eclim-path)
  (add-to-list 'load-path eclim-path )
  ;; only add the vendor path when you want to use the libraries provided with emacs-eclim
  (add-to-list 'load-path (concat eclim-path "vendor"))
  (when (try-require 'eclim)
    (setq eclim-auto-save t)
    (global-eclim-mode)))

(provide 'mhs-eclim)
