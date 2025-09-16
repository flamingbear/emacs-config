;; SaFE planning info
(defun mhs/set-pi ()
  "Set the current PI value in a persistent file."
  (interactive)
  (let* ((current-pi (read-string "Enter Current PI [e.g. PI 24.2]: "))
         (tmp-pi-file (expand-file-name ".current-pi" user-emacs-directory)))
    (with-temp-file tmp-pi-file
      (insert current-pi))
    (message "Current PI value set to: %s" current-pi)))

(defun mhs/current-pi ()
  "Retrieve the previously set current PI value from the stored file."
  (interactive)
  (let ((file-path (expand-file-name ".current-pi" user-emacs-directory)))
    (when (file-exists-p file-path)
      (with-temp-buffer
        (insert-file-contents file-path)
        (buffer-string)))))

(defun mhs/set-sprint ()
  "Set the current PI value in a persistent file."
  (interactive)
  (let* ((current-sprint (read-string "Enter Current sprint [e.g. 1, 2, PI]: "))
         (tmp-sprint-file (expand-file-name ".current-sprint" user-emacs-directory)))
    (with-temp-file tmp-sprint-file
      (insert current-sprint))
    (message "Current sprint value set to: %s" current-sprint)))

(defun mhs/current-sprint-number ()
  "Retrieve the previously set current Sprint value from the stored file."
  (interactive)
  (let ((file-path (expand-file-name ".current-sprint" user-emacs-directory)))
    (when (file-exists-p file-path)
      (with-temp-buffer
        (insert-file-contents file-path)
        (buffer-string)))))

(defun mhs/current-sprint ()
  "Retrieve and display the current sprint and PI, concatenated."
  (interactive)
  (let ((pi (mhs/current-pi))
        (sprint (mhs/current-sprint-number)))
    (if (and pi sprint)
        (message "%s.%s" (string-trim pi) (string-trim sprint))
      (message "PI or Sprint is not set properly."))))

(defun mhs/scrum ()
  "Insert a daily report template in a new temporary buffer."
  (interactive)
  (with-current-buffer (generate-new-buffer "Daily Report")
    (insert "Yesterday:\nToday:\nBlockers:")
    (forward-line -2)
    (end-of-line)
    (pop-to-buffer (current-buffer))))



(provide 'mhs-scrum)
