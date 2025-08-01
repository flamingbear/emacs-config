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

(defun mhs/clock-into-scrum ()
  "Clock into the Meetings/Scrum task for the current PI in data-services.org."
  (interactive)
  (let ((current-pi (mhs/current-pi))
        (target-file (cl-find-if
                      (lambda (file)
                        (string-match "data-services\\.org$" file))
                      (org-agenda-files))))
    (unless current-pi
      (error "No current PI set. Use mhs/set-current-pi to set one"))
    (unless target-file
      (error "Could not find data-services.org in org-agenda-files"))
    (with-current-buffer (find-file-noselect target-file)
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward (format "^\\* PI %s$" (regexp-quote current-pi)) nil t)
          (error "Could not find %s heading" current-pi))
        (unless (re-search-forward "^\\*\\* Meetings$" nil t)
          (error "Could not find Meetings heading under %s" current-pi))
        (unless (re-search-forward "^\\*\\*\\* Scrum$" nil t)
          (error "Could not find Scrum heading under %s/Meetings" current-pi))
        (org-clock-in)
        (message "Clocked into %s/Meetings/Scrum" current-pi)))))

(defun mhs/clock-into-meeting ()
  "Show all meeting options under current PI and clock into selected one."
  (interactive)
  (let ((current-pi (concat "PI " (mhs/current-pi)))
        (target-file (cl-find-if
                      (lambda (file)
                        (string-match "data-services\\.org$" file))
                      (org-agenda-files)))
        meetings)
    (unless (mhs/current-pi)
      (error "No current PI set. Use mhs/set-current-pi to set one"))
    (unless target-file
      (error "Could not find data-services.org in org-agenda-files"))

    (with-current-buffer (find-file-noselect target-file)
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward (format "^\\* %s$" (regexp-quote current-pi)) nil t)
          (error "Could not find %s heading" current-pi))
        (unless (re-search-forward "^\\*\\* Meetings$" nil t)
          (error "Could not find Meetings heading under %s" current-pi))

        ;; Collect all meeting subheadings
        (let ((meetings-start (point))
              (meetings-end (save-excursion
                              (or (re-search-forward "^\\*\\* " nil t)
                                  (point-max)))))
          (goto-char meetings-start)
          (while (re-search-forward "^\\*\\*\\* \\(.+\\)$" meetings-end t)
            (push (match-string 1) meetings))))

      (if meetings
          (let* ((meeting-choice (completing-read "Select meeting to clock into: "
                                                  (reverse meetings) nil t))
                 (meeting-heading (format "^\\*\\*\\* %s$" (regexp-quote meeting-choice))))
            (save-excursion
              (goto-char (point-min))
              (re-search-forward (format "^\\* %s$" (regexp-quote current-pi)))
              (re-search-forward "^\\*\\* Meetings$")
              (if (re-search-forward meeting-heading nil t)
                  (progn
                    (org-clock-in)
                    (message "Clocked into %s/Meetings/%s" current-pi meeting-choice))
                (error "Could not find meeting: %s" meeting-choice))))
        (error "No meetings found under %s/Meetings" current-pi)))))


(defun mhs/scrum ()
  "Insert a daily report template in a new temporary buffer."
  (interactive)
  (interactive)
  (with-current-buffer (generate-new-buffer "Daily Report")
    (insert "Yesterday:\nToday:\nBlockers:")
    (forward-line -2)
    (end-of-line)
    (pop-to-buffer (current-buffer))))

(provide 'mhs-scrum)
