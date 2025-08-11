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
  (interactive)
  (with-current-buffer (generate-new-buffer "Daily Report")
    (insert "Yesterday:\nToday:\nBlockers:")
    (forward-line -2)
    (end-of-line)
    (pop-to-buffer (current-buffer))))

(defun mhs/clock-into-meeting ()
  "Show all meeting options under current PI and clock into selected one.
Also offers option to create a new meeting."
  (interactive)
  (let ((current-pi (concat "PI " (mhs/current-pi)))
        (target-file (cl-find-if
                      (lambda (file)
                        (string-match "data-services\\.org$" file))
                      (org-agenda-files))))
    (unless (mhs/current-pi)
      (error "No current PI set. Use mhs/set-current-pi to set one"))
    (unless target-file
      (error "Could not find data-services.org in org-agenda-files"))

    (with-current-buffer (find-file-noselect target-file)
      (let* ((meetings (mhs/get-meetings-for-pi current-pi))
             (meeting-options (append meetings '("Create new meeting...")))
             (meeting-choice (completing-read "Select meeting to clock into: "
                                              meeting-options nil t)))

        (if (string= meeting-choice "Create new meeting...")
            (mhs/create-and-clock-new-meeting current-pi)
          (mhs/clock-into-existing-meeting current-pi meeting-choice))))))

(defun mhs/get-meetings-for-pi (pi-name)
  "Get all meeting names under the specified PI."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^\\* %s$" (regexp-quote pi-name)) nil t)
      (when (re-search-forward "^\\*\\* Meetings$" nil t)
        (let (meetings)
          (org-map-entries
           (lambda ()
             (when (= (org-current-level) 3)
               (push (org-get-heading t t t t) meetings)))
           nil 'tree)
          (reverse meetings))))))

(defun mhs/create-and-clock-new-meeting (pi-name)
  "Create a new meeting under PI and clock into it."
  (let ((new-meeting-name (read-string "Enter new meeting name: ")))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (format "^\\* %s$" (regexp-quote pi-name)))
      (re-search-forward "^\\*\\* Meetings$")

      ;; Find end of Meetings section
      (org-end-of-subtree t)
      (insert (format "\n*** %s\n" new-meeting-name))
      (forward-line -1)
      (org-clock-in)
      (message "Created and clocked into %s/Meetings/%s" pi-name new-meeting-name))))

(defun mhs/clock-into-existing-meeting (pi-name meeting-name)
  "Clock into an existing meeting under the specified PI."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward (format "^\\* %s$" (regexp-quote pi-name)) nil t)
      (error "Could not find %s heading" pi-name))
    (unless (re-search-forward "^\\*\\* Meetings$" nil t)
      (error "Could not find Meetings heading under %s" pi-name))
    (unless (re-search-forward (format "^\\*\\*\\* %s$" (regexp-quote meeting-name)) nil t)
      (error "Could not find meeting: %s" meeting-name))

    (org-clock-in)
    (message "Clocked into %s/Meetings/%s" pi-name meeting-name)))

(provide 'mhs-scrum)
