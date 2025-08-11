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
  (let* ((current-pi (mhs/current-pi))
         (pi-heading (concat "PI " current-pi))
         (target-file (seq-find (lambda (file)
                                  (string-match-p "data-services\\.org\\'" file))
                                (org-agenda-files))))

    (unless current-pi
      (user-error "No current PI set. Use `mhs/set-pi' to set one"))
    (unless target-file
      (user-error "Could not find data-services.org in `org-agenda-files'"))

    (with-current-buffer (find-file-noselect target-file)
      (org-with-wide-buffer
       (let* ((meetings (mhs/get-meetings-for-pi pi-heading))
              (choices (if meetings
                          (append meetings '("── Create new meeting... ──"))
                        '("── Create new meeting... ──")))
              (choice (completing-read
                       (format "Clock into meeting (%s): " pi-heading)
                       choices nil t)))

         (if (string-prefix-p "──" choice)
             (mhs/create-and-clock-new-meeting pi-heading)
           (mhs/clock-into-existing-meeting pi-heading choice)))))))

(defun mhs/get-meetings-for-pi (pi-name)
  "Get all meeting names under the specified PI using org-element API."
  (org-with-wide-buffer
   (when-let ((pi-pos (org-find-exact-headline-in-buffer pi-name)))
     (goto-char pi-pos)
     (when-let ((meetings-pos (save-excursion
                                (and (org-goto-first-child)
                                     (catch 'found
                                       (while t
                                         (when (string= (org-get-heading t t t t) "Meetings")
                                           (throw 'found (point)))
                                         (unless (org-goto-sibling)
                                           (throw 'found nil))))))))
       (goto-char meetings-pos)
       (let (meetings)
         (when (org-goto-first-child)
           (push (org-get-heading t t t t) meetings)
           (while (org-goto-sibling)
             (push (org-get-heading t t t t) meetings)))
         (nreverse meetings))))))

(defun mhs/create-and-clock-new-meeting (pi-name)
  "Create a new meeting under PI and clock into it."
  (let ((meeting-name (read-string "New meeting name: ")))
    (when (string-empty-p meeting-name)
      (user-error "Meeting name cannot be empty"))

    (org-with-wide-buffer
     (if-let ((meetings-pos (mhs/find-or-create-meetings-section pi-name)))
         (progn
           (goto-char meetings-pos)
           (org-end-of-subtree t t)
           (unless (bolp) (insert "\n"))
           (insert (format "*** %s\n" meeting-name))
           (forward-line -1)
           (org-clock-in)
           (message "Created and clocked into: %s → Meetings → %s"
                    pi-name meeting-name))
       (user-error "Could not find or create Meetings section under %s" pi-name)))))

(defun mhs/clock-into-existing-meeting (pi-name meeting-name)
  "Clock into an existing meeting under the specified PI."
  (org-with-wide-buffer
   (if-let ((meeting-pos (mhs/find-meeting-heading pi-name meeting-name)))
       (progn
         (goto-char meeting-pos)
         (org-clock-in)
         (message "Clocked into: %s → Meetings → %s" pi-name meeting-name))
     (user-error "Could not find meeting: %s under %s" meeting-name pi-name))))

(defun mhs/find-or-create-meetings-section (pi-name)
  "Find or create the Meetings section under PI-NAME, return its position."
  (when-let ((pi-pos (org-find-exact-headline-in-buffer pi-name)))
    (goto-char pi-pos)
    (or (save-excursion
          (and (org-goto-first-child)
               (catch 'found
                 (while t
                   (when (string= (org-get-heading t t t t) "Meetings")
                     (throw 'found (point)))
                   (unless (org-goto-sibling)
                     (throw 'found nil))))))
        ;; Create Meetings section if it doesn't exist
        (progn
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (insert "** Meetings\n")
          (forward-line -1)
          (point)))))

(defun mhs/find-meeting-heading (pi-name meeting-name)
  "Find the position of MEETING-NAME under PI-NAME's Meetings section."
  (when-let ((pi-pos (org-find-exact-headline-in-buffer pi-name)))
    (goto-char pi-pos)
    (when-let ((meetings-pos (save-excursion
                               (and (org-goto-first-child)
                                    (catch 'found
                                      (while t
                                        (when (string= (org-get-heading t t t t) "Meetings")
                                          (throw 'found (point)))
                                        (unless (org-goto-sibling)
                                          (throw 'found nil))))))))
      (goto-char meetings-pos)
      (and (org-goto-first-child)
           (catch 'found
             (while t
               (when (string= (org-get-heading t t t t) meeting-name)
                 (throw 'found (point)))
               (unless (org-goto-sibling)
                 (throw 'found nil))))))))

(provide 'mhs-scrum)
