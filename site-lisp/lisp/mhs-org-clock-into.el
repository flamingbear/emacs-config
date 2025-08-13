(defun mhs/clock-into-section (section-name)
  "Show all items under current PI's SECTION-NAME and clock into selected one.
Also offers option to create a new item."
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
       (let* ((items (mhs/get-items-for-pi-section pi-heading section-name))
              (choices (if items
                          (append items (list (format "── Create new %s... ──"
                                                     (downcase (substring section-name 0 -1)))))
                        (list (format "── Create new %s... ──"
                                     (downcase (substring section-name 0 -1))))))
              (choice (completing-read
                       (format "Clock into %s (%s): " (downcase section-name) pi-heading)
                       choices nil t)))

         (if (string-prefix-p "──" choice)
             (mhs/create-and-clock-new-item pi-heading section-name)
           (mhs/clock-into-existing-item pi-heading section-name choice)))))))

(defun mhs/clock-into-task ()
  "Show all task options under current PI and clock into selected one."
  (interactive)
  (mhs/clock-into-section "Tasks"))

(defun mhs/clock-into-ticket ()
  "Show all ticket options under current PI and clock into selected one."
  (interactive)
  (mhs/clock-into-section "Tickets"))

(defun mhs/clock-into-meeting ()
  "Show all ticket options under current PI and clock into selected one."
  (interactive)
  (mhs/clock-into-section "Meetings"))

(defun mhs/get-items-for-pi-section (pi-name section-name)
  "Get all item names under the specified PI's section using org-element API."
  (org-with-wide-buffer
   (when-let ((pi-pos (org-find-exact-headline-in-buffer pi-name)))
     (goto-char pi-pos)
     (when-let ((section-pos (save-excursion
                               (and (org-goto-first-child)
                                    (catch 'found
                                      (while t
                                        (when (string= (org-get-heading t t t t) section-name)
                                          (throw 'found (point)))
                                        (unless (org-goto-sibling)
                                          (throw 'found nil))))))))
       (goto-char section-pos)
       (let (items)
         (when (org-goto-first-child)
           (push (org-get-heading t t t t) items)
           (while (org-goto-sibling)
             (push (org-get-heading t t t t) items)))
         (nreverse items))))))

(defun mhs/create-and-clock-new-item (pi-name section-name)
  "Create a new item under PI's section and clock into it."
  (let ((item-name (read-string (format "New %s name: " (downcase (substring section-name 0 -1))))))
    (when (string-empty-p item-name)
      (user-error "Item name cannot be empty"))

    (org-with-wide-buffer
     (if-let ((section-pos (mhs/find-or-create-section pi-name section-name)))
         (progn
           (goto-char section-pos)
           (org-end-of-subtree t t)
           (unless (bolp) (insert "\n"))
           (insert (format "*** %s\n" item-name))
           (forward-line -1)
           (org-clock-in)
           (message "Created and clocked into: %s → %s → %s"
                    pi-name section-name item-name))
       (user-error "Could not find or create %s section under %s" section-name pi-name)))))

(defun mhs/clock-into-existing-item (pi-name section-name item-name)
  "Clock into an existing item under the specified PI's section."
  (org-with-wide-buffer
   (if-let ((item-pos (mhs/find-item-heading pi-name section-name item-name)))
       (progn
         (goto-char item-pos)
         (org-clock-in)
         (message "Clocked into: %s → %s → %s" pi-name section-name item-name))
     (user-error "Could not find %s: %s under %s" (downcase (substring section-name 0 -1)) item-name pi-name))))

(defun mhs/find-or-create-section (pi-name section-name)
  "Find or create the specified section under PI-NAME, return its position."
  (when-let ((pi-pos (org-find-exact-headline-in-buffer pi-name)))
    (goto-char pi-pos)
    (or (save-excursion
          (and (org-goto-first-child)
               (catch 'found
                 (while t
                   (when (string= (org-get-heading t t t t) section-name)
                     (throw 'found (point)))
                   (unless (org-goto-sibling)
                     (throw 'found nil))))))
        ;; Create section if it doesn't exist
        (progn
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (insert (format "** %s\n" section-name))
          (forward-line -1)
          (point)))))

(defun mhs/find-item-heading (pi-name section-name item-name)
  "Find the position of ITEM-NAME under PI-NAME's section."
  (when-let ((pi-pos (org-find-exact-headline-in-buffer pi-name)))
    (goto-char pi-pos)
    (when-let ((section-pos (save-excursion
                              (and (org-goto-first-child)
                                   (catch 'found
                                     (while t
                                       (when (string= (org-get-heading t t t t) section-name)
                                         (throw 'found (point)))
                                       (unless (org-goto-sibling)
                                         (throw 'found nil))))))))
      (goto-char section-pos)
      (and (org-goto-first-child)
           (catch 'found
             (while t
               (when (string= (org-get-heading t t t t) item-name)
                 (throw 'found (point)))
               (unless (org-goto-sibling)
                 (throw 'found nil))))))))

(provide 'mhs-org-clock-into)
