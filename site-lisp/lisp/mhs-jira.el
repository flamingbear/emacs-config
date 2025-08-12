;;; mhs-jira.el --- interact with JIRA server via emacs

;; Copyright (C) 2014  Matt Savoie

;; Author: Matt Savoie <savoie@nsidc-snowblower.ad.nsidc.org>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; just sets a couple of values

;;; Code:

(defvar mhs-jira--current-ticket-number "DAS-xx"
  "The current ticket number that is clocked in.")

(defvar mhs-jira--url-base "https://nsidc.org/jira/browse/")

(defun mhs-jira--set-ticket-number ()
  "Set the ticket number to the current region, or prompt the user for input."
  (interactive)
  (setq mhs-jira--current-ticket-number
        (upcase
         (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-from-minibuffer "Enter ticket number: " mhs-jira--current-ticket-number))))
  (mhs-jira--update-url-base  mhs-jira--current-ticket-number))


(defun mhs-jira--update-url-base (ticket-number)
  "Handle multiple JIRA ticket locations based on ticket-number prefix."
  (cond ((string-prefix-p "NDCUM" ticket-number)
	 (setq mhs-jira--url-base "https://bugs.earthdata.nasa.gov/browse/"))
	((string-prefix-p "CUMULUS" ticket-number)
	 (setq mhs-jira--url-base "https://bugs.earthdata.nasa.gov/browse/"))
	((string-prefix-p "DAS" ticket-number)
	 (setq mhs-jira--url-base "https://bugs.earthdata.nasa.gov/browse/"))
	(t (setq mhs-jira--url-base "https://nsidc.org/jira/browse/"))))


(defun mhs-jira--ticket-uri ()
  "Return the URI to the current ticket."
  (concat mhs-jira--url-base mhs-jira--current-ticket-number))

(defun mhs-jira--insert-org-ticket-link ()
  "Insert an orgmode style link to the current ticket.

   [[https://nsidc.org/jira/browse/PM-xx][PM-xx]]"
  (interactive)
  (insert (concat "[[" (mhs-jira--ticket-uri) "][" mhs-jira--current-ticket-number "]]")))

(defun mhs-jira--insert-ticket-number ()
  "Insert the current ticket abbreviation."
  (interactive)
  (insert mhs-jira--current-ticket-number))

(defun mhs-jira--browse-current-ticket ()
  "Open a browser to the current ticket."
  (interactive)
  (browse-url (mhs-jira--ticket-uri))
  (message "Opening browser to ticket: %s" mhs-jira--current-ticket-number))


(defun mhs-jira--replace-ticket-with-link ()
     (interactive)
     (if (use-region-p)
	 (let* ((start (region-beginning))
		(end (region-end))
		(selected-text (buffer-substring-no-properties start end)))
	   (setq mhs-jira--current-ticket-number selected-text)
	   (mhs-jira--update-url-base mhs-jira--current-ticket-number)
	   (delete-region start end)
	   (mhs-jira--insert-org-ticket-link)
	   )))

(defun mhs-jira--replace-marked-das-link ()
  (interactive)
  ;; Ensure the region is active, meaning there's a selection to work with.
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             ;; Extract the string within the marked region.
             (selected-text (buffer-substring-no-properties start end))
             ;; Pattern to match DAS-#### or TRT-#### format.
             (pattern "^\\(DAS\\|TRT\\)-\\([0-9]+\\)$"))
        ;; Check if the selected text matches our pattern.
        (if (string-match pattern selected-text)
            (let ((prefix (match-string 1 selected-text))
                  (id (match-string 2 selected-text)))
              ;; Delete the original text.
              (delete-region start end)
              ;; Insert the replacement text.
              (insert (format "[[https://bugs.earthdata.nasa.gov/browse/%s-%s][%s-%s]]"
                              prefix id prefix id))
              (message "Replaced %s link with structured format." prefix))
          (message "Selected text does not match the DAS-#### or TRT-#### pattern.")))
    ;; If no region is selected, inform the user.
    (message "No region selected.")))

(defun mhs/das-ticket-to-markdown-link ()
  "Replace DAS ticket in region with markdown link format."
  (interactive)
  (when (use-region-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (ticket (buffer-substring-no-properties start end)))
      (when (string-match "^DAS-[0-9]+$" ticket)
        (delete-region start end)
        (insert (format "[%s](https://bugs.earthdata.nasa.gov/browse/%s)"
                        ticket ticket))))))

(defun mhs-jira--org-find-current-ticket ()
  "Look through the org files searching for a task matching the current ticket number."
  (interactive)
  ;; todo
  )

;; export org-mode subtrees to jira markdown.
;; Not terrible at all
(use-package ox-jira
  :ensure t
  )

;; Default keybindings
(when (require 'mhs-map)

  (defvar mhs-jira-map (make-keymap)
    "Make a keymap for jira commands.")

  (define-prefix-command 'mhs-jira-map)

  (define-key mhs-map "j" 'mhs-jira-map)

  (define-key mhs-jira-map "s" 'mhs-jira--set-ticket-number)
  (define-key mhs-jira-map "t" 'mhs-jira--insert-ticket-number)
  (define-key mhs-jira-map "o" 'mhs-jira--insert-org-ticket-link)
  (define-key mhs-jira-map "b" 'mhs-jira--browse-current-ticket))



(provide 'mhs-jira)
;;; mhs-jira.el ends here
