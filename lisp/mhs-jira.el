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

(defvar mhs-jira--current-ticket-number "RO-xx"
  "The current ticket number that is clocked in.")

(defvar mhs-jira--url-base "https://nsidc.org/jira/browse/")

(defun mhs-jira--set-ticket-number ()
  "Set the ticket number to the current region, or prompt the user for input."
  (interactive)
  (setq mhs-jira--current-ticket-number
        (upcase
         (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-from-minibuffer "Enter ticket number: " "RO-")))))


(defun mhs-jira--ticket-uri ()
  "Return the URI to the current ticket."
  (concat mhs-jira--url-base mhs-jira--current-ticket-number))

(defun mhs-jira--insert-org-ticket-link ()
  "Insert an orgmode style link to the current ticket.

   [[https://nsidc.org/jira/browse/RO-xx][RO-xx]]"
  (interactive)
  (insert (concat "[[" (mhs-jira--ticket-uri) "][" mhs-jira--current-ticket-number "]]")))

(defun mhs-jira--insert-ticket-number ()
  "Insert the current ticket abbreviation."
  (interactive)
  (insert mhs-jira--current-ticket-number))

(defun mhs-jira--browse-current-ticket ()
  "Open a browse to the current ticket."
  (interactive)
  (browse-url (mhs-jira--ticket-uri)))

(defun mhs-jira--org-find-current-ticket ()
  "Look through the org files searching for a task matching the current ticket number."
  (interactive)
  ;; todo
  )


;; ;; Default keybindings
(when (try-require 'mhs-map)

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
