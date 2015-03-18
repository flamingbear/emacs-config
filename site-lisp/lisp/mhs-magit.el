;;; mhs-magit.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Matt Savoie

;; Author: Matt Savoie <savoie@savoie-laptop.ad.int.nsidc.org>
;; Keywords:

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

;;


;;; Code:
;; Stolen here

;;  TODO integrate bitbucket:
;; "git@bitbucket.org:nsidc/extended_generic_swath.git"

;; http://endlessparentheses.com/easily-create-github-prs-from-magit.html?source=rss
(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/compare/%s"
     (replace-regexp-in-string
      "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
      (magit-get "remote"
                 (magit-get-current-remote)
                 "url"))
     (magit-get-current-branch))))

(eval-after-load 'magit
  '(define-key magit-mode-map "V"
     #'endless/visit-pull-request-url))


(provide 'mhs-magit)
;;; mhs-magit.el ends here
