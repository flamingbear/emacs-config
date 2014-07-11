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

(try-require 'jira)
(setq jira-url "https://nsidc.org/jira/rpc/xmlrpc")

(provide 'mhs-jira)
;;; mhs-jira.el ends here
