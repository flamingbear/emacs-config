(setq gnus-invalid-group-regexp "[:`'\"]\\|^$")

;; ---------------------------
;; What do I do with old mail?
(defvar mhs-imap-archive-group)
(setq mhs-imap-archive-group  "nnimap+kryos:Sent")
(setq gnus-message-archive-group mhs-imap-archive-group)

;; Where should expired articles end up?
;;---------------------------------------
(defvar mhs-expired-group "nnimap+kryos:Trash")
(setq nnmail-expiry-target mhs-expired-group)





(require 'nnir)

;; Old school : This is how you use imap as POP3 before migration to IMAP
;; (setq mail-sources '((file)
;; 		      (imap :user "savoie" 
;; 		           :server "kryos.colorado.edu"
;; 		           :stream ssl )
;;                      ))

(setq gnus-secondary-select-methods 
      '(
        (nnimap "kryos"
                (nnimap-inbox "INBOX")
                (nnimap-address "kryos.colorado.edu")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnir-search-engine imap)
                (nnimap-authinfo-file "~/.authinfo")
                )
        (nnimap "CU-exchange"
                (nnimap-address "exchangeimap.colorado.edu")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnir-search-engine imap)
                (nnimap-authinfo-file "~/.authinfo")
                )
	(nnml "private" )))



;; For information on swish-e and mail indexing look at .gnus files from before 2011-05-13

;; ;;GMAIL for flamingbear
;; Uncomment out for working IMAP
;; (add-to-list 'gnus-secondary-select-methods '(nnimap "gmail"
;;                                                      (nnimap-address "imap.gmail.com")
;;                                                      (nnimap-server-port 993)
;;                                                      (nnimap-authinfo-file "~/.imap-authinfo")
;;                                                      (nnimap-stream ssl)))


(setq nnimap-split-inbox '("INBOX"))
(setq nnimap-split-predicate "UNDELETED")
(setq nnimap-split-crosspost nil)

(setq nnimap-split-rule '(("kryos" ("INBOX" nnimap-split-fancy))))

;; -----------------------------------------------
;; Where should the incoming mail be delivered to?
(setq nnmail-split-methods 'nnmail-split-fancy)
(setq nnimap-split-methods 'nnimap-split-fancy)
;; Function for splitting kryos INBOX mail
(setq nnimap-split-fancy
      '(| 
	("X-Gnus-Mail-Source" "flamingbear" "flamingmail")
        ("subject" "Build failed in Hudson" "nsidc-notices")
        ("subject" "status reports reminder" "nsidc-notices")
        ("subject" "Searchlight status reminder" "nsidc-notices")
        (from "majordomo@kryos" "nsidc-notices")
        (any ".*Searchlight Maven Webapp.*" "nsidc-notices")
        (to "tiger@nsidc.org" "nsidc-notices")
        ("subject" ".*HDF Bulletin.*" "nsidc-notices")
        ("subject" ".*Hudson build became unstable.*" "nsidc-notices")
        ("subject" ".*way from my mail.*" "nsidc-notices")
        ("subject" ".*check for bad data.*" "nsidc-notices")
        ("subject" ".*Searchlight Maven Webapp.*" "nsidc-notices")
        ("subject" ".*NSIDC_NCAR_UCAR.*" "nsidc-notices")
        ("subject" ".*HDF Newsletter.*" "nsidc-notices")
        ("subject" ".*\\[community\\] Unidata.*" "nsidc-notices")
        (from "leave@cires.colorado" "nsidc-notices")
        (from ".*announcements@iedro.org" "nsidc-notices")
        (from "No Fluff Just Stuff" "nsidc-notices")
        (from "Earth System Research Laboratory" "nsidc-notices")
        (from "MicroRad" "nsidc-notices")
        (from "Linda Miller" "nsidc-notices")
        (from ".*iiisconferences.org" "nsidc-notices")
        ("subject" "osgis:.*" "nsidc-notices")
        ("subject" "Spam Quarantine Report" "nsidc-notices")
        ("subject" ".*Program Coordination Board.*" "nsidc-notices")
        (to "lost@nsidc" "nsidc-notices")
        ("subject" ".*Esip-federatedsearch.*" "esip-mail")
        (to "libre@nsidc.org" "lists")
        ("subject" "endevor_team" "endevor_list")
        (to ".*opensearch@googlegroups.com.*" "opensearch-mail")
        (to ".*clojure@googlegroups.com.*" "clojure-mail")
        (any ".*esip-federatedsearch.*" "esip-mail")
        ("subject" "jdee-users" "lists")
        ("subject" "Minutes and Questions" "nsidc-notices")
        ("subject" "Server Status" "nsidc-notices")
        ("subject" "General News & Discussion" "nsidc-notices")
        (from "nofluffjuststuff" "nsidc-notices")
        (from "ucereport@mxlogic.com" "nsidc-notices")
        (from "CIRES Message Center" "nsidc-notices")
        (from "URGENT Campus" "nsidc-notices")
        (from "Science Programmers Trac Administrator" "nsidc-notices")
        (from "PM Trac Administrator" "nsidc-notices")
        (from "CEDDAPE Trac Adminis.*" "nsidc-notices")
        (from "Searchlight Project Administrator" "nsidc-notices")
        (from "sjskhalsa@gmail.com" "mail-2011")
        (from "NEi Software" "lists")
	("from" "David Korn" "mail-2011")
	("from" "Peter Gibbons" "mail-2011")
        ("from" "iti@nsidc.org" "nsidc-notices")
	("from" "nscf user" "glas_requests")
        ("subject" "CF-metadata" "cf-metadata")
        ("subject" "cf-satellite" "cf-metadata")
        ("subject" "Mail from run_ds_pc.tcl" "glas_requests")
        ("subject" "NRTSI-G Script" "nsidc-notices")
        ("subject" "NRTSI PROBLEM.*" "nsidc-notices")
        ("subject" ".*CVS SNAPSHOT.*" "nsidc-notices")
        ("subject" "Data Operations] page changed" "nsidc-notices")
        ("subject" ".*timeseries thumbnail.*" "nsidc-notices")
        ("subject" ".*hires extent image.*" "nsidc-notices")
        ("subject" "Cron <savoie.*" "nsidc-notices")
        ("subject" "Cron <nrtsig.*" "nsidc-notices")
        ("subject" ".*Found bad NRTSIG data.*" "nsidc-notices")
        ("subject" "web:.*" "nsidc-notices")
        ("subject" "wrf-users.*" "wrf-mailing-lists")
        (to "wrf-.*" "wrf-mailing-lists")
        ("subject" ".*ncl-talk.*" "ncl-talk-lists")
        ("subject" ".*: GLAS Subsetting Request" "glas_requests")
        ("subject" "Output from your job" "nsidc-notices")
	("subject" "index_mail.pl" "nsidc-notices")
	("subject" "GLAS user Request submitted to" "glas_requests")
	("subject" "Mail from populate_db.tcl" "glas_requests")
	("subject" "Mail from data_select_req.tcl" "glas_requests")
	("subject" "newsboard" "nsidc-notices")
	("subject" "Backup Report" "nsidc-notices")
	("subject" "cs" "nsidc-notices")
	("subject" "cmc.*" "nsidc-notices")
	("subject" "^notices: " "nsidc-notices")
	(from "notices@nsidc.org" "nsidc-notices")
	(from "cs@nsidc.org" "nsidc-notices")
	(from "glas-support" "nsidc-notices")
	(from "ipy@nsidc.org" "nsidc-notices")
	("subject" "NWR" "nsidc-notices")
	("subject" "phget Warning" "nsidc-notices")
	("subject" "EOS Data Gateway product request" "nsidc-notices")
	("subject" "received at GES DAAC" "nsidc-notices")
	(from "GLAS SCF" "nsidc-notices")
	(from "Faculty and Research" "nsidc-notices")
	("subject" "ECS Notification" "nsidc-notices")
	("subject" "pl Warning" "nsidc-notices")
	(from "World Wide Web" "nsidc-notices")
	(from "geospatial" "lists")
	(from "rscf" "nsidc-notices")
	("subject" "glas-scfweb" "lists")
	(from "webadmin@rose.gsfc.nasa.gov" "lists")
	(from "hdfnews" "lists")
	("subject" "Western U.S. Passive" "lists")
	("subject" "Tibetan Plateau" "lists")
	("subject" "NISE Finished" "lists")
        (from "Buff Bulletin" "nsidc-notices")
	("subject" "Buff Bulletin" "nsidc-notices")
	("subject" "NSIDC_Util.pm Error" "nsidc-notices")
	("subject" "[Rr]eboot.*" "nsidc-notices")
        ("subject" "EP-News" "spam")
	(from "CU-Boulder Administrative.*" "nsidc-notices")
	(from "Administrative E-Mem.*" "nsidc-notices")
	(to "sdptoolkit" "lists")
	(to "ecs-mo" "lists")
        (to "savoie@tundra.colorado.edu" "spam")
        ("subject" "^No new Files on sidads" "nsidc-notices")
        "mail-2011"))

(defadvice message-send-mail (around gmail-message-send-mail protect activate)
  "Set up SMTP settings to use Gmail's server when mail is from a gmail.com address."
  (interactive "P")
  (if (save-restriction
        (message-narrow-to-headers)
        (string-match "flamingbear.com" (message-fetch-field "from")))

      (let ((message-send-mail-function 'smtpmail-send-it)
            ;; gmail says use port 465 or 587, but 25 works and those don't, go figure
            (smtpmail-starttls-credentials '(("smtp.gmail.com" 25 nil nil)))
            (smtpmail-auth-credentials '(("smtp.gmail.com" 25 "mattie@flamingbear.com" nil)))
            (smtpmail-default-smtp-server "smtp.gmail.com")
            (smtpmail-smtp-server "smtp.gmail.com")
            (smtpmail-smtp-service 25)
            (smtpmail-local-domain "flamingbear.com"))
        ad-do-it)
    ad-do-it))

;; (defadvice message-send-mail (around culink-message-send-mail protect activate)
;;   "Set up SMTP settings to use culink's server when mail is from a colorado.edu address."
;;   (interactive "P")
;;   (if (save-restriction
;;         (message-narrow-to-headers)
;;         (string-match "colorado.edu" (message-fetch-field "from")))

;;       (let ((message-send-mail-function 'smtpmail-send-it)
;;             (smtpmail-starttls-credentials '(("smtp.colorado.edu" 587 nil nil)))
;;             (smtpmail-auth-credentials '(("smtp.colorado.edu" 587 "matthew.savoie@colorado.edu" nil)))
;;             (smtpmail-default-smtp-server "smtp.colorado.edu")
;;             (smtpmail-smtp-server "smtp.colorado.edu")
;;             (smtpmail-smtp-service 587)
;;             (smtpmail-local-domain "colorado.edu"))
;;         ad-do-it)
;;     ad-do-it))


(setq gnus-posting-styles
      '((".*"
         (From "savoie@nsidc.org"))
        ;; ("^nnimap.*"
        ;;  (From "mattie@flamingbear.com")
        ;;  (signature "This is not spam(tm)."))
        ))

;;          (From (save-excursion
;;                  (set-buffer gnus-article-buffer)
;;                  (message-fetch-field "to")))



;; Old news reader
;; text.giganews.com

(setq gnus-extra-headers '(To Newsgroups))
(setq nnmail-extra-headers gnus-extra-headers)
(setq gnus-summary-line-format
      "%U%R%z%I%(%[%4L: %-20,20f%]%) %s\n")
(setq gnus-ignored-from-addresses "savoie@snowblower\\.colorado\\.edu\\|mattie@flamingbear\\.com\\|sugarlist@flamingbear\\.com\\|savoie@nsidc\\.org")


(setq gnus-thread-hide-subtree 't)

(add-hook 'gnus-select-article-hook 'gnus-summary-show-thread)


;; -----------------------------------------------
;; Where should the incoming mail be delivered to?

;(setq nnmail-split-methods 'nnmail-split-fancy)


(setq nnmail-expiry-wait-function
      (lambda (group)
	(cond ((string= group "nsidc-notices") 90)
	      ((string= group "glas_requests") 10)
	      ((string= group "spam") 7)
	      ((string= group "clojure-mail") 7)
              ((string-match "mail-" group) 30)
	      ((string= group "lists") 200)
	      ((string= group "cf-metadata") 10)
	      ((string= group "saved") 'never)
	      (t 60))))


(setq gnus-auto-expirable-newsgroups "\\(.*lists\\|mail.misc\\|.*spam\\|.*metadata\\|.*nsidc-notices.*\\|.*mylists\\|glas\\|clojure\\|.*exchange:INBOX\\)")


