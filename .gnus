
(setq gnus-invalid-group-regexp "[:`'\"]\\|^$")

;; ---------------------------
;; What do I do with old mail?
(defvar mhs-imap-archive-group)
(setq mhs-imap-archive-group  "nnimap+kryos:Sent")
(setq gnus-message-archive-group mhs-imap-archive-group)

;; Where should expired articles end up?
;;---------------------------------------
(defvar mhs-expired-group "Trash")
(setq nnmail-expiry-target mhs-expired-group)


;; Do we really want the auto-fill turned on. I don't think so
(setq message-fill-column 'nil)

;; (setq mml2015-signers '("1AFA6998")) # flamingbear
(setq mml2015-signers '("171530BF"))  ;; savoie@nsidc.org
;; search mail with various search engines.

(setq gnus-buttonized-mime-types (quote ("multipart/signed")))
(setq gnus-inhibit-mime-unbuttonizing nil)

(use-package nnir :defer 2)

(setq gnus-select-method
      '(nnimap "kryos"
	       (nnimap-inbox "INBOX")
               (nnimap-address "kryos.colorado.edu")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnir-search-engine imap)
               (nnimap-authinfo-file "~/.authinfo.gpg")
	       (nnimap-record-commands t)))

(setq gnus-secondary-select-methods
      '((nnimap "CU-exchange"
                (nnimap-address "outlook.office365.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnir-search-engine imap)
                (nnimap-authinfo-file "~/.authinfo.gpg"))
	(nnml "private" )))


(defvar default-mailbox (concat "maild/mail-" (format-time-string "%Y")))


(defun default-mailbox ()
  (interactive)
  (concat "maild/mail-" (format-time-string "%Y")))


;; We don't want to see each time we send email.
(setq gnus-gcc-mark-as-read 't)
;; -----------------------------------------------
;; Where should the incoming mail be delivered to?

(setq nnmail-split-methods 'nnmail-split-fancy)
(setq nnimap-split-methods 'nnimap-split-fancy)
;; Function for splitting kryos INBOX mail

(defvar my-split-methods
  '(|
    (any ".*savoie@kryos.colorado.edu.*" "spam")
    ("Subject" ".*POTENTIAL SPAM.*" "spam")
    ("Delivered-To" "savoie@kryos.colorado.edu" "spam")
    ("Subject" ".*\\[confluence\\].*" "maild/confluence")
    ("Subject" ".*\\[Bamboo\\].*" "maild/bitbucket")
    (any "Proofpoint Essentials" "mail/lists")
    ("Subject" ".*\\[cumulus-nasa/cumulus.*\\].*" "maild/bitbucket")
    ("Subject" ".*CUMULUS/nsidc-deploy.*" "maild/bitbucket")
    ("Subject" ".*\\[Cumulus-All\\].*" "maild/cumulus")
    ("To" ".*@noreply.github.com" "maild/bitbucket")
    ("Subject" ".*\\[GitHub\\].*" "maild/bitbucket")
    ("Subject" ".*Bitbucket.*" "maild/bitbucket")
    (from ".*Travis CI.*" "maild/travis")
    ("Subject" ".*\\[eosdis-ux\\].*" "maild/lists")
    ("Subject" ".*\\[ECE\\].* Earth Science Data System Working Groups.*" "mail/lists")
    ("Subject" ".*\\[ECE\\].*" "mail/lists")
    ("Subject" ".*\\[eosdis-.*\\].*" "maild/lists")
    ("Subject" ".*\\[gdal-dev\\].*" "maild/lists")
    ("Subject" ".*\\[Prod_changes\\].*" "maild/jira-production")
    ("Subject" ".*\\[Asina\\].*" "maild/asina")
    ("Subject" ".*\\[Greenlandtoday\\].*" "maild/greenlandtoday")
    ("Subject" ".*Esip-documentation.*" "maild/esip-documentation")
    ("Subject" ".*Esip-sciencesoftware.*" "maild/esip-documentation")
    ("Subject" ".*ESIP Update.*" "maild/esip-documentation")
    ("Subject" "NISE [Ff]ailure.*" "maild/nsidc-notices")
    ("Subject" ".*Software Release Announcements.*" "maild/jira-production")
    ("Subject" ".*JIRA.*" "maild/jira-tickets")
    (from ".*JIRA.*" "maild/jira-tickets")
    ("subject" ".*\\[Server Status.*" "maild/nsidc-notices")
    (from "www@nsidc.org" "maild/nsidc-notices")
    (from "Pivotal Tracker" "maild/nsidc-notices")
    (from "scm\@scm\.nsidc\.org" "maild/nsidc-notices")
    ("subject" "status reports reminder" "maild/nsidc-notices")
    (from "majordomo@kryos" "maild/nsidc-notices")
    (any ".*Searchlight Maven Webapp.*" "maild/nsidc-notices")
    (to "tiger@nsidc.org" "maild/nsidc-notices")
    ("subject" ".*HDF Bulletin.*" "maild/nsidc-notices")
    ("subject" ".*way from my mail.*" "maild/nsidc-notices")
    ("subject" ".*check for bad data.*" "maild/nsidc-notices")
    ("subject" ".*NSIDC_NCAR_UCAR.*" "maild/nsidc-notices")
    ("subject" ".*HDF Newsletter.*" "maild/nsidc-notices")
    ("subject" ".*\\[community\\] Unidata.*" "maild/nsidc-notices")
    (from "leave@cires.colorado" "maild/nsidc-notices")
    (from "ciresleave@Colorado.EDU" "maild/nsidc-notices")
    (from ".*announcements@iedro.org" "maild/nsidc-notices")
    (from "No Fluff Just Stuff" "maild/nsidc-notices")
    (from "Earth System Research Laboratory" "maild/nsidc-notices")
    (from "Linda Miller" "maild/nsidc-notices")
    (from ".*iiisconferences.org" "maild/nsidc-notices")
    ("subject" "osgis:.*" "maild/nsidc-notices")
    ("subject" "Spam Quarantine Report" "maild/nsidc-notices")
    ("subject" ".*Program Coordination Board.*" "maild/nsidc-notices")
    (to "lost@nsidc" "maild/nsidc-notices")
    ("subject" ".*Esip-federatedsearch.*" "maild/esip-mail")
    ("subject" "endevor_team" "maild/endevor_list")
    (to ".*opensearch@googlegroups.com.*" "maild/opensearch-mail")
    (any ".*esip-federatedsearch.*" "maild/esip-mail")
    ("subject" "Minutes and Questions" "maild/nsidc-notices")
    ("subject" "General News & Discussion" "maild/nsidc-notices")
    (from "nofluffjuststuff" "maild/nsidc-notices")
    (from "ucereport@mxlogic.com" "maild/nsidc-notices")
    (from "CIRES Message Center" "maild/nsidc-notices")
    (from "URGENT Campus" "maild/nsidc-notices")
    (from "Science Programmers Trac Administrator" "maild/nsidc-notices")
    (from "PM Trac Administrator" "maild/nsidc-notices")
    (from "CEDDAPE Trac Adminis.*" "maild/nsidc-notices")
    (from "Searchlight Project Administrator" "maild/nsidc-notices")
    ("from" "iti@nsidc.org" "maild/nsidc-notices")
    ("from" "nscf user" "maild/glas_requests")
    ("subject" ".*CF-metadata.*" "maild/cf-metadata")
    ("subject" ".*CF Metadata.*" "maild/cf-metadata")
    ("subject" "cf-satellite" "maild/cf-metadata")
    ("subject" "Mail from run_ds_pc.tcl" "maild/glas_requests")
    ("subject" "NRTSI-G Script" "maild/nsidc-notices")
    ("subject" "NRTSI PROBLEM.*" "maild/nsidc-notices")
    ("subject" "Data Operations] page changed" "maild/nsidc-notices")
    ("subject" ".*timeseries thumbnail.*" "maild/nsidc-notices")
    ("subject" ".*hires extent image.*" "maild/nsidc-notices")
    ("subject" "Cron <savoie.*" "maild/nsidc-notices")
    ("subject" "Cron <nrtsig.*" "maild/nsidc-notices")
    ("subject" ".*Found bad NRTSIG data.*" "maild/nsidc-notices")
    ("subject" "web:.*" "maild/nsidc-notices")
    ("subject" ".*: GLAS Subsetting Request" "maild/glas_requests")
    ("subject" "Output from your job" "maild/nsidc-notices")
    ("subject" "index_mail.pl" "maild/nsidc-notices")
    ("subject" "GLAS user Request submitted to" "maild/glas_requests")
    ("subject" "Mail from populate_db.tcl" "maild/glas_requests")
    ("subject" "Mail from data_select_req.tcl" "maild/glas_requests")
    ("subject" "newsboard" "maild/nsidc-notices")
    ("subject" "Backup Report" "maild/nsidc-notices")
    ("subject" "cs" "maild/nsidc-notices")
    ("subject" "cmc.*" "maild/nsidc-notices")
    ("subject" "^notices: " "maild/nsidc-notices")
    (from "notices@nsidc.org" "maild/nsidc-notices")
    (from "cs@nsidc.org" "maild/nsidc-notices")
    (from "glas-support" "maild/nsidc-notices")
    (from "ipy@nsidc.org" "maild/nsidc-notices")
    ("subject" "NWR" "maild/nsidc-notices")
    ("subject" "phget Warning" "maild/nsidc-notices")
    ("subject" "EOS Data Gateway product request" "maild/nsidc-notices")
    ("subject" "received at GES DAAC" "maild/nsidc-notices")
    (from "GLAS SCF" "maild/nsidc-notices")
    (from "Faculty and Research" "maild/nsidc-notices")
    ("subject" "ECS Notification" "maild/nsidc-notices")
    ("subject" "pl Warning" "maild/nsidc-notices")
    (from "World Wide Web" "maild/nsidc-notices")
    (from "rscf" "maild/nsidc-notices")
    ("subject" "glas-scfweb" "maild/lists")
    (from "webadmin@rose.gsfc.nasa.gov" "maild/lists")
    (from "hdfnews" "maild/lists")
    ("subject" "Western U.S. Passive" "maild/lists")
    ("subject" "Tibetan Plateau" "maild/lists")
    ("subject" "NISE Finished" "maild/lists")
    (from "Buff Bulletin" "maild/nsidc-notices")
    ("subject" "Buff Bulletin" "maild/nsidc-notices")
    ("subject" "NSIDC_Util.pm Error" "maild/nsidc-notices")
    ("subject" "[Rr]eboot.*" "maild/nsidc-notices")
    ("subject" "EP-News" "spam")
    (from "CU-Boulder Administrative.*" "maild/nsidc-notices")
    (from "Administrative E-Mem.*" "maild/nsidc-notices")
    (to "sdptoolkit" "maild/lists")
    (from "ecs-mo" "maild/lists")
    ("subject" "Daac-mgmt" "maild/nsidc-notices")
    (to "savoie@tundra.colorado.edu" "spam")
    ("subject" "^No new Files on sidads" "maild/nsidc-notices")
    (: default-mailbox)
    ))
(setq nnimap-split-fancy my-split-methods)
(setq nnmail-split-fancy my-split-methods)





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


(setq gnus-posting-styles
      '((".*"
         (From "savoie@nsidc.org"))
        ;; ("^nnimap.*"
        ;;  (From "mattie@flamingbear.com")
        ;;  (signature "This is not spam(tm)."))
        ))


(setq gnus-extra-headers '(To Newsgroups))
(setq nnmail-extra-headers gnus-extra-headers)
(setq gnus-summary-line-format
      "%U%R%z%I%(%[%4L: %-20,20f%]%) %s\n")
(setq gnus-ignored-from-addresses "mattie@flamingbear\\.com\\|savoie@nsidc\\.org")


(setq gnus-thread-hide-subtree 't)

(add-hook 'gnus-select-article-hook 'gnus-summary-show-thread)


;; Set automatic days to expire on some groups
(setq nnmail-expiry-wait-function
      (lambda (group)
        (cond ((string-match "nsidc-notices" group) 90)
              ((string-match "jira-production" group) 30)
              ((string-match "jira-tickets" group) 30)
              ((string-match "bitbucket" group) 30)
	      ((string-match "maild/travis" group) 30)
              ((string-match "confluence" group) 30)
              ((string-match "spam" group) 7)
              ((string-match "mail-" group) 30)
              ((string-match "lists" group) 100)
              ((string-match "cf-metadata" group) 10)
              ((string-match "saved" group) 'never)
              (t 100))))

;; Set the groups that are auto-expirable.
(setq gnus-auto-expirable-newsgroups "\\(.*lists\\|.*bitbucket.*\\|.*jira.*\\|.*list\\|.*nsidc-notices\\|.*confluence\\|.*exchange:INBOX\\)")
