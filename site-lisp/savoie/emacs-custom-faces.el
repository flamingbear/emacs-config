;; Add custom themes
(setq custom-theme-directory (locate-user-emacs-file "themes"))
(add-to-list 'custom-theme-load-path custom-theme-directory)

;; Set project and environment from out new VM file.
(defvar my-project)
(defvar my-environment)
(defun parse-fqdn (fn)
  (with-temp-buffer
    (insert-file-contents fn)
    (string-match "^\\(.*?\\)\\.\\(.*?\\)\\." (buffer-string))
    (list (match-string 1 (buffer-string)) (match-string 2 (buffer-string)))))

(defvar fqdn-filename "/etc/fqdn")
(when (file-exists-p fqdn-filename)
  (let ((results (parse-fqdn fqdn-filename)))
    (setq my-environment (first results))
    (setq my-project (second results))))



;; We like to know what machine we're running on.  So change the color
;; depending on where you're running from. Also set defaults like what idl
;; command to run for specific versions.
(use-package rainbow-mode :ensure t)
(setq my-menu-fg-color "#ffffff")
(setq my-menu-bg-color "#a52a2a")



(when (display-graphic-p)
  ;; If we're not running NX set the fonts like I like them.  Otherwise, we
  ;; default to 10x20
  (setq use-inconsolata t)
  (if  (or (string-match "snow.colorado.edu" (system-name))
           (not (string-match "snow[^s]" (system-name)))
           (string-match "masie" (user-login-name))
           (string-match "nise" (user-login-name))
           (string-match "cdr" (user-login-name)))
      (setq use-inconsolata nil))

  (cond (use-inconsolata (mhs-use-inconsolata))
        (running-macos (progn (mhs-use-inconsolata)
                              (set-face-attribute 'default nil :height 195)))
        (t (mhs-use-normal-face))))

;; This was the stuff below in my .gnu-emacs-custom but I've updated to use
;; different fonts for NX and whatnots.:


;; I'm starting to use BUILD as the default environment everywhere, so let's
;; get it here and set it to unknown if it's not set so that colors can be set
;; up according to your build.
(setq build (getenv "BUILD"))

(if (eq build nil)
    (setq build "unknown"))


;;-------------------------------------------------------------------
;; Customize the modeline and menu colors based on your environment.
;;-------------------------------------------------------------------
(cond ((string-match (user-login-name) "nrtsig")
       (progn
         (cond ((or (string-match build  "F17_prod") (string-match build  "production"))
                (setq my-menu-fg-color "#ff0000")
                (setq my-menu-bg-color "#483d8b")) ;darkslateblue
               ((or (string-match build  "F17_test") (string-match build  "testing"))
                (setq my-menu-fg-color "#ffe4b5")
                (setq my-menu-bg-color "#483d8b")) ; darkslateblue
               )))

      ;; if you read from a project file /etc/profile.d/file
      ((boundp 'my-project)
       (setq my-menu-fg-color "#005000")
       ;; Set background colors based on machine environment.
       (cond ((string-equal "blue" my-environment)
	      (setq my-menu-fg-color "#800000"))
	     ((string-equal "dev" my-environment)
	      (setq my-menu-fg-color "#006000"))
	     ((string-equal "integration" my-environment)
	      (setq my-menu-fg-color "#d7d700"))
	     ((string-equal "qa" my-environment)
	      (setq my-menu-fg-color "black"))
	     )
       ;; Set foreground color based on project name
       (cond
	((string-equal "shapefiles" my-project)
	 (setq my-menu-bg-color "yellow"))

	((or (string-equal "gsx" my-project)
	     (string-equal "greenland_today" my-project))
	 (setq my-menu-bg-color "#00aa00"))

	((string-equal "seaice" my-project)
	 (setq my-menu-bg-color "#d787ff"))

	((string-equal "seaiceservice" my-project)
	 (setq my-menu-bg-color "blue"))

	((string-equal "morph-vm" my-project)
	 (setq my-menu-bg-color "#d787d7"))

	((string-equal "snotelpages" my-project)
	 (setq my-menu-bg-color "#af5f00")
	 (setq idlwave-shell-explicit-file-name "idl83"))

	((string-equal "seaiceprojects" my-project)
	 (setq my-menu-bg-color "#afff00")
	 (setq idlwave-shell-explicit-file-name "idl83"))

	((string-equal "sea_ice_tools" my-project)
	 (setq my-menu-bg-color "green"))
	)
       )


      ;; NOAA Combined
      ((string-match "n0046_dev" build)
       (progn (setq my-menu-fg-color "#b8860b")   ;midnightblue
              (setq my-menu-bg-color "#8b4513")
              (setq idlwave-shell-explicit-file-name "idl82")
              )) ;cornflowerblue



      ;; Home Laptop: kitteh!!
      ((string-match ".*kitteh.*" (system-name))
       (progn (setq my-menu-fg-color "#ff40ff")
              (setq my-menu-bg-color "#932092")))


      ;; Dev or apps VM - standard
      ((string-match "^v.*\.\\(dev\\|apps\\)\.int\.nsidc\.org" (system-name))
       (progn (setq my-menu-fg-color "#F2FF30")
              (setq my-menu-bg-color "#5f5fff"))) ;

      ;; user archive.
      ((string-match (user-login-name) "archive")
       (progn (setq my-menu-fg-color "#000000")
              (setq my-menu-bg-color "#ff0000")))


      ;; You'll get the same colors for MASIE emacs sessions for any
      ;; machine... Set the title.
      ((string-match "MASIE_dev" build)
       (progn
         (setq idlwave-shell-explicit-file-name "my_idl.7.0.sh")
         (setq my-menu-bg-color "#7a378b") ; mediumOrchid4
         (setq my-menu-fg-color "#00ff7f")))

      ((string-match build "MASIE_test")
       ;;(setq idlwave-shell-explicit-file-name "my_idl.7.0.sh")
       (progn (setq idlwave-shell-explicit-file-name
                    (expand-file-name "~savoie/local/bin/my_idl.6.4.sh"))
              (setq my-menu-bg-color "#7a378b")
              (setq my-menu-fg-color "#ffd700")))

      ((string-match build "MASIE_prod")
       (progn ;;(setq idlwave-shell-explicit-file-name "my_idl.7.0.sh")
         (setq idlwave-shell-explicit-file-name "my_idl.6.4.sh")
         (setq my-menu-bg-color "#7a378b") ; mediumOrchid4
         (setq my-menu-fg-color "#cd6889")))

      ;; We're back to nusnow...
      ((string-match "nusnow.colorado" (system-name))
       (progn
         (setq my-menu-fg-color "#adff2f")
         (setq my-menu-bg-color "#556b2f")
         (when (string-match user-login-name "nise")
           (setq my-menu-fg-color "#cd6600"))))

      ;; Works with snow.colo only now...
      ((string-match "^snow.colorado.edu" (system-name))
       (progn
         (setq idlwave-shell-explicit-file-name "my_idl.8.2.sh")
         (setq my-menu-bg-color "#ffe4b5")
         (setq my-menu-fg-color "#000000")
         (cond ((string-match (user-login-name) "nise")
                (setq my-menu-fg-color "#1e90ff")))
         (cond ((string-match "development" build)
                (setq my-menu-fg-color "#8b008b")
                )
               ((string-match build  "testing")
                (setq my-menu-fg-color "#006400")
                )
               ((string-match build  "production")
                (setq my-menu-fg-color "#ff0000")
                )
               ((string-match "F17_dev" build)
                (setq idlwave-shell-explicit-file-name "my_idl.8.2.sh")
                (setq my-menu-fg-color "#c71585")
                (setq my-menu-bg-color "#deb887")
                )
               ((string-match build "dev")
                (setq idlwave-shell-explicit-file-name "my_idl.8.2.sh")
                (setq my-menu-fg-color "#698b69")
                (setq my-menu-bg-color "#b4cdcd")
                )
               ((string-match build "surgery_dev")
                (setq idlwave-shell-explicit-file-name "my_idl.8.2.sh")
                (setq my-menu-fg-color "#8b008b")
                (setq my-menu-bg-color "#b4cdcd")
                )
               ((string-match build  "F17_stdev_check")
                (setq idlwave-shell-explicit-file-name "my_idl.8.1.sh")
                (setq my-menu-fg-color "#b8860b")
                (setq my-menu-bg-color "#b4cdcd")
                )

               ((string-match build  "cdr_dev")
                (setq idlwave-shell-explicit-file-name "my_idl.8.1.sh")
                (setq my-menu-fg-color "#adff2f")
                (setq my-menu-bg-color "#4169e1")
                )
               ((string-match build  "cdr_test")
                (setq my-menu-fg-color "#ffd700")
                (setq my-menu-bg-color "#4169e1")
                )
               ((string-match build  "cdr_test")
                (setq my-menu-fg-color "#ff0000")
                (setq my-menu-bg-color "#4169e1")
                )
               ((string-match build  "F17_test")
                (setq my-menu-fg-color "#006400")
                (setq my-menu-bg-color "#deb887")
                )
               ((string-match build  "test_F15")
                (setq my-menu-fg-color "#8a2be2")
                (setq my-menu-bg-color "#deb887")
                )
               ((string-match build  "F15_bridge")
                (setq my-menu-fg-color "#4169e1")
                (setq my-menu-bg-color "#deb887")
                )
               ((string-match build  "F15_production")
                (setq my-menu-fg-color "#ff1493")
                (setq my-menu-bg-color "#deb887"))
               )))

      ((string-match "nsidc-snowblow" (system-name))
       (progn (setq my-menu-fg-color "#ffd700")
              (setq my-menu-bg-color "#9f79ee") ; mediumpurple2
              ;; (setq idlwave-shell-explicit-file-name "my_idl.6.4.sh")
              (setq idlwave-shell-explicit-file-name "my_idl.8.1.sh")))

      ((string-match (system-name) "sidads.colorado.edu")
       (progn (setq my-menu-fg-color "#8b3a3a")   ; indianred4
              (setq my-menu-bg-color "#ffdab9"))) ;peachpuff

      ((string-match (system-name) "arctic3.colorado.edu")
       (progn (setq my-menu-fg-color "#00688b")
              (setq my-menu-bg-color "#fa8072")
              (setq idlwave-shell-explicit-file-name "my_idl.6.3.sh")
              (setq idlwave-system-directory "/usr/local/rsi/idl/")
              ))

      ((string-match (system-name) "arctic5.colorado.edu")
       (progn (setq my-menu-fg-color "#ffff00")
              (setq my-menu-bg-color "#9370db")
              (setq idlwave-shell-explicit-file-name "my_idl.7.0.sh")
              (setq idlwave-system-directory "/usr/local/rsi/idl/")
              ))


      ((string-match (system-name) "arctic4.colorado.edu")
       (progn (setq my-menu-fg-color "#00bfff")   ; deep sky blue
              (setq my-menu-bg-color "#adff2f"))) ;green yellow


      (running-macos
       (progn
         (setq my-menu-fg-color "#baf257")
         (setq my-menu-bg-color "#738466")))

      ;; Default values
      (t (progn (setq my-menu-fg-color "#ffd700")
                (setq my-menu-bg-color "#6b8e23"))))



(defun mhs-update-mode-line ()
 "set the mode and menu bar colors according to customizations"
 (interactive)
 ;; Set both mode and menu rather than inheriting
 (set-face-foreground 'mode-line my-menu-fg-color)
 (set-face-background 'mode-line my-menu-bg-color)

 (set-face-foreground 'menu my-menu-fg-color)
 (set-face-background 'menu my-menu-bg-color))


;; We can choose different themes if you don't have full colors (like in terminal)
;; Also like moe-light moe-dark
;; We can choose different themes if you don't have full colors (like in terminal)
;; (require 'moe-theme)
;; (require 'solarized)
(if (<= (display-color-cells) 256)
    (load-theme 'ample-savoie t)
  (load-theme 'ample-savoie t))

(mhs-update-mode-line)
(provide 'emacs-custom-faces)
;;; .EMACS-CUSTOM-FACES ends here
