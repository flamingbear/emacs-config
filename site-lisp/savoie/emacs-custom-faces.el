;; Add custom themes
(setq custom-theme-directory (locate-user-emacs-file "themes"))
(add-to-list 'custom-theme-load-path custom-theme-directory)

;; Set my-project and my-environment from out of the /etc/fqdn file on VMs
(defvar my-project)
(defvar my-environment)
(defun parse-fqdn (fn)
  "Parses /etc/fqdn file to get maching information."
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


;; I'm done using BUILD as an environment marker. but leave it for posterity.
(setq build (getenv "BUILD"))

(if (eq build nil)
    (setq build "unknown"))


;;-------------------------------------------------------------------
;; Customize the modeline and menu colors based on your environment.
;;-------------------------------------------------------------------
(cond
      ;; if you read from a project file /etc/fqdn
      ((boundp 'my-project)
       (setq my-menu-fg-color "#005000")
       ;; Set background colors based on machine environment.
       (cond ((string-equal "blue" my-environment)
	      (setq my-menu-fg-color "#A00000"))
	     ((string-equal "dev" my-environment)
	      (setq my-menu-fg-color "#006000"))
	     ((string-equal "integration" my-environment)
	      (setq my-menu-fg-color "#d7d700"))
	     ((string-equal "qa" my-environment)
	      (setq my-menu-fg-color "black"))
	     )
       ;; Set foreground color based on project name
       (cond
	((or (string-equal "shapefiles" my-project)
	     (string-equal "soac-data" my-project))
	 (setq my-menu-bg-color "yellow"))

	((or (string-equal "gsx" my-project)
	     (string-equal "greenland_today" my-project)
	     (string-equal "pm_metadata" my-project))
	 (setq my-menu-bg-color "#00aa00"))

	((string-equal "seaice" my-project)
	 (setq my-menu-bg-color "#d787ff"))

	((string-equal "seaiceservice" my-project)
	 (setq my-menu-bg-color "blue"))

	((string-equal "morph-vm" my-project)
	 (setq my-menu-bg-color "blue"))

	((string-equal "dapaggr" my-project)
	 (setq my-menu-bg-color "#5c5cff"))

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

      ;; Home Laptop: kitteh!!
      ((string-match ".*kitteh.*" (system-name))
       (progn (setq my-menu-fg-color "#ff40ff")
              (setq my-menu-bg-color "#932092")))

      ;; user archive.
      ((string-match (user-login-name) "archive")
       (progn (setq my-menu-fg-color "#000000")
              (setq my-menu-bg-color "#ff0000")))


      ;; Works with snow.colo only now...
      ((string-match "^snow.colorado.edu" (system-name))
       (progn
         (setq idlwave-shell-explicit-file-name "my_idl.8.2.sh")
         (setq my-menu-bg-color "#ffe4b5")
         (setq my-menu-fg-color "#000000")
         (cond ((string-match (user-login-name) "nise")
                (setq my-menu-fg-color "#1e90ff")))
         (cond ((string-match "development" build)
                (setq my-menu-fg-color "#8b008b"))
               ((string-match build  "testing")
                (setq my-menu-fg-color "#006400"))
               ((string-match build  "production")
                (setq my-menu-fg-color "#ff0000"))
               ((string-match "F17_dev" build)
                (setq idlwave-shell-explicit-file-name "my_idl.8.2.sh")
                (setq my-menu-fg-color "#c71585")
                (setq my-menu-bg-color "#deb887"))
               ((string-match build "dev")
                (setq idlwave-shell-explicit-file-name "my_idl.8.2.sh")
                (setq my-menu-fg-color "#698b69")
                (setq my-menu-bg-color "#b4cdcd"))
               ((string-match build "surgery_dev")
                (setq idlwave-shell-explicit-file-name "my_idl.8.2.sh")
                (setq my-menu-fg-color "#8b008b")
                (setq my-menu-bg-color "#b4cdcd"))
               ((string-match build  "F17_stdev_check")
                (setq idlwave-shell-explicit-file-name "my_idl.8.1.sh")
                (setq my-menu-fg-color "#b8860b")
                (setq my-menu-bg-color "#b4cdcd"))
               )))

      (running-macos
       (progn
         (setq my-menu-fg-color "#5d478b")
         (setq my-menu-bg-color "#ab82ff")))

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


(use-package ample-theme
  :ensure t)

(if (<= (display-color-cells) 256)
    (load-theme 'ample-flat-256 t)
  (load-theme 'ample-flat-plus t))

(mhs-update-mode-line)
(provide 'emacs-custom-faces)

;;; .EMACS-CUSTOM-FACES ends here
