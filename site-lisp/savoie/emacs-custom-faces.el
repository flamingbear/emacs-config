;; Add custom themes
(setq custom-theme-directory (locate-user-emacs-file "themes"))
(add-to-list 'custom-theme-load-path custom-theme-directory)


(defvar my-project nil
  "Variable to be filled with the name of the project parsed from /etc/fqdn.")
(defvar my-environment nil
  "Variable to be filled with the name of the environment parsed from /etc/fqdn.")

(defun parse-fqdn (fn)
  "Parse /etc/fqdn file to get machine information.  FN is the filename, always /etc/fqdn."
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
(defvar my-modeline-fg-color "#ffffff"
  "My foreground color for the modeline should be customized below.")
(defvar my-modeline-bg-color "#a52a2a"
  "My background color for the modeline should be customized below.")



(when (display-graphic-p)
  ;; If we're not running NX set the fonts like I like them.  Otherwise, we
  ;; default to 10x20
  (cond (running-macos (progn (mhs-use-inconsolata)
                              (set-face-attribute 'default nil :height 195)))
        (t (mhs-use-normal-face))))


;; I'm done using BUILD as an environment marker. but leave it for posterity.
(defvar build (getenv "BUILD"))

(if (eq build nil)
    (setq build "unknown"))


;;-------------------------------------------------------------------
;; Customize the modeline and menu colors based on your environment.
;;
;; TODO [MHS, 11/24/2020] This is OBE since I swtiched to
;;sanityinc-tomorrow-night can revisit later.
;;-------------------------------------------------------------------
(cond
 (my-project
  (setq my-modeline-fg-color "#005000")
  ;; Set background colors based on machine environment.
  (cond ((string-equal "blue" my-environment)
	 (setq my-modeline-fg-color "#A00000"))
	((string-equal "dev" my-environment)
	 (setq my-modeline-fg-color "#006000"))
	((string-equal "integration" my-environment)
	 (setq my-modeline-fg-color "#d7d700"))
	((string-equal "qa" my-environment)
	 (setq my-modeline-fg-color "black"))
	)
  ;; Set background modeline color based on project name
  (cond
   ((or (string-equal "shapefiles" my-project)
	(string-equal "soac-data" my-project))
    (setq my-modeline-bg-color "yellow"))

   ((or (string-equal "gsx" my-project)
	(string-equal "greenland_today" my-project)
	(string-equal "pm_metadata_vm" my-project))
    (setq my-modeline-bg-color "#00aa00"))

   ((string-equal "seaice" my-project)
    (setq my-modeline-bg-color "#d787ff"))

   ((string-equal "seaiceservice" my-project)
    (setq my-modeline-bg-color "blue"))

   ((string-equal "morph-vm" my-project)
    (setq my-modeline-bg-color "blue"))

   ((string-equal "dapaggr" my-project)
    (setq my-modeline-bg-color "#5c5cff"))

   ((string-equal "snotelpages" my-project)
    (setq my-modeline-bg-color "#af5f00")
    (setq idlwave-shell-explicit-file-name "idl83"))

   ((string-equal "seaiceprojects" my-project)
    (setq my-modeline-bg-color "#afff00")
    (setq idlwave-shell-explicit-file-name "idl83"))

   ((string-equal "sea_ice_tools" my-project)
    (setq my-modeline-bg-color "green"))
   )
  )

 ;; Home Laptop: kitteh!!
 ((string-match ".*kitteh.*" (system-name))
  (progn (setq my-modeline-fg-color "#ff40ff")
	 (setq my-modeline-bg-color "#932092")))

 ;; user archive.
 ((string-match (user-login-name) "archive")
  (progn (setq my-modeline-fg-color "#000000")
	 (setq my-modeline-bg-color "#ff0000")))

 (running-macos
  (progn
    (setq my-modeline-fg-color "#5d478b")
    (setq my-modeline-bg-color "#ab82ff")
    ))

 ;; Default values if nothing else matched.
 (t (progn (setq my-modeline-fg-color "#ffd700")
	   (setq my-modeline-bg-color "#6b8e23")))
 )



(defun mhs-update-mode-line ()
  "Set the mode and menu bar colors according to customizations."
  (interactive)
  ;; Set both mode and menu rather than inheriting
  (set-face-foreground 'mode-line my-modeline-fg-color)
  (set-face-background 'mode-line my-modeline-bg-color)

  (set-face-foreground 'menu my-modeline-fg-color)
  (set-face-background 'menu my-modeline-bg-color)

  (set-face-foreground 'vertical-border my-modeline-fg-color)
  (set-face-background 'vertical-border my-modeline-bg-color)
  )

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-tomorrow-night t)
;;   )

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t)
;;   )

;; (use-package doom-themes :ensure t
;;   :config
;;   (load-theme 'doom-gruvbox)
;;   (setq doom-gruvbox-dark-variant "hard"))

(use-package ample-theme
  :ensure t
  :config
  (load-theme 'ample-flat t))
;; (use-package ample-theme :ensure t)
;; ;; when you want to have fancy mode lines on App Emacs, but still deal with terminal windows...
;; (if (<= (display-color-cells) 256)
;;     (load-theme 'ample-flat-256 t)
;;   (load-theme 'ample-flat-plus t))

;; (mhs-update-mode-line)
(provide 'emacs-custom-faces)

;;; .EMACS-CUSTOM-FACES ends here
