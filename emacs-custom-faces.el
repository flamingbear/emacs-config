;;; .EMACS-CUSTOM-FACES --- customize the modeline display based on machine, user and BUILD environment.

;; Copyright (C) 2011 Matt Savoie

;; Author: Matt Savoie <savoie@nsidc.org>
;; Maintainer: Matt Savoie <savoie@nsidc.org>
;; Created: 07 Sep 2011
;; Version: 1.0
;; Keywords:

 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to <savoie@nsidc.org>) or from the
;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; |Matt Savoie|<savoie@nsidc.org>
;; |customize the modeline display based on machine, user and BUILD environment.
;; |$Date$|$Revision: 19611 $|~/packages/.emacs-custom-faces

;;; Commentary:

;;; Change log:
;; $Log$
;;

;;; Code:

(defconst -version (substring "$Revision: 19611 $" 11 -2)
  "$Id: emacs-custom-faces.el 19611 2011-10-20 21:09:37Z savoie $

Report bugs to: Matt Savoie <savoie@nsidc.org>")


;; Set up the  default values for frame creation
;;-------------------------------------------
(setq default-frame-alist 
      (quote ((mouse-color . "sky blue")
              (cursor-color . "orange1") 
              (tool-bar-lines . 0) 
              (top . 25) 
              (left . 54) 
              (menu-bar-lines . 1) 
              (background-color . "gray12") 
              (foreground-color . "gray89"))))

(add-to-list 'default-frame-alist 
             (cond ((>= (x-display-pixel-height) 1578)
                    '(height . 62))
                   ((>= (x-display-pixel-height) 1200)
                    '(height . 51))
                   ((>= (x-display-pixel-height) 1050)
                    '(height . 47))
                   ((>= (x-display-pixel-height) 900)
                    '(height . 40))
                   (t '(height . 30))))

(add-to-list 'default-frame-alist 
             (cond ((>= (x-display-pixel-width) 1280)
                    '(width . 120))
                   (t '(width . 93))))

;;-------------------------------------------




;; We like to know what machine we're running on.  So change the color
;; depending on where you're running from. Also set defaults like what idl
;; command to run for specific versions.
(setq my-menu-fg-color "white")
(setq my-menu-bg-color "brown")


;; If we're not running NX set the fonts like I like them.  Otherwise, we
;; default to 10x20
(setq nx-env (getenv "NX_ROOT"))
(setq use-inconsolata t)
(if  (or (not (string= nx-env nil))
         (not (string-match "snow[^s]" (system-name)))
         (string-match "masie" (user-login-name))
         (string-match "nise" (user-login-name))
         (string-match "cdr" (user-login-name)))
    (setq use-inconsolata nil))

;; special case
(if running-on-dev-vm (setq use-inconsolata 't))

(cond (use-inconsolata (mhs-use-inconsolata))
      (running-macos (mhs-use-monaco) )
      (t (mhs-use-normal-face)))

;; This was the stuff below in my .gnu-emacs-custom but I've updated to use
;; different fonts for NX and whatnots.:


;; I'm starting to use BUILD as the default environment everywhere, so let's
;; get it here and set it to unknown if it's not set so that colors can be set
;; up according to your build.
(setq build (getenv "BUILD"))

(if (eq build nil) 
    (setq build "unknown"))


(cond ((string-match (user-login-name) "nrtsig")
       (progn
         (cond ((string-match build  "F17_prod") 
                (setq my-menu-fg-color "red")
                (setq my-menu-bg-color "#483d8b")) ;darkslateblue
               ((string-match build  "F17_test") 
                (setq my-menu-fg-color "moccasin")
                (setq my-menu-bg-color "#483d8b")) ; darkslateblue
               )))

      ;; NOAA Combined
      ((string-match build "n0046_dev")
       (progn (setq my-menu-fg-color "turquoise")   ;midnightblue
              (setq my-menu-bg-color "darkgreen"))) ;cornflowerblue

      ((string-match (system-name) "wuzzles.colorado.edu")
       (progn (setq my-menu-fg-color "#2f4f4f")   ;dark slate gray 
              (setq my-menu-bg-color "#c0ff3e"))) ;olivedrab1 


      ((string-match (user-login-name) "archive")
       (progn (setq my-menu-fg-color "black")
              (setq my-menu-bg-color "red")))


      ;; You'll get the same colors for MASIE emacs sessions for any
      ;; machine... Set the title.
      ((string-match build "MASIE_dev")
       (progn 
         (setq idlwave-shell-explicit-file-name "my_idl.7.0.sh")
         (setq my-menu-bg-color "#7a378b") ; mediumOrchid4
         (setq my-menu-fg-color "spring green")))

      ((string-match build "MASIE_test")
       ;;(setq idlwave-shell-explicit-file-name "my_idl.7.0.sh")
       (progn (setq idlwave-shell-explicit-file-name 
                    "/home/savoie/local/bin/my_idl.6.4.sh")
              (setq my-menu-bg-color "#7a378b") ; turquoise1
              (setq my-menu-fg-color "gold")))

      ((string-match build "MASIE_prod")
       (progn ;;(setq idlwave-shell-explicit-file-name "my_idl.7.0.sh")
         (setq idlwave-shell-explicit-file-name "my_idl.6.4.sh")
         (setq my-menu-bg-color "#7a378b") ; mediumOrchid4
         (setq my-menu-fg-color "palevioletred3")))

      ;; We're back to nusnow...
      ((string-match "nusnow.colorado" (system-name))
       (progn
         (setq my-menu-fg-color "greenyellow")
         (setq my-menu-bg-color "darkolivegreen")
         (when (string-match user-login-name "nise")
           (setq my-menu-fg-color "darkorange3"))))

      ;; Works with snow.colo only now...
      ((string-match "^snow.colorado.edu" (system-name))
       (progn 
         (setq idlwave-shell-explicit-file-name "my_idl.8.1.sh")
         (setq my-menu-bg-color "moccasin")
         (setq my-menu-fg-color "black")
         (cond ((string-match (user-login-name) "nise")
                (setq my-menu-fg-color "dodgerblue")))
         (cond ((string-match build  "development")
                (setq my-menu-fg-color "darkmagenta")
                )
               ((string-match build  "testing")
                (setq my-menu-fg-color "darkgreen")
                )
               ((string-match build  "production")
                (setq my-menu-fg-color "red")
                )
               ((string-match build  "F17_dev")
                                        ;(setq idlwave-shell-explicit-file-name "my_special_idl.8.1.sh")
                (setq idlwave-shell-explicit-file-name "my_idl.8.1.sh")
                (setq my-menu-fg-color "mediumvioletred")
                (setq my-menu-bg-color "burlywood")
                )
               ;; ((string-match build  "F17_snow")
               ;;  (setq idlwave-shell-explicit-file-name "my_idl.7.0.sh")
               ;;  (setq my-menu-fg-color "blue")
               ;;  (setq my-menu-bg-color "burlywood")
               ;;  )
               ((string-match build  "cdr_dev")
                (setq idlwave-shell-explicit-file-name "my_idl.8.1.sh")
                (setq my-menu-fg-color "greenyellow")
                (setq my-menu-bg-color "royalblue")
                )
               ((string-match build  "cdr_test")
                (setq my-menu-fg-color "gold")
                (setq my-menu-bg-color "royalblue")
                )
               ((string-match build  "F17_test")
                (setq my-menu-fg-color "dark green")
                (setq my-menu-bg-color "burlywood")
                )
               ((string-match build  "test_F15")
                (setq my-menu-fg-color "blue violet")
                (setq my-menu-bg-color "burlywood")
                )
               ((string-match build  "F15_bridge")
                (setq my-menu-fg-color "royal blue")
                (setq my-menu-bg-color "burlywood")
                )
               ((string-match build  "F15_production")
                (setq my-menu-fg-color "deep pink")
                (setq my-menu-bg-color "burlywood"))
               )))

      ((string-match "nsidc-snowblow" (system-name))
       (progn (setq my-menu-fg-color "gold")
              (setq my-menu-bg-color "#9f79ee") ; mediumpurple2
              ;; (setq idlwave-shell-explicit-file-name "my_idl.6.4.sh")
              (setq idlwave-shell-explicit-file-name "my_idl.8.1.sh")))
      
      ((string-match (system-name) "sidads.colorado.edu")
       (progn (setq my-menu-fg-color "#8b3a3a")   ; indianred4
              (setq my-menu-bg-color "#ffdab9"))) ;peachpuff
      
      ((string-match (system-name) "arctic3.colorado.edu")
       (progn (setq my-menu-fg-color "blue4")
              (setq my-menu-bg-color "salmon")
              (setq idlwave-shell-explicit-file-name "my_idl.6.3.sh")
              (setq idlwave-system-directory "/usr/local/rsi/idl/")
              ))

      ((string-match (system-name) "arctic5.colorado.edu")
       (progn (setq my-menu-fg-color "yellow")
              (setq my-menu-bg-color "purple")
              (setq idlwave-shell-explicit-file-name "my_idl.7.0.sh")
              (setq idlwave-system-directory "/usr/local/rsi/idl/")
              ))


      ((string-match (system-name) "arctic4.colorado.edu")
       (progn (setq my-menu-fg-color "#00bfff")   ; deep sky blue
              (setq my-menu-bg-color "#adff2f"))) ;green yellow

      ;; POPSICLE only one to start.
      (running-on-dev-vm
       (progn (setq my-menu-fg-color "palevioletred1")
              (setq my-menu-bg-color "darkred")))

      (running-macos
       (progn
         (setq my-menu-fg-color "#baf257")
         (setq my-menu-bg-color "#738466")))
      
      ;; Default values
      (t (progn (setq my-menu-fg-color "gold")
                (setq my-menu-bg-color "olive drab"))))

(set-face-foreground 'menu my-menu-fg-color)
(set-face-background 'menu my-menu-bg-color)


;;; .EMACS-CUSTOM-FACES ends here
