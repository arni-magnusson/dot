;;-------------
;; 2.1  Visual
;;-------------
(add-hook 'window-setup-hook 'frame-maximize)
;;------------------
;; 3.2  Executables
;;------------------
(setq diff-command "winmergeu")
(setq diff-switches "-e")
;;-------------
;; 3.3  Backup
;;-------------
(setq backup-directory-alist '(("." . "c:/home/emacs/backup/")))
;;-------------
;; 3.4  Recent
;;-------------
(setq recentf-save-file "c:/home/emacs/.recentf")
;;-----------
;; 5.1  File
;;-----------
(defvar ps-outfile "c:/out.ps"
  "Filename for `ps-print-buffer-with-faces'.")
;;-----------
;; 5.3  View
;;-----------
(defun font-1 ()
  "Set default font to Lucida Console bold 2."
  (interactive)
  (setq-default line-spacing 0)
  (set-frame-font "-*-lucida console-bold-r-*-*-2-*-*-*-*-*-iso8859-1"))
(defun font-2 ()
  "Set default font to Courier New bold 4."
  (interactive)
  (setq-default line-spacing 0)
  (set-frame-font "-*-courier new-bold-r-*-*-4-*-*-*-*-*-iso8859-1"))
(defun font-3 ()
  "Set default font to Courier New bold 6."
  (interactive)
  (setq-default line-spacing 0)
  (set-frame-font "-*-courier new-bold-r-*-*-6-*-*-*-*-*-iso8859-1"))
(defun font-4 ()
  "Set default font to Courier New 11."
  (interactive)
  (setq-default line-spacing 0)
  (set-frame-font "-*-courier new-normal-r-*-*-11-*-*-*-*-*-iso8859-1"))
(defun font-5 ()
  "Set default font to Courier New 13."
  (interactive)
  (setq-default line-spacing 0)
  (set-frame-font "-*-courier new-normal-r-*-*-13-*-*-*-*-*-iso8859-1"))
(defun font-6 ()
  "Set default font to Courier New bold 13."
  (interactive)
  (setq-default line-spacing 0)
  (set-frame-font "-*-courier new-bold-r-*-*-13-*-*-*-*-*-iso8859-1"))
(defun font-7 ()
  "Set default font to Courier New bold 22."
  (interactive)
  (setq-default line-spacing 0)
  (set-frame-font "-*-courier new-bold-r-*-*-22-*-*-*-*-*-iso8859-1"))
(defun font-8 ()
  "Set default font to Microsoft Sans Serif 9."
  (interactive)
  (setq-default line-spacing 1)
  (set-frame-font "-*-microsoft sans serif-normal-r-*-*-9-*-*-*-*-*-iso8859-1"))
(defun font-9 ()
  "Set default font to Microsoft Sans Serif 13."
  (interactive)
  (setq-default line-spacing 1)
  (set-frame-font "-*-microsoft sans serif-normal-r-*-*-13\
-*-*-*-*-*-iso8859-1"))
(defun font-0 ()
  "Set default font to Times New Roman 15."
  (interactive)
  (setq-default line-spacing 1)
  (set-frame-font "-*-times new roman-normal-r-*-*-15-*-*-*-*-*-iso8859-1"))
(defun frame-maximize ()
  "Maximize window."
  (interactive)
  (modify-frame-parameters nil '((fullscreen . maximized))))
(defun frame-restore ()
  "Restore window."
  (interactive)
  (modify-frame-parameters nil '((fullscreen . nil))))
;;-----------
;; 6.2  ADMB
;;-----------
(setq admb-init "admb-set & ")
;;--------
;; 6.16 R
;;--------
(defvar R-editor "emacs"
  "Editor called by R process with 'edit()' command.")
(setq ess-history-file "c:/home/r/.Rhistory")
;;------------
;; 7.12 Dired
;;------------
(defvar dired-fmt "-Ggl"
  "Format for dired. See help for variable `dired-listing-switches'.")
