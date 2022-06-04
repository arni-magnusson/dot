;;------------
;; 2.3  Faces
;;------------
(setq-default line-spacing 1) ; or :height 126
;;------------------
;; 3.2  Executables
;;------------------
(setq diff-command "kompare")
(setq diff-switches "")
;;-------------
;; 3.3  Backup
;;-------------
(setq backup-directory-alist '(("." . "~/emacs/backup/")))
;;-------------
;; 3.4  Recent
;;-------------
(setq recentf-save-file "~/emacs/.recentf")
;;-----------
;; 5.1  File
;;-----------
(defvar ps-outfile "~/out.ps" "Filename for `ps-print-buffer-with-faces'.")
;;-----------
;; 5.3  View
;;-----------
;;  8  Noto Sans Mono 14  condensed (235 chars)
;;  9  Noto Mono 16       clearer than courier
;; 10  Libertine Mono 16  serif but mono

(defun font-1 ()
  "Set default font to Latin Modern 14"
  (interactive)
  (setq-default line-spacing 0)
  (set-frame-font "-*-Latin Modern Mono-normal-*-*-*-14-*-*-*-*-*-*-*"))
(defun font-2 ()
  "Set default font to Courier 10"
  (interactive)
  (setq-default line-spacing 1)
  (set-frame-font "-*-Courier 10 Pitch-normal-*-*-*-12-*-*-*-*-*-*-*"))
(defun font-3 ()
  "Set default font to Courier 12"
  (interactive)
  (setq-default line-spacing 1)
  (set-frame-font "-*-Courier 10 Pitch-normal-*-*-*-12-*-*-*-*-*-*-*"))
(defun font-4 ()
  "Set default font to Courier 14"
  (interactive)
  (setq-default line-spacing 1)
  (set-frame-font "-*-Courier 10 Pitch-normal-*-*-*-14-*-*-*-*-*-*-*"))
(defun font-5 ()
  "Set default font to Courier 16"
  (interactive)
  (setq-default line-spacing 1)
  (set-frame-font "-*-Courier 10 Pitch-normal-*-*-*-16-*-*-*-*-*-*-*"))
(defun font-6 ()
  "Set default font to Courier bold 16"
  (interactive)
  (setq-default line-spacing 1)
  (set-frame-font "-*-Courier 10 Pitch-bold-*-*-*-16-*-*-*-*-*-*-*"))
(defun font-7 ()
  "Set default font to Courier bold 36"
  (interactive)
  (setq-default line-spacing 1)
  (set-frame-font "-bitstream-Courier 10 Pitch-bold-*-*-*-36-*-*-*-*-*-*-*"))
(defun font-8 ()
  "Set default font to Noto Sans Mono 14"
  (interactive)
  (setq-default line-spacing 0)
  (set-frame-font "-*-Noto Sans Mono-normal-*-*-*-14-*-*-*-*-*-*-*"))
(defun font-9 ()
  "Set default font to Noto Mono 16"
  (interactive)
  (setq-default line-spacing 1)
  (set-frame-font "-*-Noto Mono-normal-*-*-*-16-*-*-*-*-*-*-*"))
(defun font-0 ()
  "Set default font to Linux Libertine Mono 16"
  (interactive)
  (setq-default line-spacing 2)
  (set-frame-font "-*-Linux Libertine Mono O-normal-*-*-*-16-*-*-*-*-*-*-*"))
;; (defalias 'frame-maximize 'ignore) ; not required on Hafstrambur
;; (defun frame-maximize ()
;;   "Maximize frame."
;;   (interactive)
;;   (let ((maxcols (+ (truncate (/ (display-pixel-width)(frame-char-width)))
;;                     0))
;;         (maxrows (+ (truncate (/ (display-pixel-height)(frame-char-height)))
;;                     0)))
;;     (set-frame-size (selected-frame) maxcols maxrows)
;;     (set-frame-position (selected-frame) 0 0)))
;; (defun frame-maximize-x ()
;;   "Maximize frame." ;; alternative approach, smaller frame and inconsistent
;;   (interactive)
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;                          '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;                          '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
;; (defun frame-restore ()
;;   "Restore window."
;;   (interactive)
;;   (ignore))
(defun frame-maximize ()
  "Maximize window."
  (interactive)
  (modify-frame-parameters nil '((fullscreen . maximized))))
(defun frame-restore ()
  "Restore window."
  (interactive)
  (modify-frame-parameters nil '((fullscreen . nil))))
;;--------
;; 6.16 R
;;--------
(setq ess-history-file "~/r/.Rhistory")
;;----------
;; 6.18 TMB
;;----------
(setq tmb-compile-args ",'-fno-gnu-unique -O0 -Wall'")
(setq tmb-debug-args ",'-fno-gnu-unique -g -O0'")
;;------------
;; 7.12 Dired
;;------------
(defvar dired-fmt "--group-directories-first -Ggl"
  "Format for dired. See help for variable `dired-listing-switches'.")
