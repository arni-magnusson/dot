;;==============================================================================
;;
;; 1  OS-SPECIFICS
;;
;;==============================================================================
;; Menu bar must be disabled in Windows before Emacs is maximized
(if menu-bar-mode (menu-bar-mode 0))
(if (string-match "windows" (prin1-to-string system-type))
    (load "~/.emacs-windows.el" nil t)(load "~/.emacs-linux.el" nil t))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;;==============================================================================
;;
;; 2  INTERFACE
;;
;;==============================================================================
;;------------
;; 2.1  Sound
;;------------
(setq ring-bell-function 'ignore)
;;-------------
;; 2.2  Visual
;;-------------
;; (defun arni-window-setup-hook ()(frame-maximize)) ; linux max frame
;; (add-hook 'window-setup-hook 'arni-window-setup-hook)
(blink-cursor-mode 0                            ) ; no blinking
(column-number-mode t                           ) ; coordinates
(font-5                                         ) ; default font
(require 'scroll-bar)(set-scroll-bar-mode 'right) ; linux scroll bar
(setq frame-title-format "%b"                   ) ; window title
(setq inhibit-startup-echo-area-message "arnim" ) ; blank minibuffer
(setq inhibit-startup-screen t                  ) ; blank buffer
(setq initial-scratch-message nil               ) ; blank buffer, really
(setq line-number-display-limit-width 10000     ) ; line numbers with long lines
(setq-default show-trailing-whitespace t        ) ; show white trail
(setq-default truncate-lines t                  ) ; long lines
(show-paren-mode t                              ) ; match, default Emacs >=28.1
(require 'tool-bar)(tool-bar-mode 0             ) ; no toolbar
;;--------------
;; 2.3  Editing
;;--------------
(defalias 'yes-or-no-p 'y-or-n-p              ) ; ask y or n
(delete-selection-mode 1                      ) ; typing replaces selected text
(electric-indent-mode -1                      ) ; RET is just newline
(prefer-coding-system 'utf-8                  ) ; utf8 if not sure
(setq duplicate-line-final-position -1        ) ; Move cursor after duplicating
(require 'imenu)(setq imenu-max-items 43      ) ; code navigation
(setq initial-major-mode 'text-mode           ) ; text-mode for scratch buffer
(setq mail-host-address "spc.int"             ) ; change-log-mode
(setq save-abbrevs nil                        ) ; don't save abbrevs
(setq scroll-error-top-bottom t               ) ; pgup and pgdn all the way
(setq user-full-name "Arni Magnusson"         ) ; change-log-mode
(setq select-enable-clipboard t               ) ; linux clipboard
(setq vc-follow-symlinks t                    ) ; open symlinks without warning
(setq-default buffer-file-coding-system 'utf-8) ; utf8 instead of latin1
(setq-default fill-column 80                  ) ; text width
(setq-default indent-tabs-mode nil            ) ; no tab characters
(setq-default major-mode 'text-mode           ) ; text-mode for new buffer
(setq-default require-final-newline t         ) ; ensure file ends with newline
(setq-default sentence-end-double-space nil   ) ; one space after period
(setq-default sort-fold-case t                ) ; sort-lines ignoring case
(setq bs-must-always-show-regexp ; include in buffer cycle
      "Help\\|R\\|SQL\\|compilation\\|gud\\|info\\|input\\|shell")
;;------------
;; 2.4  Faces
;;------------
;; Only special faces should inherit
;; family width height weight slant fg bg underline overline strike box inherit
(defconst - 'unspecified "Shorthand for unsetting face attributes.")
(defun arni-colors ()
  "Apply my preferred colors."
  (interactive) ; provide enough details to reset faces
  (if (not window-system)
      (progn ; black  red  green  yellow  blue  magenta  cyan  white
        (set-face-attribute 'default nil :background -)
        (set-face-attribute 'escape-glyph nil :foreground "red")
        (set-face-attribute 'fixed-pitch nil :family -)
        (set-face-attribute 'font-lock-builtin-face nil :foreground "red"
                            :weight -)
        (set-face-attribute 'font-lock-comment-face nil :foreground "yellow"
                            :weight -)
        (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground -)
        (set-face-attribute 'font-lock-constant-face nil :foreground -
                            :underline t :weight -)
        (set-face-attribute 'font-lock-doc-face nil :foreground - :weight -)
        (set-face-attribute 'font-lock-function-name-face nil :foreground -
                            :weight -)
        (set-face-attribute 'font-lock-keyword-face nil :foreground "blue"
                            :weight -)
        (set-face-attribute 'font-lock-regexp-grouping-backslash nil :inherit -)
        (set-face-attribute 'font-lock-regexp-grouping-construct nil :inherit -)
        (set-face-attribute 'font-lock-string-face nil :foreground "green")
        (set-face-attribute 'font-lock-type-face nil :foreground "magenta"
                            :weight -)
        (set-face-attribute 'font-lock-variable-name-face nil :foreground -
                            :weight -)
        (set-face-attribute 'font-lock-warning-face nil :foreground "red"
                            :weight 'bold)
        (set-face-attribute 'fringe nil :background -)
        (if (>= emacs-major-version 28)
            (set-face-attribute 'help-key-binding nil :background - :box -
                                :foreground -))
        (set-face-attribute 'isearch nil :background "yellow" :foreground -)
        (set-face-attribute 'isearch-fail nil :background "red")
        (set-face-attribute 'italic nil :underline -)
        (set-face-attribute 'lazy-highlight nil :background "yellow")
        (set-face-attribute 'minibuffer-prompt nil :foreground -)
        (set-face-attribute 'show-paren-match nil :background "green")
        (set-face-attribute 'show-paren-mismatch nil :background "red")
        (set-face-attribute 'trailing-whitespace nil :background "cyan"))
    (progn
      (set-face-attribute 'default nil :background "gray85")
      (set-face-attribute 'escape-glyph nil :foreground "brown4" :weight 'bold)
      (set-face-attribute 'fixed-pitch nil :family -)
      (set-face-attribute 'font-lock-builtin-face nil :foreground "red")
      (set-face-attribute 'font-lock-comment-face nil :foreground "gray60")
      (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground -)
      (set-face-attribute 'font-lock-constant-face nil :foreground -
                          :underline t :weight -)
      (set-face-attribute 'font-lock-doc-face nil :foreground - :weight -)
      (set-face-attribute 'font-lock-function-name-face nil :foreground -
                          :weight 'bold)
      (set-face-attribute 'font-lock-keyword-face nil :foreground "mediumblue")
      (set-face-attribute 'font-lock-regexp-grouping-backslash nil :inherit -)
      (set-face-attribute 'font-lock-regexp-grouping-construct nil :inherit -)
      (set-face-attribute 'font-lock-string-face nil :foreground "forestgreen")
      (set-face-attribute 'font-lock-type-face nil :foreground "magenta2"
                          :weight -)
      (set-face-attribute 'font-lock-variable-name-face nil :foreground -
                          :weight -)
      (set-face-attribute 'font-lock-warning-face nil :foreground "red"
                          :weight 'bold)
      (set-face-attribute 'fringe nil :background -)
      (if (>= emacs-major-version 28)
          (set-face-attribute 'help-key-binding nil :background - :box -
                              :foreground -))
      (set-face-attribute 'isearch nil :background "gold" :foreground -)
      (set-face-attribute 'isearch-fail nil :background "tomato")
      (set-face-attribute 'italic nil :underline -)
      (set-face-attribute 'lazy-highlight nil :background "palegoldenrod")
      (set-face-attribute 'minibuffer-prompt nil :foreground -) ; M-x
      (set-face-attribute 'region nil :background "lightskyblue")
      (set-face-attribute 'show-paren-match nil :background "chartreuse")
      (set-face-attribute 'show-paren-mismatch nil :background "red")
      (set-face-attribute 'trailing-whitespace nil :background "gray95"))))
(defun bg (face)
  "Return FACE background color."
  (face-attribute face :background))
(defun fg (face)
  "Return FACE foreground color."
  (face-attribute face :foreground))
(arni-colors)
;;==============================================================================
;;
;; 3  FILES
;;
;;==============================================================================
;;--------------
;; 3.1  Startup
;;--------------
;; emacs -R means start in R-mode
(setq command-switch-alist '(("-R" . (lambda (arg)(R-mode)))))
(setq inhibit-default-init t) ; linux suppress default.el
;; Remember startup dir, then build `load-path' recursively
(defvar startup-directory default-directory)
(if (file-directory-p "~/emacs/lisp/")
    (progn (cd "~/emacs/lisp/")
           (normal-top-level-add-subdirs-to-load-path)))
(cd startup-directory) ; return to startup dir
;;------------------
;; 3.2  Executables
;;------------------
(require 'browse-url)
(setq browse-url-browser-function 'browse-url-firefox)
(require 'grep) ; skip binaries, ignore case, linenum, recursive, no warnings
(grep-apply-setting 'grep-command "grep -Iinrs \'\'")
(add-to-list 'exec-path "~/emacs/ispell/bin")
(setenv "ispelldictdir" (concat (getenv "home") "/emacs/ispell/lib"))
(setq ispell-dictionary "american")
;;-------------
;; 3.3  Backup
;;-------------
(setq auto-save-default nil)          ; no #file.txt#
(setq auto-save-list-file-prefix nil) ; no .emacs.d/auto-save-list
(setq create-lockfiles nil)           ; no .#files in VBox guest
(defun rm-large-files (dir kb)
  "Remove files larger than KB from directory DIR."
  (let* ((w32-get-true-file-attributes nil)
         (nested-list (directory-files-and-attributes dir t "[^.]" t))
         (file (mapcar 'car nested-list))
         (size (mapcar (lambda (x)(nth 8 x)) nested-list)))
    (dotimes (i (length nested-list))
      (if (> (nth i size)(* kb 1024))
          (delete-file (nth i file))))))
(if (string-equal (format-time-string "%u") "5") ; Friday clean
    (rm-large-files "~/emacs/backup" 1000))
(setq make-backup-files nil) ; enable in hooks for source code and documents
(setq version-control t)
(setq delete-old-versions t)
(setq kept-old-versions 1)
(setq kept-new-versions 4)
;;-------------
;; 3.4  Recent
;;-------------
(defun arni-recentf-load-hook () ; need hook before loading
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-saved-items 200)
  (add-to-list 'recentf-exclude "^[Zz]:") ; exclude Z drive (Penguin)
  (if (string-equal (format-time-string "%u") "5") ; Friday clean
      (recentf-cleanup)))
(add-hook 'recentf-load-hook 'arni-recentf-load-hook)
(recentf-mode t)
(message nil)
;;----------------
;; 3.5  Autoloads
;;----------------
;; Core packages
(autoload 'log-edit-mode     "log-edit"          "Write VC log entry."      t)
;; User packages
(autoload 'admb-mode         "admb"              "Edit ADMB code."          t)
(autoload 'cmake-mode        "cmake-mode"        "Edit CMake code."         t)
(autoload 'csharp-mode       "csharp-mode"       "Edit C# code."            t)
(autoload 'csv-mode          "csv-mode"          "View CSV (align, sort)."  t)
(autoload 'csv-nav-mode      "csv-nav"           "View CSV (other window)." t)
(autoload 'doxymacs-mode     "doxymacs"          "Doxygen minor mode."      t)
(autoload 'ess-bugs-mode     "ess-bugs-d"        "Edit BUGS code."          t)
(defalias 'bugs-mode 'ess-bugs-mode)
(autoload 'html-helper-mode  "html-helper-mode"  "Edit HTML documents."     t)
(autoload 'iss-mode          "iss-mode"          "Edit Inno Setup scripts." t)
(autoload 'json-mode         "json-mode"         "Edit JSON data."          t)
(autoload 'julia-mode        "ess-site"          "Edit Julia code."         t)
(autoload 'magit-status      "magit"             "Browse Git repository."   t)
(autoload 'markdown-mode     "markdown-mode"     "Edit Markdown document."  t)
(autoload 'php-mode          "php-mode"          "Edit PHP code."           t)
(autoload 'R                 "ess-site"          "Interactive R session."   t)
(autoload 'R-mode            "ess-site"          "Edit R code."             t)
(autoload 'r-mode "ess-site") ;*-R-*
(autoload 'Rd-mode           "ess-site"          "Edit R documentation."    t)
(autoload 'stan-mode         "stan-mode"         "Edit Stan code."          t)
;; "latex" in Windows, "auctex" in Linux
(autoload 'TeX-latex-mode    "tex-site"          "Edit LaTeX document."     t)
(autoload 'tabbar-mode       "tabbar"            "Visible buffer tabs."     t)
(autoload 'titlecase-dwim    "titlecase"         "Convert to Title Case."   t)
(autoload 'visual-basic-mode "visual-basic-mode" "Edit Visual Basic code."  t)
;;-------------------
;; 3.6  Associations
;;-------------------
(defalias 'aspic-mode 'conf-space-mode)
(defun coleraine-mode ()
  "View Coleraine results (results.dat or run01.res)."
  (interactive)
  (conf-space-mode)
  (setq show-trailing-whitespace nil)
  (redraw-display))
(defalias 'gadget-mode 'conf-windows-mode)
(defalias 'sam-mode 'conf-space-mode)
(defvar ext-code nil "Filenames related to programming languages.")
(defvar ext-conf nil "Filenames related to program settings.")
(defvar ext-doc  nil "Filenames related to document markup.")
(defvar ext-io   nil "Filenames related to program input/output.")
(defvar ext-util nil "Filenames related to programming utilities.")
(setq ext-code
      '(("\\.adb$"      . ada-mode)
        ("\\.ads$"      . ada-mode)
        ("\\.tpl$"      . admb-mode)
        ("\\.bat$"      . bat-mode)
        ("\\.cmd$"      . bat-mode)
        ("\\.c$"        . c-mode)
        ("\\.lex$"      . c-mode)
        ("\\.cc$"       . c++-mode)
        ("\\.cpp$"      . c++-mode)
        ("\\.cxx$"      . c++-mode)
        ("\\.h$"        . c++-mode)
        ("\\.hpp$"      . c++-mode)
        ("\\.htp$"      . c++-mode)
        ("\\.cs$"       . csharp-mode)
        ("\\.el$"       . emacs-lisp-mode)
        ("\\.el.gz$"    . emacs-lisp-mode)
        ("\\.f90$"      . f90-mode)
        ("\\.f95$"      . f90-mode)
        ("\\.f$"        . fortran-mode)
        ("\\.for$"      . fortran-mode)
        ("\\.java$"     . java-mode)
        ("\\.js$"       . javascript-mode)
        ("\\.jl$"       . julia-mode)
        ("\\.pl$"       . perl-mode)
        ("\\.pm$"       . perl-mode)
        ("\\.php$"      . php-mode)
        ("\\.sql$"      . sql-mode)
        ("\\.py$"       . python-mode)
        ("\\.q$"        . R-mode)
        ("\\.R$"        . R-mode)
        ("\\.Rhistory$" . R-mode)
        ("\\.s$"        . R-mode)
        ("\\.splus$"    . R-mode)
        ("\\.ssc$"      . R-mode)
        ("/NAMESPACE"   . R-mode)
        ("\\.rb$"       . ruby-mode)
        ("\\.sh$"       . sh-mode)
        ("\\.bas$"      . visual-basic-mode)
        ("\\.vbs$"      . visual-basic-mode)
        ("\\.y$"        . c-mode)))
(setq ext-conf ; [section]
      ;; space:par val,#comment
      ;; unix:par=val,#comment
      ;; windows:par=val,par val,;comment,#gadgetpar
      '(("\\.afm$"         . conf-space-mode)
        ("\\.age_length$"  . conf-unix-mode)
        ("\\.cfg$"         . conf-space-mode)
        ("\\.conf$"        . conf-space-mode)
        ("\\.cpt$"         . conf-space-mode)
        ("\\.ctl$"         . conf-space-mode)
        ("\\.dat$"         . conf-space-mode)
        ("\\.dox$"         . conf-space-mode)
        ("\\.doxyfile$"    . conf-space-mode)
        ("\\.frq$"         . conf-unix-mode)
        ("\\.gdbinit$"     . conf-space-mode)
        ("\\.gitignore$"   . conf-space-mode)
        ("\\.inputrc$"     . conf-space-mode)
        ("\\.json$"        . json-mode)
        ("\\.lst$"         . conf-space-mode)
        ("\\.muttrc$"      . conf-space-mode)
        ("\\.nanorc$"      . conf-space-mode)
        ("\\.par$"         . conf-space-mode)
        ("\\.pin$"         . conf-space-mode)
        ("\\.pinerc$"      . conf-space-mode)
        ("\\.Rproj$"       . conf-space-mode)
        ("\\.xmodmap"      . conf-space-mode)
        ("/Doxyfile"       . conf-space-mode)
        ("/Rconsole"       . conf-space-mode)
        ("/config$"        . conf-space-mode)
        ("/fstab"          . conf-space-mode)
        ("\\.gmtcommands4" . conf-unix-mode)
        ("\\.gmtdefaults4" . conf-unix-mode)
        ("\\.ini$"         . conf-unix-mode)
        ("\\.desktop$"     . conf-unix-mode)
        ("\\.sys$"         . conf-unix-mode)
        ("\\.asm$"         . conf-windows-mode)
        ("\\.def$"         . conf-windows-mode)
        ("\\.bmk$"         . emacs-lisp-mode)
        ("\\.emacs$"       . emacs-lisp-mode)
        ("\\.recentf$"     . emacs-lisp-mode)
        ("\\.reg$"         . emacs-lisp-mode)
        ("\\.bashrc$"      . sh-mode)
        ("\\.login$"       . sh-mode)
        ("\\.profile$"     . sh-mode)
        ("\\.Rprofile$"    . R-mode)
        ("\\.ss$"          . conf-space-mode)
        ("\\.ss_new$"      . conf-space-mode)
        ("\\.sso$"         . conf-space-mode)
        ("\\.tcshrc$"      . sh-mode)
        ("\\.xml$"         . xml-mode)
        ("\\.yaml$"        . conf-space-mode)
        ("\\.yml$"         . conf-space-mode)))
(setq ext-doc
      '(("\\.bib$"     . bibtex-mode)
        ("\\.bst$"     . bibtex-style-mode)
        ("\\.css$"     . css-mode)
        ("\\.htm$"     . html-helper-mode)
        ("\\.html$"    . html-helper-mode)
        ("\\.info$"    . info-buffer)
        ("\\.md$"      . markdown-mode)
        ("\\.Rmd$"     . markdown-mode)
        ("\\.org$"     . org-mode)
        ("\\.pdf$"     . ps-mode)
        ("\\.eps$"     . ps-mode)
        ("\\.pfb$"     . ps-mode)
        ("\\.ps$"      . ps-mode)
        ("\\.Rd$"      . Rd-mode)
        ("\\.rst$"     . rst-mode)
        ("\\.bbl$"     . TeX-latex-mode)
        ("\\.cls$"     . TeX-latex-mode)
        ("\\.dtx$"     . TeX-latex-mode)
        ("\\.ins$"     . TeX-latex-mode)
        ("\\.Rnw$"     . TeX-latex-mode)
        ("\\.sty$"     . TeX-latex-mode)
        ("\\.tex$"     . TeX-latex-mode)
        ("\\.texi$"    . texinfo-mode)
        ("\\.texinfo$" . texinfo-mode)
        ("\\.txi$"     . texinfo-mode)))
(setq ext-io
      '(("\\.a7inp$"      . aspic-mode)
        ("\\.inp$"        . aspic-mode)
        ("\\.res$"        . coleraine-mode)
        ("\\.ref$"        . coleraine-mode)
        ("\\.rep$"        . coleraine-mode)
        ("\\.grp$"        . conf-space-mode)
        ("\\.csv$"        . csv-mode)
        ("\\.agg$"        . gadget-mode)
        ("\\.aut$"        . gadget-mode)
        ("\\.cap$"        . gadget-mode)
        ("\\.catch$"      . gadget-mode)
        ("\\.comm$"       . gadget-mode)
        ("\\.data$"       . gadget-mode)
        ("\\.eat$"        . gadget-mode)
        ("\\.eft$"        . gadget-mode)
        ("\\.fleet$"      . gadget-mode)
        ("\\.igfs$"       . gadget-mode)
        ("\\.imm$"        . gadget-mode)
        ("\\.in$"         . gadget-mode)
        ("\\.ind$"        . gadget-mode)
        ("\\.init$"       . gadget-mode)
        ("\\.lw$"         . gadget-mode)
        ("\\.maturity$"   . gadget-mode)
        ("\\.mat$"        . gadget-mode)
        ("\\.ml$"         . gadget-mode)
        ("\\.out$"        . gadget-mode)
        ("\\.predict$"    . gadget-mode)
        ("\\.pred$"       . gadget-mode)
        ("\\.predprey$"   . gadget-mode)
        ("\\.preyb$"      . gadget-mode)
        ("\\.print$"      . gadget-mode)
        ("\\.ratio$"      . gadget-mode)
        ("\\.rec$"        . gadget-mode)
        ("\\.refw$"       . gadget-mode)
        ("\\.refweights$" . gadget-mode)
        ("\\.spawn$"      . gadget-mode)
        ("\\.stom.num$"   . gadget-mode)
        ("\\.stom.prey$"  . gadget-mode)
        ("\\.sur$"        . gadget-mode)
        ("\\.survey$"     . gadget-mode)
        ("\\.tag$"        . gadget-mode)
        ("\\.time$"       . gadget-mode)
        ("/area$"         . gadget-mode)
        ("/cod$"          . gadget-mode)
        ("/fleet$"        . gadget-mode)
        ("/had$"          . gadget-mode)
        ("/likelihood$"   . gadget-mode)
        ("/main$"         . gadget-mode)
        ("/optinfofile$"  . gadget-mode)
        ("/output$"       . gadget-mode)
        ("/penaltyfile$"  . gadget-mode)
        ("/printfile$"    . gadget-mode)
        ("/refinputfile$" . gadget-mode)
        ("/time$"         . gadget-mode)
        ("/mutt"          . mail-mode)
        ("\\.pinchk$"     . sam-mode)
        ("\\.mcmc$"       . space-mode)))
(setq ext-util
      '(("\\.ac$"          . autoconf-mode)
        ;; ("/ChangeLog"   . change-log-mode)
        ("\\.cmake$"       . cmake-mode)
        ("/CMakeLists.txt" . cmake-mode)
        ("\\.diff$"        . diff-mode)
        ("\\.patch$"       . diff-mode)
        ("\\.ebrowse$"     . ebrowse-tree-mode)
        ("/BROWSE"         . ebrowse-tree-mode)
        ("\\.iss$"         . iss-mode)
        ("/COMMIT_EDITMSG" . log-edit-mode)
        ("/svn-commit\\."  . log-edit-mode)
        ("/Makefile"       . makefile-mode)
        ("\\.mak$"         . makefile-mode)
        ("\\.mk$"          . makefile-mode)
        ("/GNUmakefile"    . makefile-mode)
        ("/Redit"          . R-mode)
        ("/tmp/Rtmp"       . R-mode)
        ("\\.tar$"         . tar-mode)
        ("\\.tar\\."       . tar-mode)))
(setq auto-mode-alist (append ext-code ext-conf ext-doc ext-io ext-util))
;;------------------
;; 3.7  Completions
;;------------------
;; Help find-file to complete the filename of interest
(setq completion-ignored-extensions
      '(".bash_history"                                 ; .bashrc
        ".cov"                                          ; .cor (admb)
        ".html"                                         ; .org (org)
        ".aux" ".dvi" ".log" ".out" ".pdf" ".ps" ".toc" ; .tex (latex)
        ".nav" ".snm" ".vrb"                            ; .tex (beamer)
        ".png"                                          ; .R (figures)
        ".o" ".exe"                                  )) ; source code (generic)
;;==============================================================================
;;
;; 4  KEYBINDINGS
;;
;;==============================================================================
;; f1-f12    1:describe, 2-3:location, 4:repeat, 5:revert, 6:window,
;;           7-8:macro, 9-10:mode, 11:outline, 12:template
;;------------------------------------------------------------------------------
;; S
;; Default
;;            SPC         HOME END PGUP PGDN
;; Custom
;;                TAB RET
;; Available
;;                                           BKSP DEL ESC
;;------------------------------------------------------------------------------
;; C
;; Windows                                             ~
;;                                                    ESC
;; Special      c     i   m          x         [
;; Default    ab defgh  kl nopqrst v x  0..9 -        /  @        _
;;                        HOME END PGUP PGDN BKSP DEL
;; Custom              j          u w yz        ];'\,.  ! #$%   ()    |: <>?
;;            SPC TAB RET
;; Available                                `                ^&*   +{}  "
;;------------------------------------------------------------------------------
;; M
;; Special                                     [
;;                TAB                                 ESC
;; Default                           x
;;            SPC         HOME END PGUP PGDN BKSP DEL
;; Custom     abcdefghijklmnopqrstuvw yz0..9`-= ];'\,./
;;                    RET
;; Available
;;------------------------------------------------------------------------------
;; C-M
;; Special                m
;; Default     b   f
;; Windows
;;                                                DEL
;; KDE           d      k
;; Emacs      a   e g  j l n p rs      z05   -=     ,.
;;            SPC     RET      END PGUP PGDN
;; Available    c    hi     o q  tuvwxy 1..9`
;;                TAB     HOME               BKSP     ESC
;;------------------------------------------------------------------------------
;; C-c
;; Custom     abcdefghijklmnopqrstuv xyz0    -=  ;' ,./
;;            SPC TAB
;; Available                        w   1..9`  []  \
;;                    RET HOME END PGUP PGDN BKSP DEL ESC
;;------------------------------------------------------------------------------
;; C-h
;; Default    ab d f  i k m  p rs  v
;; Custom     A c     I            V
;; Available      e gh j l no q  tu wxyz0..9`-=[]\;',./
;;            SPC TAB RET HOME END PGUP PGDN BKSP DEL ESC
;;------------------------------------------------------------------------------
;; C-x
;; Don't use  a                 s    x
;; Default     b d      k  no  r   vw   0..9       \
;;                                           BKSP     ESC
;; Custom       c efghij lm  pq  tu   y      -=[ ;' ,./          )
;;            SPC TAB RET HOME
;; Available                           z    `   ]
;;                             END PGUP PGDN      DEL
;;------------------------------------------------------------------------------
;; C-x C
;; Don't use  a     gh     n p
;; Special                                     [       ~
;;                                                    ESC
;; Default     bc  f     l    q s u w        -=  ;
;; Custom        de   ijk m o  r t v xyz0..9      ' ,./
;;            SPC TAB RET                    BKSP
;; Available                            1..8`   ]  \
;;                        HOME END PGUP PGDN      DEL
;;------------------------------------------------------------------------------
;; C-x v
;; Similar
;; Special
;; Default          g    l        uv          =        ~
;; Custom                                                          +
;; Available  abcdef hijk mnopqrst  wxyz    `- [];'\,./ !@#$%^&*()_ {}|:"<>?
;;            SPC TAB RET HOME END PGUP PGDN BKSP DEL ESC
;;------------------------------------------------------------------------------
;;    1       2      3
;;    click   -      imenu
;; S  extend  -      -
;; C  buffers -      toolbar
;; M  browse  -      -
;;--------------
;; 4.1  Disable
;;--------------
(global-unset-key [f10]        ) ; menu-bar-open
(global-unset-key [S-delete]   ) ; kill-region
(global-unset-key [?\C-\\]     ) ; toggle-input-method
(global-unset-key [?\C-x ?\C-z]) ; iconify-or-deiconify-frame
(global-unset-key [?\M-\;]     ) ; comment-dwim
;;------------
;; 4.2  Mouse
;;------------
(setq mouse-drag-copy-region t)          ; copy selected region to clipboard
(setq mouse-wheel-progressive-speed nil) ; scroll at constant speed
(setq mouse-wheel-scroll-amount '(4 ((shift) . 1))) ; scroll 4, or 1 with shift
(global-set-key        [M-mouse-1] 'browse-url-at-mouse) ; mouse-start-secondary
(global-unset-key [M-down-mouse-1]) ; mouse-drag-secondary
(global-unset-key [M-drag-mouse-1]) ; mouse-set-secondary
(global-unset-key      [C-mouse-2]) ; facemenu-menu
(global-unset-key      [M-mouse-2]) ; mouse-yank-secondary
(global-unset-key        [mouse-3]) ; mouse-save-then-kill
(global-unset-key      [M-mouse-3]) ; mouse-secondary-save-then-kill
(global-set-key   [S-down-mouse-1] 'mouse-extend-region) ; mouse-set-font
(global-set-key   [S-drag-mouse-1] 'mouse-extend-region)
(global-set-key          [mouse-2] 'mouse-yank-at-click) ; mouse-yank-primary
(global-set-key          [mouse-3] 'ignore    ) ; mouse-secondary-save-then-kill
(global-set-key     [down-mouse-3] 'imenu              )
(global-set-key       [C-wheel-up] 'text-scale-increase) ; default Emacs >=27.1
(global-set-key     [C-wheel-down] 'text-scale-decrease) ; default Emacs >=27.1
(global-set-key        [C-mouse-4] 'text-scale-increase) ; C-wheel-up in Linux
(global-set-key        [C-mouse-5] 'text-scale-decrease) ; C-wheel-down in Linux
(global-set-key       [M-wheel-up] 'scroll-up-100      )
(global-set-key     [M-wheel-down] 'scroll-down-100    )
(global-set-key        [M-mouse-4] 'scroll-up-100      ) ; M-wheel-up in Linux
(global-set-key        [M-mouse-5] 'scroll-down-100    ) ; M-wheel-down in Linux
;;--------------
;; 4.3  Special
;;--------------
;; Within each: symbols, upper, lower, S, C, CS, M (left-right, up-down)
;; Use [?\t] for TAB for Putty terminal, where [tab]=<tab> is not accessible
(global-set-key [escape]       'delete-other-windows   ) ; Meta button
(global-set-key [?\C-x escape] 'repeat-complex-command ) ; [map]
(global-set-key [C-insert]     'copy-line-or-region    ) ; kill-ring-save
(global-set-key [f1]           'describe-char          )
(global-set-key [S-f1]         'describe-face          )
(global-set-key [f2]           'register-store-X       )
(global-set-key [S-f2]         'register-store-Y       )
(global-set-key [f3]           'register-jump-X        )
(global-set-key [S-f3]         'register-jump-Y        )
(global-set-key [f4]           'repeat                 )
(global-set-key [S-f4]         'tags-loop-continue  ) ; continue dired-do-search
(global-set-key [C-f4]         'kill-this-buffer       )
(global-set-key [M-f4]         'save-buffers-kill-emacs)
(global-set-key [f5]           'revert-buffer          )
(global-set-key [S-f5]         'arni-colors            )
(global-set-key [M-f5]         'dot-emacs-eval         )
(global-set-key [27 f5]        'dot-emacs-eval         ) ; M-f5 in terminal
(global-set-key [f6]           'other-window           )
(global-set-key [C-f6]         'bs-cycle-next          )
(global-set-key [C-S-f6]       'bs-cycle-previous      )
(global-set-key [f7]           'record-macro           )
(global-set-key [f8]           'kmacro-call-macro      )
(global-set-key [f11]          'outline-mode         ) ; toggle-frame-fullscreen
(global-set-key [C-f11]        'beginning-of-buffer    )
(global-set-key [S-f11]        'region-to-bol          )
(global-set-key [f12]          'move-end-of-line       ) ; f12=end on laptop
(global-set-key [C-f12]        'end-of-buffer          )
(global-set-key [S-f12]        'region-to-eol          ) ; S-f12=S-end on laptop
(global-set-key [?\C-x f12]    'goto-longest-line      ) ; end-of-defun
(global-set-key [?\C-x delete] 'blank-region           )
(global-set-key [C-backspace]  'backward-delete-word   ) ; backward-kill-word
(global-set-key [M-backspace]  'backward-delete-word   ) ; backward-kill-word
(global-set-key [?\C-x backspace] 'back-to-indentation) ; backward-kill-sentence
(global-set-key [?\C-x C-backspace] 'back-to-indentation)
(global-set-key [C-S-backspace] 'kill-whole-line-stay  ) ; kill-whole-line
(global-set-key [27 127]       'backward-delete-word   ) ; M-BKSP in terminal
(global-set-key [C-delete]     'delete-word            ) ; kill-line
(global-set-key [M-delete]     'kill-buffer-maybe-window) ; backward-kill-word
(global-set-key [?\C-x home]   'set-mark-command       )
(global-set-key [select]       'end-of-line            ) ; [end] in Putty -nw
(global-set-key [C-M-next]     'scroll-both-down-page  )
(global-set-key [C-M-prior]    'scroll-both-up-page    )
;; Use \t=TAB, not [tab]
(global-set-key [?\t]          'indent-or-complete    ) ; indent-for-tab-command
(global-set-key [?\C-x ?\t]    'indent-relative        ) ; indent-rigidly
(global-set-key [backtab]      'unindent-line-or-region) ; S-tab in Win and Linx
(global-set-key [C-tab]        'indent-buffer          )
(global-set-key [C-S-tab]      'unindent-buffer        )
(global-set-key [C-S-iso-lefttab] 'unindent-buffer     ) ; linux C-S-tab
(global-set-key [S-return]     'comment-indent-new-line)
(global-set-key [C-return]     'recentf-open-files     )
(global-set-key [C-M-return]   'browse-url-at-point    )
(global-set-key [?\C-x C-return] 'describe-current-coding-full)
(global-set-key [C-enter]      'recentf-open-files     )
(global-set-key [?\C- ]        'dabbrev-expand         ) ; set-mark-command
(global-set-key [?\C-@]        'dabbrev-expand         ) ; C-SPC in terminal
(global-set-key [?\M- ]        'cycle-spacing          ) ; just-one-space
(global-set-key [?\C-\M- ]     'recentf-open-files     ) ; mark-sexp
(global-set-key [?\C-\M-@]     'recentf-open-files     ) ; C-M-SPC in terminal
(global-set-key [?\C-c tab]    'tab-mode               )
(global-set-key [?\C-c ? ]     'hl-line-mode          ) ; highlight current line
(global-set-key [?\C-x ? ]     'blank-to-paren         )
(global-set-key [?\C-x ?\C- ]  'delete-horizontal-space) ; pop-global-mark
(global-set-key [C-up]         'previous-line          ) ; backward-paragraph
(global-set-key [C-down]       'next-line              ) ; forward-paragraph
(global-set-key [C-S-up]       'pull-line-or-region-up ) ; backward-paragraph
(global-set-key [C-S-down]     'pull-line-or-region-down) ; forward-paragraph
(global-set-key [M-left]       'split-window-top-bottom) ; backward-word
(global-set-key [M-right]      'split-window-left-right) ; forward-word
(global-set-key [M-up]         'scroll-other-up        )
(global-set-key [M-down]       'scroll-other-down      )
(global-set-key [?\C-c left]   'uncomment-line-or-region)
(global-set-key [?\C-c right]  'comment-line-or-region )
(global-set-key [?\C-x left]   'split-window-top-bottom) ; previous-buffer
(global-set-key [?\C-x right]  'split-window-left-right) ; next-buffer
(global-set-key [?\C-x S-up]   'uncomment-then-up      )
(global-set-key [?\C-x S-down] 'uncomment-then-down    )
(global-set-key [?\C-x C-up]   'comment-then-up        )
(global-set-key [?\C-x C-down] 'comment-then-down      )
;;--------------------
;; 4.4  Three strokes
;;--------------------
(global-set-key [?\C-x ?r ? ]    'clear-rectangle       ) ; point-to-register
(global-set-key [?\C-x ?r ?c]    'copy-rectangle-as-kill) ; clear-rectangle
(global-set-key [?\C-x ?r ?l]    'recentf-load-list     ) ; bookmark-bmenu-list
(global-set-key [?\C-x ?r ?m]    'rectangle-mark-mode   ) ; bookmark-set
(global-set-key [?\C-x ?r ?s]    'recentf-save-list     ) ; copy-to-register
(global-set-key [?\C-x ?v ?+]    'vc-diff-select        ) ; vc-update
(global-set-key [?\C-x ?v ?=]    'vc-ediff              ) ; vc-diff
(global-set-key [?\C-x ?v ?\M-=] 'vc-diff               )
;;------------------
;; 4.5  Two strokes
;;------------------
;; C-c, C-h, C-x
(global-set-key [?\C-c ?0]    'fundamental-mode         )
(global-set-key [?\C-c ?-]    'highlight-changes-mode   )
(global-set-key [?\C-c ?=]    'diff-this-buffer-with-file)
(global-set-key [?\C-c ?\]]   'show-long-lines          )
(global-set-key [?\C-c ?\;]   'csv-mode-force           )
(global-set-key [?\C-c ?']    'space-mode               )
(global-set-key [?\C-c ?,]    'uncomment-line-or-region )
(global-set-key [?\C-c ?.]    'comment-line-or-region   )
(global-set-key [?\C-c ?/]    'which-function-mode      )
(global-set-key [?\C-c ?*]    'change-log-mode          )
(global-set-key [?\C-c ?a]    'admb-mode                )
(global-set-key [?\C-c ?b]    'tmb-mode                 )
(global-set-key [?\C-c ?c]    'c++-mode                 )
(global-set-key [?\C-c ?d]    'bat-mode                 )
(global-set-key [?\C-c ?e]    'emacs-lisp-mode          )
(global-set-key [?\C-c ?f]    'f90-mode                 )
(global-set-key [?\C-c ?g]    'gadget-mode              )
(global-set-key [?\C-c ?h]    'html-helper-mode         )
(global-set-key [?\C-c ?i]    'texinfo-mode             )
(global-set-key [?\C-c ?j]    'java-mode                )
(global-set-key [?\C-c ?k]    'markdown-mode            )
(global-set-key [?\C-c ?l]    'TeX-latex-mode           )
(global-set-key [?\C-c ?m]    'makefile-mode            )
(global-set-key [?\C-c ?n]    'display-line-numbers-mode) ; emacs 25: linum-mode
(global-set-key [?\C-c ?o]    'org-mode                 )
(global-set-key [?\C-c ?p]    'ps-mode                  )
(global-set-key [?\C-c ?q]    'sql-mode                 )
(global-set-key [?\C-c ?r]    'R-mode                   )
(global-set-key [?\C-c ?s]    'sh-mode                  )
(global-set-key [?\C-c ?t]    'text-mode                )
(global-set-key [?\C-c ?u]    'conf-mode                )
(global-set-key [?\C-c ?v]    'visual-basic-mode        )
(global-set-key [?\C-c ?x]    'hexl-mode                )
(global-set-key [?\C-c ?y]    'doxymacs-mode-with-hook  )
(global-set-key [?\C-c ?z]    'mail-mode                )
(global-set-key [?\C-c ?\C-x] 'ignore                   )
(global-set-key [?\C-h ?A]    'apropos                  )
(global-set-key [?\C-h ?I]    'Info-on-current-buffer   )
(global-set-key [?\C-h ?V]    'apropos-variable         )
(global-set-key [?\C-h ?c]    'list-colors-fullscreen   ) ; describe-key-briefly
(global-set-key [?\C-h ?l]    'apropos-library          ) ; view-lossage
(global-set-key [?\C-x ?4]    'maximize-window-top      ) ; [map]
(global-set-key [?\C-x ?$]    'insert-euro             ) ; set-selective-display
(global-set-key [?\C-x ?-]    'insert-en-dash) ; shrink-window-if-larger-than...
(global-set-key [?\C-x ?=]    'ediff-current-file       ) ; what-cursor-position
(global-set-key [?\C-x ?+]    'split-window-grid        ) ; balance-windows
(global-set-key [?\C-x ?\[]   'git-log-clean            ) ; backward-page
(global-set-key [?\C-x ?\\]   'transpose-windows        )
(global-set-key [?\C-x ?']    'region-forward-line      ) ; expand-abbrev
(global-set-key [?\C-x ?\;]   'set-fill-column          ) ; comment-set-column
(global-set-key [?\C-x ?,]    'backward-word            )
(global-set-key [?\C-x ?.]    'forward-word             ) ; set-fill-prefix
(global-set-key [?\C-x ?/]    'mark-paragraph           )
(global-set-key [?\C-x ?J]    'region-backward-paragraph)
(global-set-key [?\C-x ?N]    'new-buffer               )
(global-set-key [?\C-x ?T]    'titlecase-dwim           )
(global-set-key [?\C-x ?c]    'save-buffers-kill-emacs  )
(global-set-key [?\C-x ?e]    'region-to-line      ) ; kmacro-end-and-call-macro
(global-set-key [?\C-x ?f]    'find-file-literally      ) ; set-fill-column
(global-set-key [?\C-x ?g]    'magit-status             )
(global-set-key [?\C-x ?h]    'copy-buffer              ) ; mark-whole-buffer
(global-set-key [?\C-x ?i]    'insert-date              ) ; insert-file
(global-set-key [?\C-x ?j]    'region-forward-paragraph )
(global-set-key [?\C-x ?l]    'visual-line-mode         ) ; count-lines-page
(global-set-key [?\C-x ?m]    'mark-function            ) ; compose-mail
(global-set-key [?\C-x ?n]    'narrow-to-defun-or-region) ; [map]
(global-set-key [?\C-x ?p]    'list-packages            )
(global-set-key [?\C-x ?q]    'toggle-red-special       ) ; kbd-macro-query
(global-set-key [?\C-x ?t]    'transpose-words          )
(global-set-key [?\C-x ?u]    'google-decode-url        ) ; undo
(global-set-key [?\C-x ?w]    'widen                    )
(global-set-key [?\C-x ?y]    'yank-pop                 )
(global-set-key [?\C-x ?\C-9] 'beginning-of-defun       )
(global-set-key [?\C-x ?\C-0] 'end-of-defun             )
(global-set-key [?\C-x ?\C-_] 'tabify-spaces-copy-buffer) ; linux C-x C-/
(global-set-key [?\C-x ?\C-]] 'region-backward-line     )
(global-set-key [?\C-x ?\C-'] 'region-forward-line      )
(global-set-key [?\C-x ?\C-\\] 'convert-special         )
(global-set-key [?\C-x ?\C-,] 'tab-to-csv            ) ; or region-backward-char
(global-set-key [?\C-x ?\C-.] 'region-forward-char      )
(global-set-key [?\C-x ?\C-/] 'tabify-spaces-copy-buffer)
(global-set-key [?\C-x ?\C-b] 'ibuffer                  ) ; list-buffers
(global-set-key [?\C-x ?\C-e] 'region-to-eol            ) ; eval-last-sexp
(global-set-key [?\C-x ?\C-j] 'fill-region-or-buffer    )
(global-set-key [?\C-x ?\C-k] 'kill-this-buffer         ) ; [map]
(global-set-key [?\C-x ?\C-l] 'downcase-word-or-region  ) ; downcase-region
(global-set-key [?\C-x ?\C-m] 'describe-current-coding-brief) ; [map]
(global-set-key [?\C-x ?\C-n] 'forward-list             ) ; set-goal-column
(global-set-key [?\C-x ?\C-o] 'find-file-other-window   ) ; delete-blank-lines
(global-set-key [?\C-x ?\C-p] 'backward-list            ) ; mark-page
(global-set-key [?\C-x ?\C-r] 'reverse-region           ) ; find-file-read-only
(global-set-key [?\C-x ?\C-t] 'transpose-sexps          ) ; transpose-lines
(global-set-key [?\C-x ?\C-u] 'upcase-word-or-region    ) ; upcase-region
(global-set-key [?\C-x ?\C-v] 'end-of-buffer            ) ; find-alternate-file
(global-set-key [?\C-x ?\C-x] 'eval-expression       ) ; exchange-point-and-mark
(global-set-key [?\C-x ?\C-y] 'beginning-of-buffer      )
(global-set-key [?\C-x ?\C-z] 'indent-buffer            ) ; for terminal
(global-set-key [?\C-x ?\M-j] 'join-region-or-buffer    )
(global-set-key [?\C-x ?\M-m] 'toggle-latin-1-coding    )
(global-set-key [?\C-x ?\M-s] 'highlight-and-count-string)
;;-----------------
;; 4.6  One stroke
;;-----------------
;; C, CM, M, plain
(global-set-key [?\C-`]     'display-fill-column-indicator-mode)
(global-set-key [?\C-!]     'shell-command           )
(global-set-key [?\C-#]     'toggle-comments         )
(global-set-key [?\C-$]     'count-words             )
(global-set-key [?\C-%]     'read-only-mode          )
(global-set-key [?\C-&]     'async-shell-command     )
(global-set-key [?\C-\(]    'backward-sexp-start     )
(global-set-key [?\C-\)]    'forward-sexp-start      )
(global-set-key [?\C-=]     'duplicate-dwim          )
(global-set-key [?\C-\]]    'scroll-up-1             ) ; abort-recursive-edit
(global-set-key [?\C-{]     'backward-paragraph      )
(global-set-key [?\C-}]     'forward-paragraph       )
(global-set-key [?\C-\\]    'windows-path            ) ; toggle-input-method
(global-set-key [?\C-|]     'align                   )
(global-set-key [?\C-\;]    'region-backward-word    )
(global-set-key [?\C-:]     'region-forward-word     )
(global-set-key [?\C-']     'scroll-down-1           )
(global-set-key [?\C-,]     'backward-word           )
(global-set-key [?\C-<]     'region-backward-word    )
(global-set-key [?\C-.]     'forward-word            )
(global-set-key [?\C->]     'region-forward-word     )
(global-set-key [67108927]  'count-everything        ) ; C-?
(global-set-key [?\C-j]     'fill-paragraph-forward  ) ; newline
(global-set-key [?\C-k]     'kill-line-or-region     ) ; kill-line
(global-set-key [?\C-u]     'man                     ) ; universal-argument
(global-set-key [?\C-w]     'toggle-trailing-whitespace) ; kill-region
(global-set-key [?\C-y]     'scroll-down-command     ) ; yank
(global-set-key [?\C-z]     'undo                 ) ; iconify-or-deiconify-frame
(global-set-key [?\C-Þ]     'count-everything        )
(global-set-key [67109086]  'count-everything        ) ; C-Þ
(global-set-key [67111134]  'count-everything        ) ; C-Þ, was [331966]
(global-set-key [67109118]  'undo                    ) ; C-þ
(global-set-key [67111166]  'undo                    ) ; C-þ
(global-set-key [C-M-end]   'goto-longest-line       ) ; end-of-defun
(global-set-key [?\C-\M-5]  'read-only-mode          ) ; digit-argument
(global-set-key [?\C-\M-0]  'text-scale-adjust       ) ; digit-argument
(global-set-key [?\C-\M-_]  'unindent-buffer         ) ; for terminal
(global-set-key [?\C-\M--]  'text-scale-decrease     ) ; negative-argument
(global-set-key [?\C-\M-=]  'text-scale-increase     )
(global-set-key [?\C-\M-\]] 'scroll-both-up          )
(global-set-key [?\C-\M-\\] 'align-regexp            ) ; indent-region
(global-set-key [?\C-\M-']  'scroll-both-down        )
(global-set-key [?\C-\M-,]  'backward-paragraph      )
(global-set-key [?\C-\M-.]  'forward-paragraph       ) ; find-tag-regexp
(global-set-key [?\C-\M-a]  'goto-non-ascii          ) ; beginning-of-defun
(global-set-key [?\C-\M-e]  'query-replace-regexp    ) ; end-of-defun
(global-set-key [?\C-\M-g]  'goto-char               )
(global-set-key [?\C-\M-h]  'backward-delete-word    ) ; M-backspace in terminal
(global-set-key [?\C-\M-j]  'join-line-nospace       ) ; indent-new-comment-line
(global-set-key [?\C-\M-l]  'longlines-mode          ) ; reposition-window
(global-set-key [?\C-\M-n]  'pull-line-or-region-down) ; forward-list
(global-set-key [?\C-\M-p]  'pull-line-or-region-up  ) ; backward-list
(global-set-key [?\C-\M-z]  'zap-back-to-char        )
(if window-system                              ; keep digit-argument in terminal
    (progn (global-set-key [?\M-1] 'font-1           ) ; digit-argument
           (global-set-key [?\M-2] 'font-2           ) ; digit-argument
           (global-set-key [?\M-3] 'font-3           ) ; digit-argument
           (global-set-key [?\M-4] 'font-4           ) ; digit-argument
           (global-set-key [?\M-5] 'font-5           ) ; digit-argument
           (global-set-key [?\M-6] 'font-6           ) ; digit-argument
           (global-set-key [?\M-7] 'font-7           ) ; digit-argument
           (global-set-key [?\M-8] 'font-8           ) ; digit-argument
           (global-set-key [?\M-9] 'font-9           ) ; digit-argument
           (global-set-key [?\M-0] 'font-0         ))) ; digit-argument
(global-set-key [?\M-%]     'read-only-mode          ) ; query-replace
(global-set-key [?\M-=]     'duplicate-dwim-comment  ) ; count-line-region
(global-set-key [?\M-+]     'transpose-windows       )
;; (global-set-key [?\M-\[] 'ignore                  ) ; paste into Emacs -nw
(global-set-key [?\M-\]]    'scroll-up-1             )
(global-set-key [?\M-{]     'backward-paragraph      ) ; backward-paragraph
(global-set-key [?\M-}]     'forward-paragraph       ) ; forward-paragraph
(global-set-key [?\M-']     'transpose-windows       ) ; delete-horizontal-space
(global-set-key [?\M-\\]    'set-tab-width           ) ; abbrev-prefix-mark
(global-set-key [?\M-,]     'memo                    ) ; tags-loop-continue
(global-set-key [?\M-.]     'dot-emacs-edit          ) ; find-tag
(global-set-key [?\M-/]     'delete-comments         ) ; dabbrev-expand
(global-set-key [?\M-a]     'mark-buffer             ) ; backward-sentence
(if window-system
    (global-set-key [?\M-b] 're-builder             )) ; backward-word
(global-set-key [?\M-c]     'copy-line-or-region     ) ; capitalize-word
(global-set-key [?\M-d]     'delete-word             ) ; kill-word
(global-set-key [?\M-e]     'query-replace           ) ; forward-sentence
(if window-system
    (global-set-key [?\M-f] 'occur                  )) ; forward-word
(global-set-key [?\M-g]     'goto-line               ) ; [map]
(global-set-key [?\M-h]     'font-lock-mode          ) ; mark-paragraph
(global-set-key [?\M-i]     'overwrite-mode          ) ; tab-to-tab-stop
(global-set-key [?\M-j]     'delete-indentation      ) ; indent-new-comment-line
(global-set-key [?\M-k]     'kill-this-buffer        ) ; kill-sentence
(global-set-key [?\M-l]     'delete-trailing-spc-tab-m) ; downcase-word
(global-set-key [?\M-m]     'toggle-utf-8-coding     ) ; back-to-indentation
(global-set-key [?\M-n]     'bs-cycle-next           )
(global-set-key [?\M-o]     'other-window            ) ; facemenu-set-face
(global-set-key [?\M-p]     'bs-cycle-previous       )
(global-set-key [?\M-q]     'sql-oracle-default      ) ; fill-paragraph
(global-set-key [?\M-r]     'Rni                     ) ; move-to-window-line
(global-set-key [?\M-s]     'highlight-and-count-regexp)
(global-set-key [?\M-t]     'sort-lines              ) ; transpose-words
(global-set-key [?\M-u]     'untabify-buffer         ) ; uppercase-word
(global-set-key [?\M-v]     'yank-quiet              ) ; scroll-down
(global-set-key [?\M-w]     'auto-fill-mode          ) ; kill-ring-save
(global-set-key [?\M-y]     'delete-all-blank-lines  ) ; yank-pop
(global-set-key [?\M-z]     'zap-up-to-char          ) ; zap-to-char
;;-----------------
;; 4.7  Minibuffer
;;-----------------
(define-key minibuffer-local-map
            [C-up]   'previous-history-element) ; previous-line
(define-key minibuffer-local-map
            [C-down] 'next-history-element    ) ; next-line
(define-key minibuffer-local-map
            [?\C-n]  'next-history-element    ) ; next-line
(define-key minibuffer-local-map
            [?\C-p]  'previous-history-element) ; previous-line
(define-key minibuffer-local-completion-map [?\M-v]  'yank) ; reactivate yank
;;==============================================================================
;;
;; 5  FUNCTIONS
;;
;;==============================================================================
;; File - Edit - View - Insert - Format - Tools - Window - Help
;; new    copy   narrow lorem    indent   comment max      ascii
;; close  region        utf8     fill     count   split    colors
;; print  goto          seq      tabs     sort
;;-----------
;; 5.1  File
;;-----------
(defun byte-dir-this-really-works (dir)
  "Compile all `.el' files in DIR and subdirs, except where `.elc' is current."
  (interactive "DByte compile directory: ")
  (byte-recompile-directory dir 0))
(defalias 'cp 'copy-file)
(defalias 'del 'delete-file)
(defalias 'dir 'list-directory)
(defalias 'htmlize 'htmlfontify-buffer)
(defun kill-this-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(defun kill-buffer-maybe-window ()
  "Kill current buffer and window.
Doesn't complain about last window, unlike `kill-buffer-and-window`."
  (interactive)
  (kill-this-buffer)
  (if (> (length (window-list)) 1)
      (delete-window)))
(defun kill-process-now ()
  "Stop last active process."
  (interactive)
  (if (> (length (process-list)) 0) ; any active processes?
      (let ((active-process-buffer ; last process for GDB
             (buffer-name (process-buffer (car (last (process-list)))))))
        (kill-process (car (last (process-list)))))))
(defun kill-unmodified-buffers ()
  "Close all buffers that have not been modified."
  (interactive)
  (dolist (B (buffer-list))
    (kill-buffer-if-not-modified B)))
(defun latin-1-revert ()
  "Reload file assuming latin-1 encoding."
  (interactive)
  (revert-buffer-with-coding-system 'iso-latin-1))
(defun latin-1-dos ()
  "Reload file assuming latin-1 encoding."
  (interactive)
  (revert-buffer-with-coding-system 'iso-latin-1-dos))
(defun latin-1-unix ()
  "Reload file assuming latin-1 encoding."
  (interactive)
  (revert-buffer-with-coding-system 'iso-latin-1-unix))
(defun ll (dir)
  "List files in DIR."
  (interactive "DList directory (verbose): ")
  (list-directory dir t))
(defalias 'ls 'list-directory)
(defun memo ()
  "Open ~/emacs/.memo."
  (interactive)
  (find-file "~/emacs/.memo"))
(defalias 'mv 'rename-file)
(defun new-buffer ()
  "Create new buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer "Untitled"))
  (eval (list (default-value 'major-mode))))
(defalias 'pdf-print 'print-pdf)
(defun print-pdf ()
  "Print buffer to out.ps and out.pdf. See `ps-outfile'."
  (interactive)
  (let ((pdf-outfile (concat (file-name-sans-extension ps-outfile) ".pdf")))
    (setq ps-landscape-mode nil     ) ; portrait
    ;; [Courier]-[Helvetica,Helvetica-Narrow]-[Times,Palatino,NewCenturySchbk]
    (setq ps-font-family 'Courier   )
    ;; 9 and 6.5 font size for 120 columns, landscape and portrait
    (setq ps-font-size '(9 . 10)    )
    (setq ps-paper-type 'a4         ) ; paper size (a4 or letter)
    (setq ps-print-header nil       ) ; no header (incl. page number)
    (setq ps-number-of-columns 1    ) ; no columns
    (setq ps-line-number nil        ) ; no line numbers
    (setq ps-line-spacing 0         ) ; no line spacing
    (setq ps-paragraph-spacing 0    ) ; no paragraph spacing
    (setq ps-use-face-background nil) ; no background color
    (setq ps-left-margin 60)
    (setq ps-right-margin 60)
    (setq ps-top-margin 60)
    (setq ps-bottom-margin 60)
    (ps-print-buffer-with-faces ps-outfile)
    ;; ps2pdf in Windows uses -arg#val instead of -arg=val, Linux either
    (shell-command (concat "ps2pdf -sPAPERSIZE#" (prin1-to-string ps-paper-type)
                           " " ps-outfile " " pdf-outfile))
    (message "Distilled %s" pdf-outfile)))
(defun read-file-to-list (file)
  "Read text from FILE into list of strings."
  (interactive "fRead file: ")
  (with-temp-buffer (insert-file-contents file)
                    (split-string (buffer-string) "\n" t)))
(defun read-file-to-string (file)
  "Read text from FILE into string."
  (interactive "fRead file: ")
  (with-temp-buffer (insert-file-contents file)(buffer-string)))
(defalias 'rm 'delete-file)
(defalias 'rmdir 'delete-directory)
(defun toggle-backup-files ()
  "Toggle backup settings for current buffer."
  (interactive)
  (setq make-backup-files (not make-backup-files))
  (message "Backup %s" (if make-backup-files "ON" "OFF")))
(defun toggle-latin-1-coding ()
  "Toggle between `latin-1-unix' and `latin-1-dos' encoding."
  (interactive)
  (if (string-match "unix" (prin1-to-string buffer-file-coding-system))
      (set-buffer-file-coding-system 'iso-latin-1-dos t)
    (set-buffer-file-coding-system 'iso-latin-1-unix t)))
(defun toggle-utf-8-coding ()
  "Toggle between `utf-8-unix' and `utf-8-dos' encoding."
  (interactive)
  (if (string-match "unix" (prin1-to-string buffer-file-coding-system))
      (set-buffer-file-coding-system 'utf-8-dos t)
    (set-buffer-file-coding-system 'utf-8-unix t)))
(defun utf-8-revert ()
  "Reload file assuming utf-8 encoding."
  (interactive)
  (revert-buffer-with-coding-system 'utf-8))
(defun utf-8-dos ()
  "Use `utf-8-dos' encoding."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos t))
(defun utf-8-unix ()
  "Use `utf-8-unix' encoding."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t))
;;-----------
;; 5.2  Edit
;;-----------
(defun backward-delete-word (&optional n)
  "Delete previous N words."
  (interactive "*p")
  (delete-word (- n)))
(defun backward-sexp-start ()
  "Move to previous expression."
  (interactive)
  (backward-sexp))
(defun copy-buffer ()
  "Copy buffer without moving cursor."
  (interactive)
  (kill-new (buffer-substring-no-properties (point-min)(point-max)))
  (message "Copied buffer"))
(defun copy-line-or-region ()
  "Copy region if selected, otherwise copy line."
  (interactive)
  (if (use-region-p)
      (progn (kill-new (buffer-substring-no-properties (point)(mark)))
             (deactivate-mark))
    (if (< (line-beginning-position)(line-end-position))
        (progn (kill-new (buffer-substring-no-properties
                          (line-beginning-position)
                          (min (+ (line-end-position) 1)(point-max))))
               (message "Copied line")))))
(defun delete-word (&optional n)
  "Delete following N words."
  (interactive "*p")
  (delete-region (point)(save-excursion (forward-word n)(point))))
(defun duplicate-dwim-comment ()
  "Duplicate line, commenting out the original one."
  (interactive "*")
  (let ((duplicate-line-final-position 0))
    (duplicate-dwim))
  (comment-line 1)
  (uncomment-region (line-beginning-position)(line-end-position)))
(defun forward-sexp-start ()
  "Move to next expression."
  (interactive)
  (forward-sexp 2)
  (backward-sexp))
(defalias 'goto-column 'move-to-column)
(defun goto-line-lisp (line &optional buffer)
  "Go to LINE. Use in Lisp programs instead of `goto-line'."
  (interactive "nGoto line: ")
  (if (not (null buffer))
      (with-current-buffer buffer
        (goto-char (point-min))
        (beginning-of-line line))
    (goto-char (point-min))
    (beginning-of-line line)))
(defun goto-longest-line ()
  "Go to longest line."
  (interactive)
  (let ((line 1)
        (length 0))
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (setq length (current-column))
      (while (not (eobp))
        (progn (end-of-line 2)
               (if (> (current-column) length)
                   (progn (setq line (line-number-at-pos))
                          (setq length (current-column)))))))
    (goto-line-lisp line)
    (message "Line %d is %d characters" line length)))
(defun highlight-and-count-regexp (regexp)
  "Highlight and count REGEXP occurrences (case-sensitive).
See also `highlight-and-count-string'."
  (interactive "sRegexp (case-sensitive) to highlight: ")(require 'hi-lock)
  (let ((case-fold-search nil))
    (set-face-attribute 'hi-pink     nil :background "brown1"    )
    (set-face-attribute 'hi-blue     nil :background "dodgerblue")
    (set-face-attribute 'hi-black-b  nil :background "black"
                        :foreground "white" :weight -            )
    (set-face-attribute 'hi-blue-b   nil :background "blue2"
                        :foreground "white" :weight -            )
    (set-face-attribute 'hi-red-b    nil :background "red"
                        :foreground "white" :weight -            )
    (set-face-attribute 'hi-green-b  nil :background "green3"
                        :foreground "white" :weight -            )
    (set-face-attribute 'hi-black-hb nil :background "gray50"
                        :foreground "white" :weight - :height - :inherit -)
    (if (string-equal regexp "")
        (progn (hi-lock-mode 0)
               (setq hi-lock-face-defaults
                     '("hi-yellow" "hi-pink" "hi-green" "hi-blue" "hi-black-b"
                       "hi-blue-b" "hi-red-b" "hi-green-b" "hi-black-hb")))
      (progn (hi-lock-face-buffer regexp (car hi-lock-face-defaults))
             (setq hi-lock-face-defaults
                   (append (cdr hi-lock-face-defaults)
                           (list (car hi-lock-face-defaults))))
             (message "%d occurrences"
                      (how-many regexp (point-min)(point-max)))))))
(defun highlight-and-count-string (string)
  "Highlight and count STRING occurrences (case-insensitive).
See also `highlight-and-count-regexp'."
  (interactive "sString (case-insensitive) to highlight: ")
  (highlight-and-count-regexp (case-fold-string string)))
(defun highlight-long-lines (&optional n)
  "Highlight lines longer than N (default 80)."
  (interactive "p")
  (if (= n 1)(setq n 80))
  (let ((regexp (concat "^.\\{" (number-to-string (+ n 1)) ",\\}.*")))
    (hi-lock-mode 0)
    (hi-lock-face-buffer regexp)
    (message "%d lines wider than %d columns"
             (how-many regexp (point-min)(point-max)) n)))
(defun jump-middle ()
  "Go to middle of page."
  (interactive)
  (deactivate-mark)
  (goto-char (point-min))
  (forward-line (middle-from-here)))
(defun kill-line-or-region (&optional n)
  "Kill region if selected, otherwise kill N lines."
  (interactive "*p")
  (if (use-region-p)
      (kill-region (point)(mark))
    (if (= n 1)(kill-line)(kill-line n))))
(defun kill-whole-line-stay (&optional n)
  "Kill N whole lines and stay in column."
  (interactive "*p")
  (let ((col (current-column)))
    (kill-whole-line n)
    (move-to-column col)))
(defun mark-buffer ()
  "Mark whole buffer."
  (interactive)
  (push-mark (point))
  (push-mark (point-max) nil t)
  (goto-char (point-min))
  (region-set))
(defun mark-function ()
  "Mark function."
  (interactive)
  (mark-defun)
  (region-set))
(defun mark-quit ()
  "Inactivate mark, so motion commands do not extend region."
  (interactive)
  (setq mark-active nil)
  (message "Quit"))
(defun mouse-extend-region (event)
  "Extend region to mouse position."
  (interactive "e")
  (if (use-region-p)
      (mouse-set-point event)
    (mouse-save-then-kill event)))
(defalias 'occur-multi 'multi-occur)
(defun pos-at-beginning-of-line (&optional n)
  "Return the position at beginning of line N.
See also `line-beginning-position'."
  (save-excursion
    (goto-char (point-min))
    (line-beginning-position n)))
(defun pos-at-end-of-line (&optional n)
  "Return the position at end of line N.
See also `line-end-position'."
  (save-excursion
    (goto-char (point-min))
    (line-end-position n)))
(defun pull-line-down (&optional n)
  "Pull line down N lines."
  (let ((auto-fill-function nil))
    (drag-stuff-line-down n)))
(defun pull-line-up (&optional n)
  "Pull line up N lines."
  (let ((auto-fill-function nil))
    (drag-stuff-line-up (- n))))
(defun pull-line-or-region-down (&optional n)
  "Pull line or region down N lines."
  (interactive "*p")
  (require 'drag-stuff)
  (if (use-region-p)
      (pull-region-down n)
    (pull-line-down n)))
(defun pull-line-or-region-up (&optional n)
  "Pull line or region up N lines."
  (interactive "*p")
  (require 'drag-stuff)
  (if (use-region-p)
      (pull-region-up n)
    (pull-line-up n)))
(defun pull-region-down (&optional n)
  "Pull region down N lines."
  (let ((auto-fill-function nil)
        (point-first (< (point)(mark)))
        (shrink (= (region-end)
                   (pos-at-beginning-of-line
                    (line-number-at-pos (region-end))))))
    (if shrink
        (progn (if point-first (exchange-point-and-mark))
               (backward-char)))
    (drag-stuff-down n)
    (if shrink
        (progn (forward-char)
               (if point-first (exchange-point-and-mark))))))
(defun pull-region-up (&optional n)
  "Pull region up N lines."
  (let ((auto-fill-function nil)
        (point-first (< (point)(mark)))
        (shrink (= (region-end)
                   (pos-at-beginning-of-line
                    (line-number-at-pos (region-end))))))
    (if shrink
        (progn (if point-first (exchange-point-and-mark))
               (backward-char)))
    (drag-stuff-up n)
    (if shrink
        (progn (forward-char)
               (if point-first (exchange-point-and-mark))))))
(defun region-backward-char (&optional n)
  "Extend region backward N characters."
  (interactive "p")
  (if (not (use-region-p))(push-mark))
  (backward-char n)
  (region-set))
(defun region-backward-line (&optional n)
  "Extend region backward N lines."
  (interactive "p")
  (if (not (use-region-p))(push-mark))
  (forward-line (- n)) ; to handle goal-column (warn)
  (region-set))
(defun region-backward-paragraph (&optional n)
  "Extend region backward N paragraphs."
  (interactive "p")
  (if (not (use-region-p))(push-mark))
  (backward-paragraph n)
  (region-set))
(defun region-backward-word (&optional n)
  "Extend region backward N words."
  (interactive "p")
  (if (not (use-region-p))(push-mark))
  (backward-word n)
  (region-set))
(defun region-bol-bottom ()
  "Extend region from beginning of current line to bottom."
  (interactive)
  (if (not (use-region-p))
      (push-mark (line-beginning-position)))
  (goto-char (point-max))
  (region-set))
(defun region-bol-down (&optional n)
  "Extend region from beginning of current line down N lines."
  (interactive "p")
  (if (not (use-region-p))
      (push-mark (line-beginning-position)))
  (beginning-of-line (+ 1 n))
  (region-set))
(defun region-bol-top (&optional n)
  "Extend region from beginning of current line to top, or line N."
  (interactive "p")
  (if (not (use-region-p))
      (push-mark (line-beginning-position)))
  (goto-line-lisp n)
  (region-set))
(defun region-bol-up (&optional n)
  "Extend region from beginning of current line up N lines."
  (interactive "p")
  (if (not (use-region-p))
      (push-mark (line-beginning-position)))
  (beginning-of-line (- 1 n))
  (region-set))
(defun region-forward-char (&optional n)
  "Extend region forward N characters."
  (interactive "p")
  (if (not (use-region-p))(push-mark))
  (forward-char n)
  (region-set))
(defun region-forward-line (&optional n)
  "Extend region forward N lines."
  (interactive "p")
  (if (not (use-region-p))(push-mark))
  (forward-line n) ; to handle goal-column (warning)
  (region-set))
(defun region-forward-paragraph (&optional n)
  "Extend region forward N paragraphs."
  (interactive "p")
  (if (not (use-region-p))(push-mark))
  (forward-paragraph n)
  (region-set))
(defun region-forward-word (&optional n)
  "Extend region forward N words."
  (interactive "p")
  (if (not (use-region-p))(push-mark))
  (forward-word n)
  (region-set))
(defun region-set ()
  "Make sure the region is active (yellow) and transient (will exit properly)."
  (interactive)
  (activate-mark)(setq transient-mark-mode (cons 'only transient-mark-mode)))
(defun region-to-bol ()
  "Extend region to beginning of current line."
  (interactive)
  (if (not (use-region-p))(push-mark))
  (goto-char (line-beginning-position))
  (region-set))
(defun region-to-eol ()
  "Extend region to end of current line."
  (interactive)
  (if (not (use-region-p))(push-mark))
  (goto-char (line-end-position))
  (region-set))
(defun region-to-line (&optional n)
  "Extend region to beginning of line N."
  (interactive "nExtend region to line: ")
  (if (not (use-region-p))(push-mark))
  (goto-line-lisp n)
  (region-set))
(defun register-jump-X ()
  "Return cursor to location stored in register X."
  (interactive)
  (jump-to-register ?X)
  (message "Jumped to stored location X"))
(defun register-jump-Y ()
  "Return cursor to location stored in register Y."
  (interactive)
  (jump-to-register ?Y)
  (message "Jumped to stored location Y"))
(defun register-store-X ()
  "Store current location of cursor in register X."
  (interactive)
  (point-to-register ?X)
  (message "Location X stored"))
(defun register-store-Y ()
  "Store current location of cursor in register Y."
  (interactive)
  (point-to-register ?Y)
  (message "Location Y stored"))
(defun scroll-both-down (&optional n)
  "Scroll both windows down N lines."
  (interactive "p")
  (scroll-up n)
  (scroll-other-window n))
(defun scroll-both-down-page (&optional n)
  "Scroll both windows down N pages."
  (interactive "p")
  (scroll-up (pages n))
  (scroll-other-window (pages n)))
(defun scroll-both-up (&optional n)
  "Scroll both windows up N lines."
  (interactive "p")
  (scroll-down n)
  (scroll-other-window-down n))
(defun scroll-both-up-page (&optional n)
  "Scroll both windows up N pages."
  (interactive "p")
  (scroll-down (pages n))
  (scroll-other-window-down (pages n)))
(defun scroll-down-1 ()
  "Scroll down one line."
  (interactive)
  (scroll-up 1))
(defun scroll-down-3 ()
  "Scroll down 3 lines."
  (interactive)
  (scroll-up 3))
(defun scroll-down-10 ()
  "Scroll down 10 lines."
  (interactive)
  (scroll-up 10))
(defun scroll-down-100 ()
  "Scroll down 100 lines."
  (interactive)
  (scroll-up 100))
(defun scroll-other-down (&optional n)
  "Scroll other window down N lines."
  (interactive "p")
  (scroll-other-window n))
(defun scroll-other-up (&optional n)
  "Scroll other window up N lines."
  (interactive "p")
  (scroll-other-window-down n))
(defun scroll-up-1 ()
  "Scroll one line up."
  (interactive)
  (scroll-down 1))
(defun scroll-up-3 ()
  "Scroll 3 lines up."
  (interactive)
  (scroll-down 3))
(defun scroll-up-10 ()
  "Scroll 10 lines up."
  (interactive)
  (scroll-down 10))
(defun scroll-up-100 ()
  "Scroll 100 lines up."
  (interactive)
  (scroll-down 100))
(defalias 'show-long-lines 'highlight-long-lines)
(defun yank-quiet (&optional arg)
  "Yank without showing the message \"Mark set\"."
  (interactive "*P")
  (if (use-region-p)
      (delete-region (point)(mark)))
  (yank arg)
  (message nil))
(defun zap-back-to-char (char)
  "Delete region back to, but not including, CHAR (case-sensitive)."
  (interactive "*cZap back to char: ")
  (let ((case-fold-search nil))
    (delete-region (point)
                   (progn (search-backward (string char))
                          (forward-char)
                          (point)))))
(defun zap-up-to-char (char)
  "Delete region up to, but not including, CHAR (case-sensitive)."
  (interactive "*cZap up to char: ")
  (let ((case-fold-search nil))
    (delete-region (point)
                   (progn (search-forward (string char))
                          (backward-char)
                          (point)))))
;;-----------
;; 5.3  View
;;-----------
(defvar default-comment-color (fg 'font-lock-comment-face)
  "Default comment color. See `toggle-comments'.")
(defvar default-escape-color (fg 'escape-glyph)
  "Default comment color. See `toggle-escape-glyphs'.")
(defvar green-cite nil
  "Non-nil if bibliographic citations are currently green.
See `toggle-green-cite'.")
(defvar red-special nil
  "Non-nil if special characters are currently red.
See `toggle-red-special'.")
(defun arni-after-setting-font-hook () ; Windows
  (frame-restore)
  (frame-maximize))
(add-hook 'after-setting-font-hook 'arni-after-setting-font-hook)
(defalias 'citations 'toggle-green-cite)
(defun dark-theme ()
  "Apply tango-dark color theme."
  (interactive)
  (load-theme 'tango-dark))
(defun goto-non-ascii ()
  "Go to next non-ASCII character."
  (interactive)
  (deactivate-mark)
  (if (re-search-forward "[^\u0009-\u000a\u0020-\u007e]" nil t)
      (message (concat "Non-ASCII char: " (string (char-before))))
    (message "Only ASCII chars after this point")))
(defun goto-special-char ()
  "Go to next special character.
See also `toggle-red-special'."
  (interactive)
  (deactivate-mark)
  (if (re-search-forward "[^\u0009-\u000a\u0020-\u007e\u00a1-\u00ff]" nil t)
      (message (concat "Special char: " (string (char-before))))
    (message "Only normal chars after this point")))
(defun gray-background ()
  "Set gray background."
  (interactive)
  (set-background-color "gray85"))
(defalias 'highlight-current-line 'hl-line-mode)
(defalias 'linum-mode 'display-line-numbers-mode)
(defun max-colors ()
  "Apply maximum colors, so every face can be distinguished."
  (interactive)
  (arni-colors)
  (set-face-attribute 'font-lock-doc-face nil :foreground "orange"
                      :weight 'bold ) ; Emacs "docstring"
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "brown4"
                      :weight 'bold)) ; x
(defun narrow-to-defun-or-region ()
  "Narrow to function or region."
  (interactive)
  (if (use-region-p)
      (progn (narrow-to-region (point)(mark))
             (goto-char (point-min)))
    (narrow-to-defun)))
(defun occur-non-ascii ()
  "Show all non-ASCII character in buffer."
  (interactive)
  (occur "[^\u0009-\u000a\u0020-\u007e]"))
(defalias 'refresh 'arni-colors)
(defun set-tab-width (&optional n)
  "Set visual `tab-width'."
  (interactive "nTab width: ")
  (setq tab-width n)
  (message "Tab width is now %d" n))
(defun toggle-comments ()
  "Toggle invisible comments."
  (interactive)
  (if (string-equal (fg 'font-lock-comment-face)(bg 'default))
      (set-face-attribute 'font-lock-comment-face nil
                          :foreground default-comment-color)
    (set-face-attribute 'font-lock-comment-face nil :foreground (bg 'default)))
  (message "Comments %s"
           (if (string-equal (fg 'font-lock-comment-face)(bg 'default))
               "OFF" "ON")))
(defun toggle-red-non-ascii ()
  "Toggle red highlighting of non-ASCII characters."
  (interactive)
  (defface special-face '((t :inherit isearch-fail))
    "Face used to highlight special characters."
    :group t)
  (let ((special-chars "[^\u0009-\u000a\u0020-\u007e]"))
    (if red-special
        (progn (hi-lock-unface-buffer special-chars)
               (hi-lock-mode 0))
      (hi-lock-face-buffer special-chars 'special-face))
    (setq red-special (not red-special))
    (force-mode-line-update)
    (message "Red non-ASCII chars %s" (if red-special "ON" "OFF"))))
(defun toggle-red-special ()
  "Toggle red highlighting of special characters.
Special means problematic characters, mainly outside Latin-1, that are hard to
read or save."
  (interactive)
  (defface special-face '((t :inherit isearch-fail))
    "Face used to highlight special characters."
    :group t)
  (let ((special-chars "[^\u0009-\u000a\u0020-\u007e\u00a1-\u00ff]"))
    (if red-special
        (progn (hi-lock-unface-buffer special-chars)
               (hi-lock-mode 0))
      (hi-lock-face-buffer special-chars 'special-face))
    (setq red-special (not red-special))
    (force-mode-line-update)
    (message "Red special chars %s" (if red-special "ON" "OFF"))))
(defun toggle-escape-glyphs ()
  "Toggle invisible escape glyphs."
  (interactive)
  (if (string-equal (fg 'escape-glyph)(bg 'default))
      (set-face-attribute 'escape-glyph nil :foreground default-escape-color)
    (set-face-attribute 'escape-glyph nil :foreground (bg 'default)))
  (message "Escape glyphs %s"
           (if (string-equal (fg 'escape-glyph)(bg 'default)) "OFF" "ON")))
(defun toggle-green-cite ()
  "Toggle green highlighting of citations (like this 2000) and this (2000)."
  (interactive)
  (if green-cite (hi-lock-mode 0)
    ;; Open parenthesis, not closing parenthesis, anything,
    ;; four digits, not closing parenthesis, anything
    (progn (hi-lock-face-buffer "([^)]*[0-9]\\{4\\}[^)]*)" 'hi-green)
           (hi-lock-face-buffer "[()\n]" 'default)
           ;; Commas between anything, parenthesized tail without four digits
           (hi-lock-face-buffer ", " 'default)
           (hi-lock-face-buffer ",[^0-9]*[0-9]\\{0,3\\})" 'default)))
  (setq green-cite (not green-cite))
  (force-mode-line-update)
  (message "Green citations %s" (if green-cite "ON" "OFF")))
(defun toggle-trailing-whitespace ()
  "Toggle highlighting of trailing whitespace."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))(redraw-display)
  (message "Trailing whitespace highlighting %s"
           (if show-trailing-whitespace "ON" "OFF")))
(defalias 'which-section-mode 'which-function-mode)
(defun white-background ()
  "Set white background."
  (interactive)
  (set-background-color "white"))
;;-------------
;; 5.4  Insert
;;-------------
(defun fizz ()
  "Solution to FizzBuzz test."
  (interactive "*")
  (dolist (i (number-sequence 1 100))
    (insert (cond ((and (zerop (mod i 3))(zerop (mod i 5))) "FizzBuzz")
                  ((zerop (mod i 3)) "Fizz")
                  ((zerop (mod i 5)) "Buzz")(t (number-to-string i))) "\n")))
(defun insert-date ()
  "Insert current date as string."
  (interactive "*")
  (insert (format-time-string "%d %b %Y")))
(defun insert-em-dash ()
  "Insert em dash."
  (interactive "*")
  (insert #x2014))
(defun insert-en-dash ()
  "Insert en dash."
  (interactive "*")
  (insert #x2013))
(defun insert-euro ()
  "Insert euro symbol."
  (interactive "*")
  (insert #x20ac))
(defun insert-tab-char (&optional n)
  "Insert TAB character."
  (interactive "*p")
  (insert-char ?\t n))
(defun insert-tab-char-1 ()
  "Insert one TAB character (for `indent-line-function')."
  (interactive "*p")
  (insert "\t"))
(defun lorem (&optional n)
  "Insert N lorem-ipsum paragraphs."
  (interactive "*p")
  (let ((lorem-ipsum "Lorem ipsum dolor sit amet, consectetur adipisicing \
elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut \
enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut \
aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in \
voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint \
occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit \
anim id est laborum."))
    (insert lorem-ipsum "\n")
    (dotimes (i (- n 1))
      (insert "\n" lorem-ipsum "\n"))))
(defun random-int (&optional n)
  "Insert N random integers from 0 to 255."
  (interactive "*p")
  (dotimes (i n)
    (insert (number-to-string (random 256)) "\n")))
(defun random-double (&optional n)
  "Insert N random numbers from from 0 to 0.999."
  (interactive "*p")
  (dotimes (i n)
    (insert (number-to-string (/ (random 1000) 1000.0)) "\n")))
(defun seq (&optional n)
  "Insert 1 to N."
  (interactive "*p")
  (dotimes (i n)
    (insert (number-to-string (+ i 1)) "\n")))
;;-------------
;; 5.5  Format
;;-------------
;; Manipulate spaces only
(defun delete-all-blank-lines ()
  "Delete all blank lines."
  (interactive "*")
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\n\n" nil t)
        (goto-char (point-min))
        (while (search-forward "\n\n" nil t)
          (replace-match "\n")
          (setq count (+ count 1)))
        (goto-char (point-min)))
      (if (= (char-after) ?\n)
          (progn (delete-char 1)
                 (setq count (+ count 1)))))
    (message "Deleted %d blank lines" count)))
(defalias 'clean-trails 'delete-trailing-spc-tab-m)
(defun delete-trailing-spc-tab-m ()
  "Delete spaces, tabs, and ^M glyphs from line ends.
Unlike `delete-trailing-whitespace', deletes ^M in `lisp-mode'."
  (interactive "*")
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t\r]+$" nil t)
        (replace-match "")
        (setq count (+ count 1)))
      (message "Cleaned %d lines" count))))
(defun fill-paragraph-forward (&optional n)
  "Justify N paragraphs and go to end of paragraph."
  (interactive "*p")
  (dotimes (i n)
    (fill-paragraph)
    (forward-paragraph)))
(defun fill-region-or-buffer ()
  "Fill region or buffer."
  (interactive "*")
  (if (use-region-p)
      (fill-region (region-beginning)(region-end))
    (fill-buffer)))
(defun fill-buffer ()
  "Fill all paragraphs."
  (interactive "*")
  (save-excursion
    (fill-region (point-min)(point-max)))
  (message "Filled buffer to %d columns" fill-column))
(defun toggle-tab-function ()
  "Toggle whether TAB does `insert-tab-char' or `indent-or-complete'."
  (interactive)
  (if (equal (where-is-internal 'insert-tab-char nil t) [?\t])
      (local-set-key [?\t] 'indent-or-complete)
    (local-set-key [?\t] 'insert-tab-char))
  (describe-key-briefly [?\t]))
(defun indent-buffer ()
  "Indent all lines."
  (interactive "*")
  (let ((old-bsize (buffer-size))
        (old-hash (buffer-hash))
        (was-modified-flag (buffer-modified-p)))
    (indent-region (point-min)(point-max) nil)
    (if (= (buffer-size) old-bsize)
        (message "Indented buffer (still %d bytes)" (buffer-size))
      (message "Indented buffer (%d->%d bytes)" old-bsize (buffer-size)))
    (if (and (not was-modified-flag)(string-equal (buffer-hash) old-hash))
        (set-buffer-modified-p nil))))
(defun indent-line-or-region ()
  "Indent (`indent-according-to-mode') line, or region if selected."
  (interactive "*")
  (if (use-region-p)
      (indent-region-whole)
    (indent-according-to-mode)))
(defun indent-or-complete ()
  "Indent (`indent-according-to-mode'), or complete if in minibuffer."
  (interactive "*")
  (if (minibuffer-window-active-p (minibuffer-window))
      (minibuffer-complete)
    (if (eq (car (event-modifiers last-input-event)) 'shift)
        (unindent-line-or-region)
      (indent-line-or-region))))
(defun indent-region-whole ()
  "Indent all lines in region.
Unlike `indent-region',  also indent the first half-marked line."
  (interactive "*")
  (let ((end (region-end)))
    (save-excursion
      (goto-char (region-beginning))
      (indent-region (line-beginning-position) end nil))))
;; The following alias is better than
;; `indent-relative' with `indent-according-to-mode'
(defalias 'indent-relative-definitely 'indent-relative)
(defun join-buffer ()
  "Join all paragraphs into long lines."
  (interactive "*")
  (let ((count 0))
    (save-excursion
      (goto-char (point-max))
      ;; In each iteration: skip empty lines, append lines, go one line up
      (while (not (bobp))
        (while (and (bolp)(eolp)(not (bobp)))
          (forward-line -1))
        (if (< (line-beginning-position 0)(line-end-position 0))
            (setq count (+ count 1)))
        (while (< (line-beginning-position 0)
                  (line-end-position 0))
          (delete-indentation))
        (forward-line -1)))
    (message "Joined %d paragraphs" count)))
(defun join-line-nospace (&optional n)
  "Join N lines to previous with no whitespace at join."
  (interactive "*p")
  (dotimes (i n)
    (delete-indentation)
    (delete-horizontal-space)))
(defun join-region (beg end)
  "Join region."
  (interactive "*r")
  (let ((count 0))
    (save-excursion
      (goto-char end)
      ;; In each iteration: skip empty lines, append lines, go one line up
      (while (> (point) beg)
        (while (and (bolp)(eolp)(> (point) beg))
          (forward-line -1))
        (while (and (> (point) beg)
                    (< (line-beginning-position 0)(line-end-position 0)))
          (delete-indentation)
          (setq count (+ count 1)))
        (forward-line -1)))
    (message "Joined %d lines" count)))
(defun join-region-or-buffer ()
  "Join region or buffer."
  (interactive "*")
  (if (use-region-p)
      (join-region (region-beginning)(region-end))
    (join-buffer)))
(defalias 'spaces-to-tabs 'tabify-spaces)
(defun tabify-spaces ()
  "Replace all spaces with tabs."
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward " +" nil t)
      (replace-match "\t"))))
(defun tabify-spaces-copy-buffer ()
  "Replace all spaces with tabs and copy buffer."
  (interactive "*")
  (tabify-spaces)
  (copy-buffer)
  (message "Tabified and copied buffer"))
(defun unindent-line ()
  "Unindent line, removing all whitespace at beginning of line."
  (interactive "*")
  (save-excursion
    (indent-rigidly (line-beginning-position)
                    (+ (line-beginning-position) 1)
                    -1000)))
(defun unindent-line-or-region ()
  "Unindent line, or region if selected."
  (interactive "*")
  (if (use-region-p)
      (unindent-region)
    (unindent-line)))
(defun unindent-region ()
  "Unindent region, removing all whitespace at beginning of line."
  (interactive "*")
  (let ((end (region-end)))
    (save-excursion
      (goto-char (region-beginning))
      (indent-rigidly (line-beginning-position) end -1000))))
(defun unindent-buffer ()
  "Unindent all lines."
  (interactive "*")
  (let ((old-bsize (buffer-size)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[\t ]+" nil t)
        (replace-match "")))
    (if (= (buffer-size) old-bsize)
        (message "Unindented buffer (still %d bytes)" (buffer-size))
      (message "Unindented buffer (%d->%d bytes)" old-bsize (buffer-size)))))
(defun untabify-buffer ()
  "Convert all tabs to several spaces, preserving column alignment."
  (interactive "*")
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\t" nil t)
        (untabify (- (point) 1)(point))
        (setq count (+ count 1))
        (goto-char (point-min)))
      (message "Converted %d tabs to spaces" count))))
;;------------
;; 5.6  Tools
;;------------
(defun blank-region (beg end)
  "Replace region with spaces."
  (interactive "*r")
  (let ((len (- end beg))
        (overwrite-mode 'overwrite-mode-textual))
    (delete-region beg end)
    (insert-char 32 len)))
(defun blank-to-paren ()
  "Replace every character with space until next closing parenthesis."
  (interactive "*")
  (if (zerop (how-many ")" (point)(line-end-position)))
      (message "No closing parenthesis between point and end of line")
    (while (not (= (char-after) #x29))
      (progn (delete-char 1)
             (insert " ")))))
(defun case-fold-string (str)
  "Create case-insensitive regexp from string."
  (mapconcat
   (lambda (x) (concat "[" (string (upcase x))(string (downcase x)) "]"))
   str ""))
(defun comment-line-or-region ()
  "Comment line or region."
  (interactive "*")
  (if (use-region-p)
      (comment-region
       (save-excursion
         (goto-char (region-beginning))
         (line-beginning-position))
       (save-excursion
         (goto-char (region-end))
         (line-end-position)))
    (comment-region (line-beginning-position)(line-end-position))))
(defun comment-then-up (&optional n)
  "Comment line/region and go to previous line, N times."
  (interactive "*p")
  (dotimes (i n)
    (comment-line-or-region)
    (forward-line -1)))
(defun comment-then-down (&optional n)
  "Comment line/region and go to next line, N times."
  (interactive "*p")
  (dotimes (i n)
    (comment-line-or-region)
    (forward-line 1)))
(defun convert-special ()
  "Convert special characters to Latin-1.
Remove hex 00-08,0b-1f and convert 7f-a0
using 'plain' \"quotes\" and double -- em dash."
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "[\u0000-\u0008\u000b-\u001f\u007f\u0081\u008d\u008f\u0090\u009d]"
            nil t)
      (replace-match ""))
    (goto-char (point-min)) ; Latin-1 left single quotation mark
    (while (search-forward "\u0091" nil t)(replace-match "'"))
    (goto-char (point-min)) ; Latin-1 right single quotation mark
    (while (search-forward "\u0092" nil t)(replace-match "'"))
    (goto-char (point-min)) ; Latin-1 left double quotation mark
    (while (search-forward "\u0093" nil t)(replace-match "\""))
    (goto-char (point-min)) ; Latin-1 right double quotation mark
    (while (search-forward "\u0094" nil t)(replace-match "\""))
    (goto-char (point-min)) ; Latin-1 bullet
    (while (search-forward "\u0095" nil t)(replace-match "-"  ))
    (goto-char (point-min)) ; Latin-1 en dash
    (while (search-forward "\u0096" nil t)(replace-match "-"  ))
    (goto-char (point-min)) ; Latin-1 em dash
    (while (search-forward "\u0097" nil t)(replace-match "-"  ))
    (goto-char (point-min)) ; Latin-1 tilde
    (while (search-forward "\u0098" nil t)(replace-match "~"  ))
    (goto-char (point-min)) ; Latin-1 trade mark sign
    (while (search-forward "\u0099" nil t)(replace-match "TM"  ))
    (goto-char (point-min)) ; no-break space
    (while (search-forward "\u00a0" nil t)(replace-match " "  ))
    (goto-char (point-min)) ; soft hyphen
    (while (search-forward "\u00ad" nil t)(replace-match "-"  ))
    (goto-char (point-min)) ; degree
    (while (search-forward "\u00b0" nil t)(replace-match "deg"))
    (goto-char (point-min)) ; latin capital ligature oe
    (while (search-forward "\u0152" nil t)(replace-match "Æ"  ))
    (goto-char (point-min)) ; latin small ligature oe
    (while (search-forward "\u0153" nil t)(replace-match "æ"  ))
    (goto-char (point-min)) ; latin capital letter s with caron
    (while (search-forward "\u0160" nil t)(replace-match "S"  ))
    (goto-char (point-min)) ; latin small letter s with caron
    (while (search-forward "\u0161" nil t)(replace-match "s"  ))
    (goto-char (point-min)) ; latin capital letter y with diaeresis
    (while (search-forward "\u0178" nil t)(replace-match "Y"  ))
    (goto-char (point-min)) ; latin capital letter z with caron
    (while (search-forward "\u017d" nil t)(replace-match "Z"  ))
    (goto-char (point-min)) ; latin small letter z with caron
    (while (search-forward "\u017e" nil t)(replace-match "z"  ))
    (goto-char (point-min)) ; latin small letter f with hook
    (while (search-forward "\u0192" nil t)(replace-match "f"  ))
    (goto-char (point-min)) ; modifier letter circumflex accent
    (while (search-forward "\u02c6" nil t)(replace-match "^"  ))
    (goto-char (point-min)) ; small tilde
    (while (search-forward "\u02dc" nil t)(replace-match ""   ))
    (goto-char (point-min)) ; zero width space
    (while (search-forward "\u2002" nil t)(replace-match " "  ))
    (goto-char (point-min)) ; en space
    (while (search-forward "\u200a" nil t)(replace-match ""   ))
    (goto-char (point-min)) ; hair space
    (while (search-forward "\u200b" nil t)(replace-match ""   ))
    (goto-char (point-min)) ; hyphen
    (while (search-forward "\u200c" nil t)(replace-match ""   ))
    (goto-char (point-min)) ; zero width non-joiner
    (while (search-forward "\u200d" nil t)(replace-match ""   ))
    (goto-char (point-min)) ; zero width joiner
    (while (search-forward "\u200e" nil t)(replace-match ""   ))
    (goto-char (point-min)) ; invisible left-to-right mark
    (while (search-forward "\u200f" nil t)(replace-match ""   ))
    (goto-char (point-min)) ; invisible right-to-left mark
    (while (search-forward "\u2010" nil t)(replace-match "-"  ))
    (goto-char (point-min)) ; en dash
    (while (search-forward "\u2013" nil t)(replace-match "-"  ))
    (goto-char (point-min)) ; em dash
    (while (search-forward "\u2014" nil t)(replace-match "--" ))
    (goto-char (point-min)) ; left single quotation mark
    (while (search-forward "\u2018" nil t)(replace-match "'"  ))
    (goto-char (point-min)) ; right single quotation mark
    (while (search-forward "\u2019" nil t)(replace-match "'"  ))
    (goto-char (point-min)) ; single low-9 quotation mark ,
    (while (search-forward "\u201a" nil t)(replace-match ","  ))
    (goto-char (point-min)) ; left double quotation mark
    (while (search-forward "\u201c" nil t)(replace-match "\"" ))
    (goto-char (point-min)) ; right double quotation mark
    (while (search-forward "\u201d" nil t)(replace-match "\"" ))
    (goto-char (point-min)) ; double low-9 quotation mark ,,
    (while (search-forward "\u201e" nil t)(replace-match "\"" ))
    (goto-char (point-min)) ; dagger
    (while (search-forward "\u2020" nil t)(replace-match "*"  ))
    (goto-char (point-min)) ; double dagger
    (while (search-forward "\u2021" nil t)(replace-match "*"  ))
    (goto-char (point-min)) ; bullet
    (while (search-forward "\u2022" nil t)(replace-match "-"  ))
    (goto-char (point-min)) ; horizontal ellipsis
    (while (search-forward "\u2026" nil t)(replace-match "..."))
    (goto-char (point-min)) ; per mille sign
    (while (search-forward "\u2030" nil t)(replace-match "%"  ))
    (goto-char (point-min)) ; single left-pointing angle quotation mark
    (while (search-forward "\u2039" nil t)(replace-match "<"  ))
    (goto-char (point-min)) ; single right-pointing angle quotation mark
    (while (search-forward "\u203a" nil t)(replace-match ">"  ))
    (goto-char (point-min)) ; word joiner
    (while (search-forward "\u2060" nil t)(replace-match ""   ))
    (goto-char (point-min)) ; euro sign
    (while (search-forward "\u20ac" nil t)(replace-match "EUR"))
    (goto-char (point-min)) ; trade mark sign
    (while (search-forward "\u2122" nil t)(replace-match "TM" ))
    (goto-char (point-min)) ; minus
    (while (search-forward "\u2212" nil t)(replace-match "-"  ))
    (goto-char (point-min)) ; less than or equal
    (while (search-forward "\u2264" nil t)(replace-match "<=" ))
    (goto-char (point-min)) ; greater than or equal
    (while (search-forward "\u2265" nil t)(replace-match ">=" ))
    (goto-char (point-min)) ; black circle
    (while (search-forward "\u25cf" nil t)(replace-match "-"  ))))
(defun count-everything ()
  "Show line number and position, then count lines and characters
in region or buffer.
See also `count-words'."
  (interactive)
  (if (use-region-p)
      (count-everything-region (region-beginning)(region-end))
    (count-everything-region (point-min)(point-max))))
(defun count-everything-region (beg end)
  "Show line number and position, then count lines and characters in region.
See also `count-words'."
  (interactive "r")
  (let ((current-line (what-line))
        (current-pos (point))
        (lines (count-lines beg end))
        (chars (- end beg)))
    (message "%s [pos %d]   (%d lines   %d chars)"
             current-line current-pos lines chars)))
(defun delete-comments ()
  "Delete all comments."
  (interactive "*")
  (let ((old-bsize (buffer-size)))
    (save-excursion
      (goto-char (point-min))
      (comment-kill (count-lines (point-min)(point-max))))
    (if (= (buffer-size) old-bsize)
        (message "Deleted all comments (still %d bytes)" (buffer-size))
      (message "Deleted all comments (%d->%d bytes)" old-bsize (buffer-size)))))
(defun delete-duplicate-lines-region-or-buffer ()
  "Delete duplicate lines in region or buffer."
  (interactive "*")
  (if (use-region-p)
      (delete-duplicate-lines (region-beginning)(region-end) nil nil nil t)
    (delete-duplicate-lines (point-min)(point-max) nil nil nil t)))
(defun diff-this-buffer-with-file ()
  "Diff current buffer against a saved file."
  (interactive)
  (diff-buffer-with-file (buffer-name)))
(defun doc-to-txt ()
  "Routine for pasting from Word into a text file (to diff two versions:
Clear buffer, paste, untabify, unindent, use single spaces, delete blank lines."
  (interactive)
  (delete-region (point-min)(point-max))
  (yank-quiet)
  (untabify-buffer)
  (unindent-buffer)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "  +" nil t)
      (replace-match " ")))
  (delete-all-blank-lines)
  (save-buffer))
(defun dot-emacs-edit ()
  "Open .emacs."
  (interactive)
  (find-file "~/.emacs"))
(defun dot-emacs-eval ()
  "Evaluate .emacs."
  (interactive)
  (load-file "~/.emacs"))
(defun downcase-word-or-region (&optional n)
  "Downcase N words or region."
  (interactive "*p")
  (if (use-region-p)
      (downcase-region (point)(mark))
    (downcase-word n)))
(defun find-duplicate-lines ()
  "Find duplicated lines in buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (switch-to-buffer "*unique*")
    (insert-buffer-substring-no-properties buffer)
    (delete-duplicate-lines (point-min)(point-max))
    (read-only-mode)
    (ediff-buffers buffer "*unique*"))
  (message "Run `delete-duplicate-lines-all' to delete all duplicates."))
(defalias 'find-duplicate-word 'the-the)
(defun git-log-clean ()
  "Clean up output from git log, e.g. from GitHub."
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\u001b[31m" nil t)(replace-match ""))
    (goto-char (point-min))
    (while (search-forward "\u001b[m" nil t)(replace-match ""))
    (goto-char (point-min))
    (while (search-forward "\u001b[34m " nil t)(replace-match ""))))
(defun google-decode-url ()
  "Convert Google URL to plain URL and copy line to clipboard."
  (interactive "*")
  (url-unhex-region (line-beginning-position)(line-end-position))
  ;; Remove head and tail if "google" in url
  (if (string-match "google"
                    (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position)))
      (progn (re-search-backward "&usg.*$" nil t)
             (replace-match "")
             (re-search-backward "^.*&url=" nil t)
             (replace-match "")))
  (kill-new (buffer-substring-no-properties
             (line-beginning-position)(line-end-position)))
  (message "Copied line"))
(defun msg (obj)
  "Show OBJ in minibuffer."
  (message (prin1-to-string obj)))
(defalias 'ps 'proced)
(defun record-macro ()
  "Start or stop recording keyboard macro."
  (interactive)
  (if defining-kbd-macro
      (kmacro-end-macro nil)
    (kmacro-start-macro nil)))
(defun simplify-vertical-space ()
  "Replace multiple empty lines to single empty lines."
  (interactive "*")
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\n\n\n+" nil t)
        (replace-match "\n\n")
        (setq count (+ count 1))))
    (message "Converted %d vertical spaces" count)))
(defun sort-lines-caps-first (beg end)
  "Sort lines in region, with capital letters first."
  (interactive "*r")
  (let ((sort-fold-case nil))
    (sort-lines nil beg end)))
(defun tab-to-csv ()
  "Replace all tabs with commas to create a CSV file."
  (interactive "*")
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\t" nil t)
        (replace-match ",")
        (setq count (+ count 1))))
    (message "Replaced %d tabs with commas" count)))
(defun the-the ()
  "Search forward for a duplicate word."
  (interactive)
  (deactivate-mark)
  (if (re-search-forward "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil t)
      (message "Looks dubious")
    (message "No duplicate words found after this point")))
(defun uncomment-line-or-region ()
  "Uncomment line or region.
Warning: uncomments appended comments, so that
  int i=0;  // counter
becomes
  int i=0;  counter
which doesn't compile."
  (interactive "*")
  (if (use-region-p)
      (uncomment-region
       (save-excursion
         (goto-char (region-beginning))
         (line-beginning-position))
       (save-excursion
         (goto-char (region-end))
         (line-end-position)))
    (uncomment-region (line-beginning-position)(line-end-position))))
(defun uncomment-then-up (&optional n)
  "Uncomment line/region and go to previous line, N times."
  (interactive "*p")
  (dotimes (i n)
    (uncomment-line-or-region)
    (forward-line -1)))
(defun uncomment-then-down (&optional n)
  "Uncomment line/region and go to next line, N times."
  (interactive "*p")
  (dotimes (i n)
    (uncomment-line-or-region)
    (forward-line 1)))
(defalias 'unique-lines 'delete-duplicate-lines-region-or-buffer)
(defun upcase-word-or-region (&optional n)
  "Upcase N words or region."
  (interactive "*p")
  (if (use-region-p)
      (upcase-region (point)(mark))
    (upcase-word n)))
(defun url-hexify-region (beg end)
  "Convert characters to %XX codes."
  (interactive "*r")
  (let ((text (url-hexify-string (buffer-substring beg end))))
    (delete-region beg end)
    (insert text)))
(defun url-unhex-region (beg end)
  "Convert %XX codes to characters."
  (interactive "*r")
  (let ((text (url-unhex-string (buffer-substring beg end))))
    (delete-region beg end)
    (insert text)))
(defun windows-path ()
  "Copy semicolon-separated path string from a list of path entries."
  (interactive)
  (setq path (buffer-string))
  (setq path (replace-regexp-in-string "\n" ";" path))
  (setq path (replace-regexp-in-string ";$" "" path))
  (kill-new path)
  (message "Copied Windows path to clipboard"))
;;-------------
;; 5.7  Window
;;-------------
(defun already-top-bottom-p ()
  "Return t if selected frame is split in two windows on top and bottom."
  (and (= (length (window-list)) 2)
       (window-full-width-p)))
(defun already-left-right-p ()
  "Return t if selected frame is split in two windows on left and right."
  (and (= (length (window-list)) 2)
       (window-full-height-p)))
(defun maximize-window-top ()
  "Make top window as large as possible, shrinking bottom window."
  (interactive)
  (split-window-top-bottom)
  (maximize-window))
(defun middle-from-here ()
  "Return the line halfway between current position and end of buffer."
  (/ (count-lines (point)(point-max)) 2))
(defun pages (&optional n)
  "Return the number of lines to scroll down N pages."
  (* n (- (window-body-height) next-screen-context-lines)))
(defun split-window-grid (nr nc)
  "Arrange windows in NR rows and NC columns."
  (interactive "nRows: \nnColumns: ")
  (delete-other-windows)
  (windmove-default-keybindings 'meta)
  (global-set-key [27 left]  'windmove-left ) ; M-left in terminal
  (global-set-key [27 right] 'windmove-right) ; M-right in terminal
  (global-set-key [27 up]    'windmove-up   ) ; M-up in terminal
  (global-set-key [27 down]  'windmove-down ) ; M-down in terminal
  (dotimes (i (- nc 1))
    (split-window-right)
    (dotimes (j (- nr 1))(split-window-below))
    (other-window nr))
  (dotimes (j (- nr 1))(split-window-below))
  (balance-windows))
(defun split-window-left-right ()
  "Arrange two windows on left and right side of frame."
  (interactive)
  (let ((other-window-buffer (window-buffer (next-window)))
        (was-bottom (and (already-top-bottom-p)
                         (not (eq (selected-window)(frame-first-window))))))
    (if (not (already-left-right-p))
        (progn (delete-other-windows)
               (split-window-right)
               (set-window-buffer (next-window) other-window-buffer)
               (if was-bottom (transpose-windows))))))
(defun split-window-top-bottom ()
  "Arrange two windows on top and bottom of frame."
  (interactive)
  (let ((other-window-buffer (window-buffer (next-window)))
        (was-right (and (already-left-right-p)
                        (not (eq (selected-window)(frame-first-window))))))
    (if (not (already-top-bottom-p))
        (progn (delete-other-windows)
               (split-window-below)
               (set-window-buffer (next-window) other-window-buffer)
               (if was-right (transpose-windows))))))
(defun transpose-windows ()
  "Swap contents of two windows."
  (interactive)
  (if (= (length (window-list)) 2)
      (let ((other-window-buffer (window-buffer (next-window))))
        (set-window-buffer (next-window)(current-buffer))
        (switch-to-buffer other-window-buffer)
        (other-window 1))))
;;-----------
;; 5.8  Help
;;-----------
(defun ascii-table ()
  "Show ASCII table as unibytes (0-255)."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (set-buffer-multibyte nil)
  (insert "ASCII characters up to number 255.\n")
  (dotimes (i 256)
    (insert (format "%4d | \\%03o | \\x%02x | %c\n" i i i i)))
  (goto-char (point-min)))
(defalias 'current-encoding 'describe-current-coding-brief)
(defun describe-current-coding-brief ()
  "Show current coding system in minibuffer."
  (interactive)
  (message (prin1-to-string buffer-file-coding-system)))
(defun describe-current-coding-full ()
  "Describe current coding systems in detail."
  (interactive)
  (describe-current-coding-system)
  (with-current-buffer "*Help*" (setq show-trailing-whitespace nil)))
(defalias 'display-colors 'list-colors-display)
(defalias 'display-faces 'list-faces-display)
(defun latin-1-table ()
  "Show Latin-1 table as multibytes (0-127,unicode)."
  (interactive)
  (switch-to-buffer "*Latin-1*")
  (erase-buffer)
  (set-buffer-multibyte t)
  (insert "Latin-1 characters up to number 255.\n")
  (dotimes (i 256)
    (insert (format "%4d | \\%03o | \\x%02x | %c\n" i i i i)))
  (goto-char (point-min)))
(defun list-colors-fullscreen ()
  "List colors in full screen."
  (interactive)
  (list-colors-display)
  (delete-other-windows (get-buffer-window "*Colors*"))
  (message nil))
;;==============================================================================
;;
;; 6  LANGUAGE MODES
;;
;;==============================================================================
;; Settings, faces, keys, functions
;;----------
;; 6.1  Ada
;;----------
(defun arni-ada-hook ()
  (local-unset-key [C-tab])
  (local-set-key [?\C-c ?\C-c] 'ada-build)
  (local-set-key [?\C-c ?\C-v] 'ada-run  )
  (defun ada-build ()
    "Build simple Ada program."
    (interactive)
    (save-buffer)(ada-compile-application))
  (defun ada-run ()
    "Run simple Ada program."
    (interactive)
    (let ((resize-mini-windows nil))
      (if (one-window-p)(split-window-right))
      (shell-command (file-name-sans-extension (buffer-name))))))
(add-hook 'ada-mode-hook 'arni-ada-hook)
;;-----------
;; 6.2  ADMB
;;-----------
(defun std-to-tab ()
  "Convert ADMB *.std output to tab-separated text."
  (interactive)
  (setq tab-width 23)
  (goto-char (point-min))
  (delete-trailing-spc-tab-m)
  (unindent-buffer)
  (save-excursion
    (if (search-forward "std dev" nil t)
        (replace-match "std.dev")))
  (save-excursion
    (while (re-search-forward " +" nil t)
      (replace-match "\t"))))
(defun arni-admb-hook ()
  (setq make-backup-files t)
  (arni-colors)
  ;; Allow file-local variables to set `admb-init' to these values
  (add-to-list 'safe-local-variable-values
               '(admb-init . "admb050-vc6 & "))
  (add-to-list 'safe-local-variable-values
               '(admb-init . "admb111-gcc472-win32 & "))
  (set-face-attribute 'font-lock-function-name-face nil :foreground "brown4")
  (local-set-key [f11]         'admb-outline-remember)
  (local-set-key [?\C-c ?\C-r] 'admb-rep-browser     )
  (defun admb-outline-remember ()
    "Navigate within ADMB file using `outline-mode', remembering previous mode."
    (interactive)
    (admb-outline)(setq outline-previous-mode '(admb-mode))))
(add-hook 'admb-mode-hook 'arni-admb-hook)
;;---------------
;; 6.3  Assembly
;;---------------
(defalias 'assembly-mode 'asm-mode)
(setq asm-comment-char 35) ; comment char (# in GAS) cannot be set inside hook
;;-----------
;; 6.4  Bash
;;-----------
(defun arni-sh-hook ()
  (setq make-backup-files t)
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)
  (setq sh-test '("[[  ]]" . 4))
  (sh-set-shell "bash")
  (message nil)
  (defvar sh-dos2unix nil
    "Whether to automatically convert shell scripts from dos2unix.")
  (if (string-match "dos" (prin1-to-string buffer-file-coding-system))
      (if sh-dos2unix
          (progn
            (set-buffer-file-coding-system 'utf-8-unix t)
            (message "Warning: file had Dos line endings, changed to Unix."))
        (message "Warning: file has Dos line endings")))
  ;; Syntax highlighting for [OPTIND-1], -option, --option
  (font-lock-add-keywords nil '(("[\[-]+\\(\\w+\\)"
                                 (1 font-lock-type-face append))))
  (font-lock-add-keywords nil '(("^[ \t]*set " . font-lock-warning-face)))
  (arni-colors)
  (set-face-attribute 'font-lock-builtin-face nil
                      :foreground (fg 'font-lock-keyword-face)) ; echo
  (set-face-attribute 'font-lock-type-face nil :foreground "magenta4") ; -arg
  (set-face-attribute 'font-lock-variable-name-face nil :weight 'bold) ; $x
  (set-face-attribute 'sh-heredoc nil :foreground "red") ; <<EOF
  (set-face-attribute 'sh-quoted-exec nil :foreground "brown4") ; `cmd`
  (local-unset-key [?\C-c ?=]) ; reactivate diff-this-buffer-with-file
  (local-set-key [f9]    'sh-send-line-or-region-and-step          )
  (local-set-key [f11]   'sh-outline                               )
  (local-set-key [C-f12] 'sh-template-mini                         )
  (local-set-key [M-f12] 'sh-template                              )
  (local-set-key [?\C-m] 'sh-indent-newline-indent-or-delete-region) ; return
  (local-set-key [?\C-c ?\C-c] 'sh-eval-buffer                     ) ; sh-case
  (local-set-key [?\C-c ?\C-j] 'sh-send-line-or-region-stay        )
  (local-set-key [?\C-c ?\C-v] 'sh-eval-buffer                     )
  (defun sh-eval-buffer ()
    "Evaluate commands in buffer in inferior shell.
See also `sh-send-line-or-region-and-step'."
    (interactive)
    (sh-send-text (buffer-string)))
  (defun sh-indent-newline-indent ()
    "Indent, insert newline, indent."
    (interactive "*")
    (insert " ")
    (indent-according-to-mode)
    (newline)
    ;; (clean-trails)
    (message nil)
    (indent-according-to-mode)) ; handle 'case'
  (defun sh-indent-newline-indent-or-delete-region ()
    "Indent, insert newline, and indent, or delete region."
    (interactive "*")
    (if (use-region-p)
        (progn (delete-region (point)(mark))
               (newline))
      (sh-indent-newline-indent)))
  (defun sh-outline ()
    "Navigate within shell script using `outline-mode'."
    (interactive)
    (outline-mode)
    (setq outline-regexp "#")
    (outline-mode)
    (outline-hide-body)
    (setq outline-previous-mode '(sh-mode))
    (set-face-attribute 'outline-1 nil :inherit font-lock-comment-face))
  (defun sh-send-line-or-region-stay ()
    "Run line or region and stay."
    (interactive)
    (save-excursion
      (sh-send-line-or-region-and-step)))
  (defun sh-template-mini ()
    "Insert minimal sh template."
    (interactive "*")
    (goto-char (point-min))
    (insert "#!/bin/bash\n\n"))
  (defun sh-template ()
    "Insert sh template."
    (interactive "*")
    (goto-char (point-min))
    (insert "\
#!/bin/bash
shopt -s expand_aliases
alias help='echo \"Usage: \"'
if [[ \"$1\" == \"\" ]]; then help; exit; fi
if [[ \"$1\" == \"--help\" ]]; then help; exit; fi
################################################################################
###                                                                            #
### Script:                                                                    #
###                                                                            #
### Purpose:                                                                   #
###                                                                            #
### Args:                                                                      #
###                                                                            #
### Notes:                                                                     #
###                                                                            #
### Warning:                                                                   #
###                                                                            #
### Requires:                                                                  #
###                                                                            #
### Returns:                                                                   #
###                                                                            #
################################################################################

while getopts \"p:\" A; do
  case $A in
    p) arg1=$OPTARG;;
  esac
done
shift $((OPTIND-1))

")
    (goto-char (point-min))
    (search-forward ":   ")
    (overwrite-mode t)))
(add-hook 'sh-mode-hook 'arni-sh-hook)
;;---------
;; 6.5  C#
;;---------
(defun arni-csharp-hook ()(setq c-basic-offset 2))
(add-hook 'csharp-mode-hook 'arni-csharp-hook)
;;----------
;; 6.6  C++
;;----------
(defun arni-c-hook ()
  (setq make-backup-files t)
  (abbrev-mode 0)
  ;; (setq-default c-electric-flag nil)
  (c-set-offset 'case-label '+) ; indent switch cases
  (c-set-offset 'comment-intro 0) ; don't indent comments more than statements
  ;; Don't indent comments between function() and {
  (c-set-offset 'func-decl-cont 0)
  (c-set-offset 'substatement-open 0) ; don't indent {
  (local-unset-key [?\C-\M-e]) ; reactivate query-replace-regexp
  (local-unset-key [?\C-c ?.]) ; reactivate comment-line-or-region
  (local-unset-key [?/]      ) ; reactivate normal slash
  (local-set-key [f11]               'hs-minor-mode      )
  (local-set-key [C-f12]             'cpp-template-mini  )
  (local-set-key [M-f12]             'cpp-template       )
  (local-set-key [?\C-m]             'newline-and-indent ) ; return
  (local-set-key [M-return]          'cpp-endl           )
  (local-set-key [?\C-\M-m]          'cpp-endl           ) ; M-RET in terminal
  (local-set-key [?\C-c C-backspace] 'cpp-clean          )
  (local-set-key [?\C-c ?\C-,]       'previous-error     )
  (local-set-key [?\C-c ?\C-.]       'next-error         )
  (local-set-key [?\C-c ?\C-a]       'cpp-run-args       )
  (local-set-key [?\C-c ?\C-b]       'cpp-run-gdb        )
  (local-set-key [?\C-c ?\C-c]       'cpp-compile        )
  (local-set-key [?\C-c ?\C-d]       'cpp-compile-symbols)
  (local-set-key [?\C-c ?\C-f]       'cpp-for            )
  (local-set-key [?\C-c ?\C-h]       'cpp-header         )
  (local-set-key [?\C-c ?\C-i]       'cpp-include        )
  (local-set-key [?\C-c ?\C-l]       'lex-compile        )
  (local-set-key [?\C-c ?\C-m]       'cpp-compile-make   )
  (local-set-key [?\C-c ?\C-t]       'cpp-typeid         )
  (local-set-key [?\C-c ?\C-v]       'cpp-run            )
  (local-set-key [?\C-c ?\C-y]       'doxymacs-mode-with-hook)
  (defun cpp-clean ()
    "Remove C++ binary files (*.o *.so *.dll)."
    (interactive)
    (let* ((prog (file-name-sans-extension (buffer-name)))
           (pattern (concat prog "\\.o\\|" prog "\\.so\\|" prog "\\.dll"))
           (files (directory-files "." nil pattern t)))
      (dolist (x files)(delete-file x)))
    (message "Removed binary files"))
  (defun cpp-compile ()
    "Build simple C++ program (one source file)."
    (interactive)
    (save-buffer)
    (if (one-window-p)(split-window-right))
    (compile (concat "g++ -s -static -Wall -o "
                     (file-name-sans-extension (buffer-name)) " "
                     (buffer-name))))
  (defun cpp-compile-make ()
    "Build C++ program using Makefile."
    (interactive)
    (save-buffer)
    (if (one-window-p)(split-window-right))
    (compile "make"))
  (defun cpp-compile-symbols ()
    "Build simple C++ program (one source file) with debug symbols."
    (interactive)
    (save-buffer)
    (if (one-window-p)(split-window-right))
    (compile (concat "g++ -g -static -Wall -o "
                     (file-name-sans-extension (buffer-name)) " "
                     (buffer-name))))
  (defun cpp-endl ()
    "Insert << endl; (or just endl;) and newline."
    (interactive "*")
    (delete-horizontal-space)
    (if (looking-back "<" 10)
        (insert " endl;")
      (insert " << endl;")))
  (defun cpp-for ()
    "Insert for(int i=0; i<; i++)."
    (interactive "*")
    (insert "for(int i=0; i<; i++)")
    (search-backward ";"))
  (defun cpp-header ()
    "Insert include guards (#ifndef, #define, #endif) at buffer top and bottom."
    (interactive "*")
    (let ((_CLASS_H (concat "_"
                            (upcase (file-name-sans-extension (buffer-name)))
                            "_H")))
      (save-excursion
        (goto-char (point-min))
        (insert "#ifndef " _CLASS_H "\n#define " _CLASS_H "\n")
        (goto-line-lisp (line-number-at-pos (point-max)))
        (insert "\n#endif // " _CLASS_H "\n"))))
  (defun cpp-include ()
    "Insert #include <>."
    (interactive "*")
    (insert "#include <>")
    (if (not (= (char-after) ?\n))
        (insert "\n"))
    (search-backward ">"))
  (defun cpp-run ()
    "Run simple C++ program."
    (interactive)
    (let ((resize-mini-windows nil))
      (if (one-window-p)(split-window-right))
      (shell-command (file-name-sans-extension (buffer-name)))))
  (defun cpp-run-args (args)
    "Run simple C++ program with args."
    (interactive "sArgs: ")
    (let ((resize-mini-windows nil))
      (if (one-window-p)(split-window-right))
      (shell-command
       (concat (file-name-sans-extension (buffer-name)) " " args))))
  (defun cpp-run-gdb ()
    "Debug C++ program using gdb."
    (interactive)
    (gdb (concat "gdb --annotate=3 " (file-name-sans-extension (buffer-name)))))
  (defun cpp-template ()
    "Insert C++ template."
    (interactive "*")
    (goto-char (point-min))
    (insert "\
//==============================================================================
// Program:
// Usage:
// Purpose:
// Notes:
// Warning:
// Includes:  iostream
// History:
//==============================================================================
#include <iostream>
using std::cout;
using std::endl;

//------------------------------------------------------------------------------
// Function: main
// Purpose:
// Args:     argc is the number of command line options + 1
//           argv is a vector containing the program name and cmd line options
// Requires: iostream (cout, endl)
// Returns:  0
//------------------------------------------------------------------------------
int main(int argc, char** argv)
{
  cout << \"\" << endl;
}
")
    (goto-char (point-min)))
  (defun cpp-template-mini ()
    "Insert minimal C++ template."
    (interactive "*")
    (goto-char (point-min))
    (insert "\
#include <iostream>
using namespace std;

int main()
{
  cout << \"\" << endl;
}
")
    (goto-char (point-min))
    (search-forward "\""))
  (defun cpp-typeid ()
    "Insert typeid().name()."
    (interactive "*")
    (insert "typeid().name()")
    (search-backward ")."))
  (defun lex-compile ()
    "Translate lex to C."
    (interactive)
    (compile (concat "flex " (buffer-name)))))
(add-hook 'c-mode-common-hook 'arni-c-hook)
(defun arni-ebrowse-hook ()
  (split-window-right)
  (ebrowse-tree-command:show-member-functions)
  (skip-chars-forward " \t")
  (local-unset-key [?\C-l]) ; reactivate recenter
  (local-set-key [down-mouse-1] 'ebrowse-mouse-tree-variables         )
  (local-set-key [mouse-2] 'ebrowse-mouse-tree-declaration            )
  (local-set-key [down-mouse-3] 'ebrowse-mouse-2-in-tree-buffer       )
  (local-set-key [f5]      'ebrowse-restore-windows                   )
  (local-set-key [S-f5]    'ebrowse-redraw-tree                       )
  (local-set-key [C-home]  'ebrowse-tree-top                          )
  (local-set-key [C-S-home] 'region-bol-top                           )
  (local-set-key [C-end]   'ebrowse-tree-bottom                       )
  (local-set-key [C-S-end] 'region-bol-bottom                         )
  (local-set-key [prior]   'ebrowse-tree-up-page                      )
  (local-set-key [next]    'ebrowse-tree-down-page                    )
  (local-set-key [?\t]     'ebrowse-tree-switch                       )
  (local-set-key [backtab] 'ebrowse-tree-switch                       )
  (local-set-key [?\C-m]   'ebrowse-tree-view-declaration             ) ; return
  (local-set-key [C-return] 'ebrowse-tree-view-declaration            )
  (local-set-key [left]    'ebrowse-tree-command:show-member-variables)
  (local-set-key [right]   'ebrowse-tree-command:show-member-functions)
  (local-set-key [up]      'ebrowse-tree-up                           )
  (local-set-key [down]    'ebrowse-tree-down                         )
  (local-set-key [S-up]    'region-bol-up                             )
  (local-set-key [S-down]  'region-bol-down                           )
  (local-set-key [C-up]    'ebrowse-tree-up-3                         )
  (local-set-key [C-down]  'ebrowse-tree-down-3                       )
  (local-set-key [M-up]    'ebrowse-tree-up-view                      )
  (local-set-key [M-down]  'ebrowse-tree-down-view                    )
  (local-set-key [?\C-n]   'ebrowse-tree-down                         )
  (local-set-key [?\C-p]   'ebrowse-tree-up                           )
  (local-set-key [?\C-v]   'ebrowse-tree-down-page                    )
  (local-set-key [?\C-y]   'ebrowse-tree-up-page                      )
  (local-set-key [?*]      'ebrowse-tree-member-public                )
  (local-set-key [?M]      'ebrowse-tree-middle                       )
  (local-set-key [?N]      'ebrowse-tree-down-view                    )
  (local-set-key [?P]      'ebrowse-tree-up-view                      )
  (local-set-key [?U]      'ebrowse-mark-all-classes                  )
  (local-set-key [?a]      'ebrowse-tree-member-attributes            )
  (local-set-key [?f]      'ebrowse-toggle-file-name-display          )
  (local-set-key [?i]      'ebrowse-statistics                        )
  (local-set-key [?l]      'ebrowse-tags-list-members-in-file         )
  (local-set-key [?m]      'ebrowse-tree-mark                         )
  (local-set-key [?o]      'ebrowse-tree-command:show-member-functions)
  (local-set-key [?n]      'ebrowse-tree-down                         )
  (local-set-key [?p]      'ebrowse-tree-up                           )
  (local-set-key [?q]      'ebrowse-quit                              )
  (local-set-key [?r]      'ebrowse-tags-apropos                      )
  (local-set-key [?s]      'ebrowse-tags-view-definition-other-window )
  (local-set-key [?t]      'ebrowse-tree-member-type                  )
  (local-set-key [?u]      'ebrowse-tree-mark                         )
  (local-set-key [?v]      'ebrowse-tree-command:show-member-functions)
  (define-key ebrowse-member-mode-map [mouse-1]
              'ebrowse-member-view-declaration)
  (define-key ebrowse-member-mode-map [mouse-2]
              'ebrowse-restore-windows)
  (define-key ebrowse-member-mode-map [mouse-3]
              'ebrowse-member-view-definition)
  (define-key ebrowse-member-mode-map [down-mouse-3]
              'mouse-set-point)
  (define-key ebrowse-member-mode-map [f5]
              'ebrowse-restore-windows)
  (define-key ebrowse-member-mode-map [C-home]
              'ebrowse-member-top)
  (define-key ebrowse-member-mode-map [C-end]
              'ebrowse-member-bottom)
  (define-key ebrowse-member-mode-map [C-S-home]
              'ebrowse-member-top-mark)
  (define-key ebrowse-member-mode-map [C-S-end]
              'region-bol-bottom)
  (define-key ebrowse-member-mode-map [prior]
              'ebrowse-member-up-page)
  (define-key ebrowse-member-mode-map [next]
              'ebrowse-member-down-page)
  (define-key ebrowse-member-mode-map [?\t]
              'ebrowse-pop-from-member-to-tree-buffer)
  (define-key ebrowse-member-mode-map [backtab]
              'ebrowse-pop-from-member-to-tree-buffer)
  (define-key ebrowse-member-mode-map [?\C-m]
              'ebrowse-member-view-declaration) ; return
  (define-key ebrowse-member-mode-map [C-return]
              'ebrowse-member-view-definition)
  (define-key ebrowse-member-mode-map [M-return]
              'ebrowse-member-view-definition)
  (define-key ebrowse-member-mode-map [up]
              'ebrowse-member-up)
  (define-key ebrowse-member-mode-map [down]
              'ebrowse-member-down)
  (define-key ebrowse-member-mode-map [S-up]
              'ebrowse-member-up-mark)
  (define-key ebrowse-member-mode-map [S-down]
              'region-bol-down)
  (define-key ebrowse-member-mode-map [C-up]
              'ebrowse-member-up-3)
  (define-key ebrowse-member-mode-map [C-down]
              'ebrowse-member-down-3)
  (define-key ebrowse-member-mode-map [M-up]
              'ebrowse-member-up-view)
  (define-key ebrowse-member-mode-map [M-down]
              'ebrowse-member-down-view)
  (define-key ebrowse-member-mode-map [left]
              'ebrowse-member-variables)
  (define-key ebrowse-member-mode-map [right]
              'ebrowse-member-functions)
  (define-key ebrowse-member-mode-map [?\C-y]
              'ebrowse-member-up-page)
  (define-key ebrowse-member-mode-map [?\C-v]
              'ebrowse-member-down-page)
  (define-key ebrowse-member-mode-map [?*]
              'ebrowse-toggle-public-member-filter)
  (define-key ebrowse-member-mode-map [?/]
              'ebrowse-goto-visible-member)
  (define-key ebrowse-member-mode-map [?M]
              'ebrowse-member-middle)
  (define-key ebrowse-member-mode-map [?N]
              'ebrowse-member-down-3)
  (define-key ebrowse-member-mode-map [?P]
              'ebrowse-member-up-3)
  (define-key ebrowse-member-mode-map [?a]
              'ebrowse-toggle-member-attributes-display)
  (define-key ebrowse-member-mode-map [?f]
              'ebrowse-member-tree-filenames)
  (define-key ebrowse-member-mode-map [?i]
              'ebrowse-statistics)
  (define-key ebrowse-member-mode-map [?l]
              'ebrowse-tags-list-members-in-file)
  (define-key ebrowse-member-mode-map [?m]
              'ignore)
  (define-key ebrowse-member-mode-map [?o]
              'ebrowse-member-view-declaration)
  (define-key ebrowse-member-mode-map [?n]
              'ebrowse-member-down)
  (define-key ebrowse-member-mode-map [?p]
              'ebrowse-member-up)
  (define-key ebrowse-member-mode-map [?q]
              'ebrowse-restore-windows)
  (define-key ebrowse-member-mode-map [?r]
              'ebrowse-tags-apropos)
  (define-key ebrowse-member-mode-map [?s]
              'ebrowse-tags-view-definition-other-window)
  (define-key ebrowse-member-mode-map [?t]
              'ebrowse-toggle-long-short-display)
  (define-key ebrowse-member-mode-map [?v]
              'ebrowse-member-view-declaration)
  (defun ebrowse-member-bottom ()
    "Move to bottom."
    (interactive)
    (deactivate-mark)
    (goto-char (point-max))
    (ebrowse-member-up 1))
  (defun ebrowse-member-down (&optional n)
    "Move down N lines."
    (interactive "p")
    (deactivate-mark)
    (beginning-of-line (+ 1 n))
    (if (eobp)(forward-line -1))
    (skip-chars-forward " \t<\\->"))
  (defun ebrowse-member-down-3 ()
    "Move down 3 lines."
    (interactive)
    (ebrowse-member-down 3))
  (defun ebrowse-member-down-page (&optional n)
    "Move down N pages."
    (interactive "p")
    (ebrowse-member-down (pages n)))
  (defun ebrowse-member-down-view ()
    "Move down one line and view member declaration in other window."
    (interactive)
    (ebrowse-member-down 1)
    (ebrowse-member-view-declaration))
  (defun ebrowse-member-functions ()
    "Display member functions."
    (interactive)
    (let ((line (line-number-at-pos)))
      (ebrowse-display-function-member-list)
      (goto-line-lisp (- line 1))
      (ebrowse-member-down 1)))
  (defun ebrowse-member-middle ()
    "Move to middle."
    (interactive)
    (ebrowse-member-top)
    (ebrowse-member-down (middle-from-here)))
  (defun ebrowse-member-top ()
    "Move to top."
    (interactive)
    (deactivate-mark)
    (goto-line-lisp 4)
    (ebrowse-member-up 1))
  (defun ebrowse-member-top-mark ()
    "Extend region to top."
    (interactive)
    (region-bol-top 3))
  (defun ebrowse-member-tree-filenames ()
    "Toggle filename display in tree window."
    (interactive)
    (other-window 1)
    (ebrowse-toggle-file-name-display)
    (other-window 1))
  (defun ebrowse-member-up (&optional n)
    "Move up N lines."
    (interactive "p")
    (deactivate-mark)
    (beginning-of-line (- 1 n))
    (if (< (line-number-at-pos) 3)
        (goto-line-lisp 3))
    (skip-chars-forward " \t<\\->"))
  (defun ebrowse-member-up-3 ()
    "Move up 3 lines."
    (interactive)
    (ebrowse-member-up 3))
  (defun ebrowse-member-up-mark (&optional n)
    "Extend region up N lines."
    (interactive "p")
    (region-bol-up n)
    (if (< (line-number-at-pos) 3)
        (ebrowse-member-top-mark)))
  (defun ebrowse-member-up-page (&optional n)
    "Move up N pages."
    (interactive "p")
    (ebrowse-member-up (pages n)))
  (defun ebrowse-member-up-view ()
    "Move up one line and view member declaration in other window."
    (interactive)
    (ebrowse-member-up 1)
    (ebrowse-member-view-declaration))
  (defun ebrowse-member-variables ()
    "Display member variables."
    (interactive)
    (let ((line (line-number-at-pos)))
      (ebrowse-display-variables-member-list)
      (goto-line-lisp (+ line 1))
      (ebrowse-member-up 1)))
  (defun ebrowse-member-view-declaration ()
    "View member declaration in other window."
    (interactive)
    (ebrowse-view-member-declaration 4)
    (kill-buffer (current-buffer))
    (other-window 1)
    (ebrowse-view-member-declaration 4)
    (other-window 1))
  (defun ebrowse-member-view-definition ()
    "View member definition in other window."
    (interactive)
    (ebrowse-view-member-definition 4)
    (kill-buffer (current-buffer))
    (other-window 1)
    (ebrowse-view-member-definition 4)
    (other-window 1))
  (defun ebrowse-mouse-tree-variables (event)
    "View member variables."
    (interactive "e")
    (mouse-set-point event)
    (ebrowse-tree-command:show-member-variables t))
  (defun ebrowse-mouse-tree-declaration (event)
    "View class declaration."
    (interactive "e")
    (mouse-set-point event)
    (ebrowse-tree-view-declaration))
  (defun ebrowse-quit ()
    "Quit ebrowse-tree-mode."
    (interactive)
    (delete-other-windows)
    (kill-buffer ebrowse-tree-buffer-name)
    (kill-buffer ebrowse-member-buffer-name))
  (defun ebrowse-restore-windows ()
    "Restore original window setup: tree|members."
    (interactive)
    (delete-other-windows)
    (split-window-right)
    (switch-to-buffer ebrowse-tree-buffer-name)
    (display-buffer ebrowse-member-buffer-name))
  (defun ebrowse-tree-bottom ()
    "Move to bottom."
    (interactive)
    (deactivate-mark)
    (goto-char (point-max))
    (ebrowse-tree-up 1))
  (defun ebrowse-tree-down (&optional n)
    "Move down N lines."
    (interactive "p")
    (deactivate-mark)
    (beginning-of-line (+ 1 n))
    (if (eobp)(forward-line -1))
    (skip-chars-forward " \t"))
  (defun ebrowse-tree-down-3 ()
    "Move down 3 lines."
    (interactive)
    (ebrowse-tree-down 3))
  (defun ebrowse-tree-down-page (&optional n)
    "Move down N pages."
    (interactive "p")
    (ebrowse-tree-down (pages n)))
  (defun ebrowse-tree-down-view ()
    "Move down one line and view member functions in other window."
    (interactive)
    (ebrowse-tree-down 1)
    (ebrowse-tree-command:show-member-functions))
  (defun ebrowse-tree-mark ()
    "Mark class and move to next line."
    (interactive)
    (ebrowse-toggle-mark-at-point)
    (ebrowse-tree-up 1)
    (ebrowse-tree-down 1))
  (defun ebrowse-tree-member-attributes ()
    "Toggle attribute display in member window."
    (interactive)
    (other-window 1)
    (ebrowse-toggle-member-attributes-display)
    (other-window 1))
  (defun ebrowse-tree-member-type ()
    "Toggle type display in member window."
    (interactive)
    (other-window 1)
    (ebrowse-toggle-long-short-display)
    (other-window 1))
  (defun ebrowse-tree-member-public ()
    "Toggle public display in member window."
    (interactive)
    (other-window 1)
    (ebrowse-toggle-public-member-filter)
    (other-window 1))
  (defun ebrowse-tree-middle ()
    "Move to middle."
    (interactive)
    (ebrowse-tree-top)
    (ebrowse-tree-down (middle-from-here)))
  (defun ebrowse-tree-switch ()
    "Move to member window."
    (interactive)
    (ebrowse-pop/switch-to-member-buffer-for-same-tree nil)
    (if (< (line-number-at-pos) 3)
        (ebrowse-member-top)))
  (defun ebrowse-tree-top ()
    "Move to top."
    (interactive)
    (deactivate-mark)
    (goto-line-lisp 2)
    (ebrowse-tree-up 1))
  (defun ebrowse-tree-up (&optional n)
    "Move down N lines."
    (interactive "p")
    (deactivate-mark)
    (beginning-of-line (- 1 n))
    (skip-chars-forward " \t"))
  (defun ebrowse-tree-up-3 ()
    "Move up 3 lines."
    (interactive)
    (ebrowse-tree-up 3))
  (defun ebrowse-tree-up-page (&optional n)
    "Move up N pages."
    (interactive "p")
    (ebrowse-tree-up (pages n)))
  (defun ebrowse-tree-up-view ()
    "Move up one line and view member functions in other window."
    (interactive)
    (ebrowse-tree-up 1)
    (ebrowse-tree-command:show-member-functions))
  (defun ebrowse-tree-view-declaration ()
    "View class declaration in other window."
    (interactive)
    (ebrowse-view-class-declaration 4)
    (other-window 1)))
(add-hook 'ebrowse-tree-mode-hook 'arni-ebrowse-hook)
(defun arni-gdb-hook ()
  (message nil); leave [?\C-x ?\C-a] unbound, otherwise gdb cannot be started
  (setq indent-line-function 'gud-gdb-complete-command)
  (setq gdb-show-main t)
  (local-set-key [f5]          'gdb-restore-windows)
  (local-set-key [M-up]        'gdb-many-windows   )
  (local-set-key [M-down]      'gdb-many-windows   )
  (local-set-key [?\C-c ?\C-q] 'comint-quit-subjob))
(add-hook 'gdb-mode-hook 'arni-gdb-hook)
(defvar makefile-program-name nil
  "Name of program built using Makefile. See `makefile-run'.")
(defun arni-makefile-hook ()
  (setq indent-line-function 'insert-tab-char-1)
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "brown4")
  (local-unset-key [?\M-n]) ; reactivate bs-cycle-next
  (local-unset-key [?\M-p]) ; reactivate bs-cycle-previous
  (local-set-key [C-f12]             'makefile-template           )
  (local-set-key [?\C-c C-backspace] 'makefile-clean              )
  (local-set-key [?\C-c ?\C-c]       'makefile-build              )
  (local-set-key [?\C-c ?\C-l]       'makefile-clean              )
  (local-set-key [?\C-c ?\C-n]       'makefile-next-dependency    )
  (local-set-key [?\C-c ?\C-p]       'makefile-previous-dependency)
  (local-set-key [?\C-c ?\C-q]       'makefile-kill-compilation   )
  (local-set-key [?\C-c ?\C-v]       'makefile-run                )
  (defun makefile-build ()
    "Build program using Makefile."
    (interactive)
    (save-buffer)
    (if (one-window-p)(split-window-right))
    (compile (concat "make -f " (buffer-name))))
  (defun makefile-clean ()
    "Delete binaries using Makefile."
    (interactive)
    (save-buffer)
    (if (one-window-p)(split-window-right))
    (compile (concat "make -f " (buffer-name) " clean")))
  (defun makefile-kill-compilation ()
    "Kill Makefile build process."
    (interactive)
    (kill-process (car (process-list))))
  (defun makefile-run ()
    "Run program built using Makefile."
    (interactive)
    (let ((max-mini-window-height 0))
      (if (null makefile-program-name)
          (setq makefile-program-name (read-string "Program name: ")))
      (if (and (not (file-regular-p makefile-program-name))
               (not (file-regular-p (concat makefile-program-name ".exe"))))
          (error "File %s not found" makefile-program-name))
      (if (one-window-p)(split-window-right))
      (shell-command (concat default-directory makefile-program-name))))
  (defun makefile-template ()
    "Insert Makefile template."
    (interactive "*")
    (goto-char (point-min))
    (insert "\
# g++ -s -static -Wall -o prog class.cpp prog.cpp
COMP=g++ -c -Wall $^
LINK=g++ -s -static -Wall -o $@ $^

.PHONY: all
all: prog

prog: class.o prog.o
\t$(LINK)

prog.o: prog.cpp
\t$(COMP)

class.o: class.cpp
\t$(COMP)

.PHONY: clean
clean:
\trm prog class.o prog.o
")
    (goto-char (point-min))))
(add-hook 'makefile-mode-hook 'arni-makefile-hook)
(defun doxymacs-mode-with-hook ()
  "Start `doxymacs-mode' and run `doxymacs-mode-hook'."
  (interactive)
  (doxymacs-mode)
  (run-mode-hooks 'doxymacs-mode-hook)
  (redraw-display))
(defun arni-doxymacs-hook ()
  (define-key doxymacs-mode-map [?\C-c ?\C-y ?\C-c]
              'doxymacs-insert-member-comment          )
  (define-key doxymacs-mode-map [?\C-c ?\C-y ?\C-f]
              'doxymacs-insert-function-comment        )
  (define-key doxymacs-mode-map [?\C-c ?\C-y ?\C-g]
              'doxymacs-insert-grouping-comments       )
  (define-key doxymacs-mode-map [?\C-c ?\C-y ?\C-h]
              'doxymacs-insert-file-comment            )
  (define-key doxymacs-mode-map [?\C-c ?\C-y ?\C-l]
              'doxymacs-lookup                         )
  (define-key doxymacs-mode-map [?\C-c ?\C-y ?\C-m]
              'doxymacs-insert-blank-multiline-comment )
  (define-key doxymacs-mode-map [?\C-c ?\C-y ?\C-p]
              'doxymacs-view-pdf                       )
  (define-key doxymacs-mode-map [?\C-c ?\C-y ?\C-r]
              'doxymacs-rescan-tags                    )
  (define-key doxymacs-mode-map [?\C-c ?\C-y ?\C-s]
              'doxymacs-insert-blank-singleline-comment)
  (define-key doxymacs-mode-map [?\C-c ?\C-y ?\C-u]
              'doxymacs-compile-latex                  )
  (define-key doxymacs-mode-map [?\C-c ?\C-y ?\C-v]
              'doxymacs-view-html                      )
  (define-key doxymacs-mode-map [?\C-c ?\C-y ?\C-y]
              'doxymacs-compile                        )
  (defun doxymacs-compile ()
    "Compile Doxygen file in current directory."
    (interactive)
    (save-buffer)
    (if (one-window-p)(split-window-right))
    (compile (concat "doxygen " (doxymacs-file))))
  (defun doxymacs-compile-latex ()
    "Compile Doxygen LaTeX document."
    (interactive)
    (cd "latex")
    (compile "make")
    (cd ".."))
  (defun doxymacs-file (&optional dir)
    "Return name of Doxygen configuration file in DIR."
    (if (null dir)(setq dir "."))
    (cond ((directory-files dir nil "^Doxyfile$") "Doxyfile")
          ((directory-files dir nil "\\.dox$")
           (car (directory-files "." nil "\\.dox$")))
          ((directory-files dir nil "\\.doxyfile$")
           (car (directory-files "." nil "\\.doxyfile$")))
          (t nil)))
  (defun doxymacs-view-html ()
    "View Doxygen HTML document."
    (interactive)
    (browse-url (concat default-directory "html/index.html")))
  (defun doxymacs-view-pdf ()
    "View Doxygen LaTeX document."
    (interactive)
    (shell-command "ghostview latex/refman.pdf&")
    (delete-other-windows)))
(add-hook 'doxymacs-mode-hook 'arni-doxymacs-hook)
(defalias 'flex-mode 'c-mode)
;;----------
;; 6.7  Dos
;;----------
(defun arni-bat-hook ()
  (setq make-backup-files t)
  (setq indent-line-function 'indent-relative-definitely)
  (arni-colors)
  (modify-syntax-entry ?_ "w" bat-mode-syntax-table)
  (modify-syntax-entry ?~ "w" bat-mode-syntax-table)
  (modify-syntax-entry ?{ "w" bat-mode-syntax-table)
  (modify-syntax-entry ?} "w" bat-mode-syntax-table)
  (set-face-attribute 'font-lock-builtin-face
                      nil :foreground (fg 'font-lock-keyword-face)) ; dir, del
  (set-face-attribute 'font-lock-function-name-face
                      nil :foreground "brown4") ; [call, goto]
  (set-face-attribute 'font-lock-type-face
                      nil :foreground "magenta4") ; -arg
  (set-face-attribute 'font-lock-variable-name-face
                      nil :foreground "brown4") ; %x%
  (set-face-attribute 'font-lock-warning-face
                      nil :weight -) ; ls, rm
  (local-set-key [f11]         'bat-outline-remember)
  (local-set-key [C-f12]       'bat-template        )
  (local-set-key [M-f12]       'bat-template-full   )
  (local-set-key [?\C-c ?\C- ] 'bat-sep             )
  (defun bat-outline ()
    "Navigate within Dos script using `outline-mode'."
    (interactive)
    (outline-mode)
    (setq outline-regexp "^:[^:]")
    (outline-mode)
    (outline-hide-body))
  (defun bat-outline-remember ()
    "Navigate within Dos script using `outline-mode',
remembering previous mode."
    (interactive)
    (bat-outline)
    (setq outline-top-level 2)
    (setq outline-previous-mode '(bat-mode)))
  (defun bat-sep ()
    "Insert & separator."
    (interactive "*")(insert " & "))
  (defun bat-template-full ()
    "Insert Dos template."
    (interactive "*")
    (goto-char (point-min))
    (insert "\
@echo off
setlocal
if [%1]==[] goto HELP
if [%1]==[--help] goto HELP
REM ############################################################################
REM                                                                            #
REM Script:                                                                    #
REM                                                                            #
REM Purpose:                                                                   #
REM                                                                            #
REM Args:                                                                      #
REM                                                                            #
REM Notes:                                                                     #
REM                                                                            #
REM Warning:                                                                   #
REM                                                                            #
REM Requires:                                                                  #
REM                                                                            #
REM Returns:                                                                   #
REM                                                                            #
REM ############################################################################

rem Pop args until file=%1
set par=default
:STARTLOOP
if [%2]==[] goto ENDLOOP
if %1==-flag set par=%2 & shift & shift
goto STARTLOOP
:ENDLOOP



:HELP
echo Usage:
echo.

:EOF
")
    (goto-char (point-min))
    (search-forward ":   ")
    (overwrite-mode t)))
(add-hook 'bat-mode-hook 'arni-bat-hook)
;;--------------
;; 6.8  Fortran
;;--------------
(defun arni-fortran-hook ()
  (arni-colors)
  (local-set-key [?\C-m]       'newline-and-indent      ) ; return
  (local-set-key [C-return]    'fortran-split-line-smart)
  (local-set-key [M-return]    'fortran-split-line-smart)
  (local-set-key [?\C-c ?f]    'fortran-or-f90-mode     )
  (local-set-key [?\C-c ?\C-c] 'fortran-compile         )
  (local-set-key [?\C-c ?\C-v] 'fortran-run             )
  (defun fortran-compile ()
    "Build simple Fortran program (one source file)."
    (interactive)
    (save-buffer)
    (if (one-window-p)(split-window-right))
    (compile (concat "gfortran -s -static -Wall -o "
                     (file-name-sans-extension (buffer-name)) " "
                     (buffer-name))))
  (defun fortran-or-f90-mode ()
    "Switch between Fortran and F90 mode."
    (interactive)
    (if (string-match "Fortran" mode-name)
        (f90-mode)
      (fortran-mode)))
  (defun fortran-run ()
    "Run simple Fortran program."
    (interactive)
    (let ((resize-mini-windows nil))
      (if (one-window-p)(split-window-right))
      (shell-command (file-name-sans-extension (buffer-name)))))
  (defun fortran-split-line-smart ()
    "Break Fortran line."
    (interactive)
    (if (string-match "Fortran" mode-name)
        (fortran-split-line)
      (f90-break-line))))
(add-hook 'fortran-mode-hook 'arni-fortran-hook)
(add-hook 'f90-mode-hook 'arni-fortran-hook)
;;-----------
;; 6.9  HTML
;;-----------
(defvar html-links nil
  "Non-nil if links are currently hilighted. See `html-toggle-links'.")
(defun arni-html-hook ()
  ;; Using 2.19, which has better syntax highlighting than 3.0.4kilo.
  ;; Added support for <style>, <div>, <form>, <thead>, <tbody>, {},
  ;; and /* css comments */.
  ;; Made <title> bold. Helped distinguishing between <li> and <link>.
  ;; Prevented <h1> from being colored as `font-lock-function-name-face'.
  ;; Prevented <img> from being colored as `font-lock-variable-name-face'.
  ;; Increased `html-helper-search-limit' from 2000 to 20000.
  (font-lock-mode 1)
  (setq make-backup-files t)
  (setq fill-column 80)
  (setq indent-line-function 'html-helper-indent-command)
  (arni-colors)
  (set-face-attribute 'font-lock-constant-face      nil ; links
                      :foreground "brown4" :underline -       )
  (set-face-attribute 'font-lock-type-face          nil ; <h1>
                      :foreground (fg 'font-lock-keyword-face))
  (set-face-attribute 'font-lock-variable-name-face nil ; <html>
                      :foreground (fg 'font-lock-keyword-face))
  (local-unset-key [?\t]) ; reactivate indent-or-complete
  (local-set-key [f9]             'iso-iso2sgml                    )
  (local-set-key [S-f9]           'iso-sgml2iso                    )
  (local-set-key [f10]            'html-tidy                       )
  (local-set-key [S-f10]          'html-simplify                   )
  (local-set-key [f11]            'html-outline                    )
  (local-set-key [C-f12]          'html-template                   )
  (local-set-key [?\C-m]          'reindent-then-newline-and-indent) ; return
  (local-set-key [M-return]       'html-br                         )
  (local-set-key [?\C-c ?~]       'tempo-template-html-nonbreaking-space)
  (local-set-key [?\C-\" C-left]  'html-quote-6                    )
  (local-set-key [?\C-\" C-right] 'html-quote-9                    )
  (local-set-key [?\C-\" left]    'html-quote-66                   )
  (local-set-key [?\C-\" right]   'html-quote-99                   )
  (local-set-key [?\C-\" down]    'html-quote-99-bottom            )
  (local-set-key [?\C-c ?8]       'html-8bit-char                  )
  (local-set-key [?\C-c ?\C-.]    'html-nbsp                       )
  (local-set-key [?\C-c ?\C- ]    'html-td                         )
  (local-set-key [?\C-c ?\C-8]    'html-8bit-char                  )
  (local-set-key [?\C-c ?\C-a]    'tempo-template-html-hyperlink   )
  (local-set-key [?\C-c ?\C-c]    'html-save-and-view              )
  (local-set-key [?\C-c ?\C-d]    'html-dl                         )
  (local-set-key [?\C-c ?\C-e]    'html-code                       )
  (local-set-key [?\C-c ?\C-h]    'html-toggle-links               )
  (local-set-key [?\C-c ?\C-i]    'html-img                        )
  (local-set-key [?\C-c ?\C-l]    'html-css-link                   )
  (local-set-key [?\C-c ?\C-n]    'tempo-template-html-link-target )
  (local-set-key [?\C-c ?\C-o]    'html-ol                         )
  (local-set-key [?\C-c ?\C-s]    'html-css-inline                 )
  (local-set-key [?\C-c ?\C-t]    'html-table                      )
  (local-set-key [?\C-c ?\C-u]    'html-ul                         )
  (local-set-key [?\C-c ?\C-v]    'browse-url-of-file              )
  (local-set-key [?\C--]          'html-endash                     )
  (local-set-key [?\M--]          'html-emdash                     )
  (local-set-key [?\M-/]          'html-delete-comments            )
  (defun html-8bit-char ()
    "Insert &sgml; based on keyboard input."
    (interactive "*")
    (sgml-mode)
    (sgml-name-char)
    (html-helper-mode))
  (defun html-br ()
    "Insert <br> and end line."
    (interactive "*")
    (insert "<br>")
    (reindent-then-newline-and-indent))
  (defun html-code ()
    "Insert <code> tags around object at point."
    (interactive "*")
    (insert "<code>")
    (re-search-forward "[\n, ]" nil t)
    (backward-char)
    (if (= (char-before) ?.)
        (backward-char))
    (insert "</code>"))
  (defun html-css-inline ()
    "Insert inline CSS block."
    (interactive "*")
    (insert "<style>\n  body {margin:2%; max-width:80ex}\n</style>\n")
    (search-backward "{"))
  (defun html-css-link ()
    "Link external CSS stylesheet."
    (interactive "*")
    (insert "<link rel=\"stylesheet\" href=\"\">\n")
    (search-backward "\""))
  (defun html-delete-comments ()
    "Delete all comments."
    (interactive "*")
    (save-excursion
      (goto-char (point-min))
      ;; Just [\t ]* and then regexp from html-helper-font-lock-keywords
      (while (re-search-forward
              "[\t ]*<!\\(--\\([^-]\\|-[^-]\\)*--\\s-*\\)*>" nil t)
        (replace-match ""))))
  (defun html-dl ()
    "Insert <dl> structure."
    (interactive "*")
    (insert "<dl>")
    (reindent-then-newline-and-indent)
    (insert "<dt>")(reindent-then-newline-and-indent)
    (insert "<dd>")(reindent-then-newline-and-indent)
    (insert "</dl>")
    (html-helper-indent-command)
    (newline)
    (re-search-backward "\n *</dl>"))
  (defun html-emdash ()
    "Insert &mdash;"
    (interactive "*")
    (insert "&mdash;"))
  (defun html-endash ()
    "Insert &ndash;"
    (interactive "*")
    (insert "&ndash;"))
  (defun html-img ()
    "Insert <img> with args."
    (interactive "*")
    (insert "<img src=\"\" alt=\"\">")
    (search-backward "\"" nil t 3))
  (defun html-nbsp ()
    "Insert &nbsp;"
    (interactive "*")
    (insert "&nbsp;"))
  (defun html-ol ()
    "Insert <ol> structure."
    (interactive "*")
    (insert "<ol>")
    (reindent-then-newline-and-indent)
    (insert "<li>")
    (reindent-then-newline-and-indent)
    (insert "</ol>")
    (html-helper-indent-command)
    (newline)
    (re-search-backward "\n *</ol>"))
  (defun html-outline ()
    "Navigate within HTML file using `outline-mode'."
    (interactive "*")
    (outline-mode)
    (setq outline-regexp ".*<h[0-9]\\|.*<div\\|.*<dl\\|.*<table")
    (outline-mode)
    (outline-hide-body)
    (setq outline-top-level 3)
    (setq outline-previous-mode '(html-helper-mode)))
  (defun html-quote-6 ()
    "Insert &lsquo;"
    (interactive "*")
    (insert "&lsquo;"))
  (defun html-quote-9 ()
    "Insert &rsquo;"
    (interactive "*")
    (insert "&rsquo;"))
  (defun html-quote-66 ()
    "Insert &ldquo;"
    (interactive "*")
    (insert "&ldquo;"))
  (defun html-quote-99 ()
    "Insert &rdquo;"
    (interactive "*")
    (insert "&rdquo;"))
  (defun html-quote-99-bottom ()
    "Insert &bdquo;"
    (interactive "*")
    (insert "&bdquo;"))
  (defun html-save-and-view ()
    "Save file and open in browser."
    (interactive)
    (save-buffer)
    (browse-url-of-file))
  (defun html-simplify ()
    "Simplify HTML code by removing unnecessary tags."
    (interactive "*")
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "<html>\n" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "</html>\n" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "<head>\n" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "</head>\n" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "</body>\n" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "</p>" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "</li>" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "</dt>" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "</dd>" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "<thead>\n" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "</thead>\n" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "<tbody>\n" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "</tbody>\n" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "</tr>" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "</th>" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "</td>" nil t)(replace-match ""))
      (goto-char (point-min))
      (while (search-forward "\n\n\n" nil t)(replace-match "\n\n"))
      (goto-char (- (point-max) 1))
      (if (zerop (current-column))
          (delete-char 1))))
  (defun html-table ()
    "Insert <table> structure."
    (interactive "*")
    (insert "<table>")
    (reindent-then-newline-and-indent)
    (insert "<tr><td>")
    (reindent-then-newline-and-indent)
    (insert "</table>")
    (html-helper-indent-command)
    (newline)
    (re-search-backward "\n *</table>"))
  (defun html-td ()
    "Insert <td> separator."
    (interactive "*")
    (insert "<td>"))
  (defun html-template ()
    "Insert HTML template."
    (interactive "*")
    (goto-char (point-min))
    (insert "\
<!DOCTYPE html>
<meta charset=\"utf-8\">
<title></title>
<style>
  body {margin:2%; max-width:80ex}
</style>
<body>

")
    (goto-char (point-min))
    (search-forward "e>"))
  (defun html-tidy ()
    "Validate current HTML file with tidy."
    (interactive)
    (compile (concat "tidy -e -omit -utf8 '" (buffer-name) "'")))
  (defun html-toggle-links ()
    "Toggle highlighting of links (red:href, yellow:most, green:some)."
    (interactive)
    (if html-links (hi-lock-mode 0)
      (progn (hi-lock-face-buffer " href=\"[^\"]*\\.html\""
                                  'highlight     ) ; html
             (hi-lock-face-buffer " href=\"[^\"]*\\.htm\""
                                  'highlight     ) ; htm
             (hi-lock-face-buffer " href=\"[^\"]*\\.css\""
                                  'highlight     ) ; css
             (hi-lock-face-buffer " href=\"[^\"]*\\.pdf\""
                                  'highlight     ) ; pdf
             (hi-lock-face-buffer " href=\"[^\"]*\\.zip\""
                                  'highlight     ) ; zip
             (hi-lock-face-buffer " href=\"mailto:[^\"]*\""
                                  'highlight     ) ; mailto
             (hi-lock-face-buffer " href=\"[^\"]*/\""
                                  'highlight     ) ; /
             (hi-lock-face-buffer " href=\"[^\"]*#[^\"]*\""
                                  'highlight     ) ; #
             (hi-lock-face-buffer " href=\"http[^\"]*[\?][^\"]*\""
                                  'lazy-highlight) ; ?
             (hi-lock-face-buffer " href=" 'show-paren-mismatch-face)))
    (setq html-links (not html-links))
    (force-mode-line-update)
    (message "Highlighted links %s" (if html-links "ON" "OFF")))
  (defun html-ul ()
    "Insert <ul> structure."
    (interactive "*")
    (insert "<ul>")
    (reindent-then-newline-and-indent)
    (insert "<li>")
    (reindent-then-newline-and-indent)
    (insert "</ul>")
    (html-helper-indent-command)
    (newline)
    (re-search-backward "\n *</ul>")))
(add-hook 'html-helper-mode-hook 'arni-html-hook)
(defun arni-html-load-hook ()
  (setq html-helper-build-new-buffer nil)
  (setq html-helper-do-write-file-hooks nil))
(add-hook 'html-helper-load-hook 'arni-html-load-hook)
(defun arni-css-hook ()
  (set-face-attribute 'css-property nil :inherit font-lock-keyword-face))
(add-hook 'css-mode-hook 'arni-css-hook)
(defun arni-nxml-hook ()
  (font-lock-mode 1)
  (set-face-attribute 'nxml-element-local-name nil
                      :inherit font-lock-keyword-face))
(add-hook 'nxml-mode-hook 'arni-nxml-hook)
(defun arni-sgml-hook ()
  (setq indent-line-function 'sgml-indent-line)
  (font-lock-mode 1)
  (set-face-attribute 'font-lock-function-name-face nil
                      :foreground (fg 'font-lock-keyword-face)
                      :weight -) ; <xml>
  (local-unset-key [?\t]))
(add-hook 'sgml-mode-hook 'arni-sgml-hook)
;;-----------
;; 6.10 Inno
;;-----------
(defun arni-iss-hook ()
  (setq iss-compiler-path "c:/gnu/inno/")
  (local-set-key [?\C-c ?\C-c] 'iss-compile))
(add-hook 'iss-mode-hook 'arni-iss-hook)
;;-----------
;; 6.11 Java
;;-----------
(defun arni-java-hook ()
  (setq c-basic-offset 2)
  (local-set-key [?\C-c ?\C-c] 'java-compile)
  (local-set-key [?\C-c ?\C-v] 'java-run    )
  (defun java-compile ()
    "Build simple Java program (one source file)."
    (interactive)
    (save-buffer)
    (if (one-window-p)(split-window-right))
    (compile (concat "javac " (buffer-name))))
  (defun java-run ()
    "Run simple Java program (first class defined in source)."
    (interactive)
    (let ((resize-mini-windows nil)
          (java-class nil)) ; will look for java-class in source code
      (save-excursion
        (goto-char (point-min))
        (search-forward "class " nil t)
        (setq java-class (current-word)))
      (if (one-window-p)(split-window-right))
      (shell-command (concat "java " java-class)))))
(add-hook 'java-mode-hook 'arni-java-hook)
;;-----------------
;; 6.12 JavaScript
;;-----------------
(defun arni-js-hook ()
  (setq js-indent-level 2)
  (local-unset-key [?\M-.])) ; reactivate dot-emacs-edit
(add-hook 'js-mode-hook 'arni-js-hook)
;;------------
;; 6.13 LaTeX
;;------------
;; Case-sensitive hooks, so use LaTeX everywhere
(defvar LaTeX-icelandic-quotes nil
  "Non-nil if LaTeX quotation marks are ,,Icelandic`` style.
See `LaTeX-toggle-quotes'.")
(defun arni-bibtex-hook ()
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "brown4")
  (local-unset-key [?\t]        ) ; reactivate indent-or-complete
  (local-unset-key [?\C-\M-a]   ) ; reactivate goto-non-ascii
  (local-unset-key [?\C-\M-e]   ) ; reactivate query-replace-regexp
  (local-unset-key [?\C-c ?\C-c]) ; bibtex-clean-entry
  (local-set-key [?\C-c ?\C-n] 'bibtex-next-entry    ) ; bibtex-pop-next
  (local-set-key [?\C-c ?\C-p] 'bibtex-previous-entry) ; bibtex-pop-previous
  (local-set-key [?\C-o] 'bibtex-open-line) ; `open-line' is erratic in BibTeX
  (defun bibtex-open-line ()
    (interactive "*")
    (insert "\n")
    (backward-char)))
(add-hook 'bibtex-mode-hook 'arni-bibtex-hook)
(defun arni-LaTeX-hook ()
  (defvar auctex-version AUCTeX-version
    "Alias for AUCTeX-version.")
  (setq make-backup-files t)
  (setq indent-line-function 'LaTeX-indent-line)
  (reftex-mode 1)
  (message nil)
  (font-lock-mode 1)
  (auto-fill-mode 1) ; load reftex-toc-map
  (save-excursion (if (re-search-forward "SCreport" 1000 t)
                      (auto-fill-mode -1)))
  ;; Beamer and lstlisting indentation
  (setq LaTeX-verbatim-regexp "\\(semi\\)verbatim\\*?\\|lstlisting")
  (setq TeX-clean-confirm nil)           ; remove temp files without asking
  (setq TeX-command-default "LaTeX PDF") ; pdftex is default compiler
  (setq TeX-command-Show "View")         ; ghostview is default viewer
  ;; Don't report under- and overfull boxes (C-c C-w to toggle)
  (setq TeX-debug-bad-boxes nil)
  (setq LaTeX-item-indent 0)           ; indent list items
  (setq TeX-save-query nil)            ; save without asking
  (setq font-latex-fontify-script nil) ; suppress tiny super and subscript
  (arni-colors)
  (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.vrb") ; Beamer clean .vrb
  (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.fdb_latexmk") ; latexmk
  (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
  (font-latex-set-syntactic-keywords) ; highlight
  (add-to-list 'LaTeX-verbatim-environments-local "semiverbatim")
  (font-latex-set-syntactic-keywords) ; Beamer highlight
  (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
  (font-latex-set-syntactic-keywords) ; highlight
  (add-to-list 'TeX-command-list
               '("CompilePDF" "auctex -c -p letter %t" TeX-run-LaTeX nil t))
  (add-to-list 'TeX-command-list
               '("CompilePDFa4" "auctex -c -p a4 %t" TeX-run-LaTeX nil t))
  (add-to-list 'TeX-command-list
               '("LaTeX PDF" "pdflatex %t" TeX-run-LaTeX nil t))
  ;; "View" is defined twice, but works
  (add-to-list 'TeX-command-list
               '("View" "auctex -v %t" TeX-run-discard nil t))
  (set-face-attribute 'font-latex-bold-face   nil :foreground -) ; green
  (set-face-attribute 'font-latex-italic-face nil :foreground -) ; green
  (set-face-attribute 'font-latex-math-face
                      nil :foreground "brown4") ; saddlebrown
  (set-face-attribute 'font-latex-sectioning-0-face nil :height -) ; big
  (set-face-attribute 'font-latex-sectioning-1-face nil :height -) ; big
  (set-face-attribute 'font-latex-sectioning-2-face nil :height -) ; big
  (set-face-attribute 'font-latex-sectioning-3-face nil :height -) ; big
  (set-face-attribute 'font-latex-sectioning-4-face nil :height -) ; big
  (set-face-attribute 'font-latex-sectioning-5-face
                      nil :foreground - :inherit -) ; blue helvetica
  (set-face-attribute 'font-latex-sedate-face
                      nil :foreground - :inherit font-lock-keyword-face) ; gray
  (set-face-attribute 'font-latex-string-face
                      nil :foreground - :inherit font-lock-string-face ) ; rosy
  (set-face-attribute 'font-latex-verbatim-face nil :foreground -
                      :inherit font-lock-string-face ) ; brown courier
  (set-face-attribute 'font-latex-warning-face nil :inherit -) ; bold
  (set-face-attribute 'font-lock-function-name-face nil
                      :weight -) ; {table}, {1mm}
  (set-face-attribute 'font-lock-type-face nil :foreground -) ; {text args}
  (local-unset-key [?\t]     ) ; reactivate indent-or-complete
  (local-unset-key [?\C-\M-a]) ; reactivate copy-buffer
  (local-unset-key [?\C-\M-e]) ; reactivate query-replace-regexp
  (local-unset-key [?\C-c ?.]) ; reactivate comment-line-or-region
  (local-set-key [f9]          'iso-tex2iso             )
  (local-set-key [f10]         'iso-iso2tex             )
  (local-set-key [f11]         'reftex-toc-fullscreen   )
  (local-set-key [S-f11]       'reftex-toc-left-right   )
  (local-set-key [C-f12]       'LaTeX-template-mini     )
  (local-set-key [M-f12]       'LaTeX-template          )
  (local-set-key [?\C-m]       'newline-and-indent      ) ; return
  (local-set-key [M-return]    'LaTeX-item              )
  (local-set-key [?\C-c C-backspace] 'LaTeX-master-clean)
  (local-set-key [?\C-c ?\C- ] 'LaTeX-table-sep         )
  (local-set-key [?\C-c ?\C--] 'LaTeX-break-single      )
  (local-set-key [?\C-c ?\C-=] 'LaTeX-eqnarray-equals   )
  (local-set-key [?\C-c ?\C-'] 'LaTeX-toggle-quotes     )
  (local-set-key [?\C-c ?\C-,] 'TeX-previous-error      ) ; defunct
  (local-set-key [?\C-c ?\C-.] 'TeX-next-error          )
  (local-set-key [?\C-c ?\C-a] 'LaTeX-master-PDFa4      )
  (local-set-key [?\C-c ?\C-b] 'LaTeX-babel-fontenc     ) ; TeX-command-buffer
  (local-set-key [?\C-c ?\C-c] 'LaTeX-master-latex      ) ; TeX-command-master
  (local-set-key [?\C-c ?\C-d] 'LaTeX-knitr             ) ; TeX-save-document
  (local-set-key [?\C-c ?\C-h] 'LaTeX-master-clean      ) ; C-c C-BKSP terminal
  (local-set-key [?\C-c ?\C-i] 'LaTeX-includegraphics   ) ; TeX-goto-info-page
  (local-set-key [?\C-c ?\C-m] 'LaTeX-master-open       ) ; TeX-insert-macro
  (local-set-key [?\C-c ?\C-n] 'LaTeX-noindent          ) ; TeX-normal-mode
  (local-set-key [?\C-c ?\C-o] 'LaTeX-master-PDF        )
  ;; `TeX-kill-job' doesn't stop Rnw sweaving, so bind to `kill-process-now'
  (local-set-key [?\C-c ?\C-q] 'kill-process-now        ) ; [map]
  (local-set-key [?\C-c ?\C-r] 'LaTeX-R-block           ) ; TeX-command-region
  (local-set-key [?\C-c ?\C-u] 'LaTeX-usepackage        )
  (local-set-key [?\C-c ?\C-v] 'LaTeX-ghostview         ) ; TeX-view
  (local-set-key [?\C-c ?\C-x] 'TeX-command-master      )
  (local-set-key [?\C-c ?\C-z] 'sweave-compile          )
  (local-set-key [3 67109110]  'LaTeX-break-single      ) ; ?\C-c ?\C-ö
  (local-set-key [?\C-j]       'LaTeX-fill-paragraph-forward)
  (local-set-key [?\C-u]       'universal-argument      ) ; enable C-u C-c C-e
  (defun auctex-version ()
    "Show AUCTeX version."
    (interactive)
    (message AUCTeX-version))
  (defun beamer-template ()
    "Insert Beamer template."
    (interactive "*")
    (goto-char (point-min))
    (insert "\
% \\documentclass{article}\\usepackage{beamerarticle}
\\documentclass[ignorenonframetext,hyperref={bookmarks=false}]{beamer}
\\mode<article>
{
  \\usepackage{parskip}
}
\\usetheme{warsaw}
\\setbeamertemplate{footline}{}
\\setbeamertemplate{navigation symbols}{}
\\title{}
\\subtitle{}
\\author{}
\\date{}
\\begin{document}

\\begin{frame}
  \\mode<article>{\\maketitle}
  \\mode<beamer>{\\titlepage}
\\end{frame}

\\end{document}
")
    (goto-char (point-min))
    (search-forward "title{"))
  (defun LaTeX-babel-fontenc ()
    "Insert lines to load babel and fontenc packages."
    (interactive "*")
    (insert "\
\\usepackage[icelandic]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
"))
  (defun LaTeX-break-double ()
    "Insert visual section break at end of line, like % ====="
    (interactive "*")
    (end-of-line)
    (insert "% ")
    (while (< (current-column) fill-column)
      (insert "=")))
  (defun LaTeX-break-single ()
    "Insert visual section break at end of line, like % _____"
    (interactive "*")
    (end-of-line)
    (insert "% ")
    (while (< (current-column) fill-column)
      (insert "_")))
  (defun LaTeX-eqnarray-equals ()
    "Insert eqnarray &=& separator."
    (interactive "*")
    (insert " &=& "))
  (defun LaTeX-fill-paragraph-forward ()
    "Justify and go to end of paragraph."
    (interactive)
    (while (and (bolp)(eolp)(not (eobp)))
      (forward-line)) ; move to next non-empty line
    (LaTeX-fill-paragraph nil)
    (clean-trails)
    (message nil)
    (forward-paragraph))
  (defun LaTeX-ghostview ()
    "Open PDF in viewer."
    (interactive)
    (shell-command (concat "ghostview \""
                           (file-name-sans-extension (buffer-name)) ".pdf\"&"))
    (delete-other-windows))
  (defun LaTeX-includegraphics ()
    "Insert \\includegraphics{}."
    (interactive "*")
    (insert "\\includegraphics{}")
    (search-backward "}"))
  (defun LaTeX-item ()
    "Insert \\item, possibly after a newline.
See also `LaTeX-insert-item`."
    (interactive "*")
    (if (not (looking-back "^[ \t]*" 10))
        (newline-and-indent))
    (insert "\\item "))
  (defun LaTeX-knitr ()
    "Knit document and compile to PDF."
    (interactive)
    (save-buffer)
    (compile (concat "auctex -k " (buffer-name))))
  (defun LaTeX-master-clean ()
    "Clean LaTeX temporary files."
    (interactive)
    (save-buffer)
    (TeX-command "Clean" 'TeX-master-file)
    (message "Removed temporary files"))
  (defun LaTeX-master-clean-all ()
    "Clean LaTeX temporary and output files."
    (interactive)
    (save-buffer)
    (TeX-command "Clean All" 'TeX-master-file)
    (message "Removed temporary and output files"))
  (defun LaTeX-master-latex ()
    "Compile LaTeX document with pdfTeX."
    (interactive)
    (save-buffer)
    (TeX-command "LaTeX PDF" 'TeX-master-file))
  (defun LaTeX-master-open ()
    "Open LaTeX master document."
    (interactive)
    (find-file TeX-master))
  (defun LaTeX-master-PDF ()
    "Compile LaTeX document with LaTeX, dvips, and ps2pdf (letter page)."
    (interactive)
    (save-buffer)
    (TeX-command "CompilePDF" 'TeX-master-file))
  (defun LaTeX-master-PDFa4 ()
    "Compile LaTeX document with LaTeX, dvips, and ps2pdf (A4 page)."
    (interactive)
    (save-buffer)
    (TeX-command "CompilePDFa4" 'TeX-master-file))
  (defun LaTeX-master-view ()
    "View LaTeX document with PDF viewer."
    (interactive)
    (TeX-command "View" 'TeX-master-file))
  (defun LaTeX-noindent ()
    "Insert \\noindent."
    (interactive "*")
    (insert "\\noindent\n"))
  (defun LaTeX-table-sep ()
    "Insert tabular & separator."
    (interactive "*")
    (insert " & "))
  (defun LaTeX-template ()
    "Insert LaTeX template."
    (interactive "*")
    (goto-char (point-min))
    (insert "\
\\documentclass[fleqn]{article}
\\usepackage{datetime}\
\\newdateformat{mydate}{\\THEDAY~\\monthname~\\THEYEAR}\\mydate
\\usepackage[a4paper,height=23cm]{geometry}
\\usepackage{parskip}
\\begin{document}

\\title{}
\\author{Arni Magnusson}
\\maketitle



\\end{document}
")
    (goto-char (point-min))
    (search-forward "title{"))
  (defun LaTeX-template-mini ()
    "Insert minimal LaTeX template."
    (interactive "*")
    (goto-char (point-min))
    (insert "\
\\documentclass{article}
\\begin{document}



\\end{document}
")
    (goto-char (point-min))
    (search-forward "\n\n"))
  (defun LaTeX-toggle-quotes ()
    "Toggle Icelandic quotation marks."
    (interactive)
    (if LaTeX-icelandic-quotes
        (progn (setq TeX-open-quote "``")
               (setq TeX-close-quote "''"))
      (progn (setq TeX-open-quote "\\quotedblbase{}")
             (setq TeX-close-quote "``")))
    (setq LaTeX-icelandic-quotes (not LaTeX-icelandic-quotes))
    (message "Icelandic quotes %s" (if LaTeX-icelandic-quotes "ON" "OFF")))
  (defun LaTeX-usepackage ()
    "Insert an empty \\usepackage{}."
    (interactive "*")
    (if (< (line-beginning-position)(line-end-position))
        (open-line 1))
    (insert "\\usepackage{}")
    (search-backward "}"))
  (defun reftex-toc-left-right ()
    "Show RefTeX table of contents in left window."
    (interactive)
    (reftex-toc)
    (split-window-left-right))
  (defun reftex-toc-fullscreen ()
    "Show RefTeX table of contents in full screen."
    (interactive)
    (reftex-toc)
    (delete-other-windows))
  (defun sweave-compile ()
    "Sweave document and compile to PDF."
    (interactive)
    (save-buffer)
    (compile (concat "auctex -s " (buffer-name))))
  (defun sweave-block ()
    "Insert <<>>= R block in a Sweave document."
    (interactive "*")
    (if (not (= (char-before (- (point) 1)) ?\n))(insert "\n"))
    (insert "<<>>=\n\n@")
    (if (not (= (char-after) ?\n))
        (insert "\n"))
    (if (not (= (char-after (+ (point) 1)) ?\n))
        (insert "\n"))
    (search-backward ">>"))
  (defun sweave-template-mini ()
    "Insert minimal Sweave template."
    (interactive "*")
    (goto-char (point-min))
    (insert "\
\\documentclass{article}
\\begin{document}

<<>>=
2+2
@

\\end{document}
")
    (goto-char (point-min))
    (search-forward "\n\n")))
(add-hook 'LaTeX-mode-hook 'arni-LaTeX-hook)
(defun arni-reftex-hook ()
  ;; Reactivate highlight-changes-mode and diff-this-buffer-with-file
  (define-key reftex-mode-map [?\C-c ?-] nil)   ; highlight-changes-mode
  (define-key reftex-mode-map [?\C-c ?=] nil)   ; diff-this-buffer-with-file
  (define-key reftex-mode-map [?\C-c ?\\] nil)) ; avoid reftex-index-phrase-s...
(add-hook 'reftex-mode-hook 'arni-reftex-hook)
(defun arni-reftex-toc-hook ()
  (local-set-key [f5]       'reftex-toc-Rescan          )
  (local-set-key [C-home]   'reftex-toc-top             )
  (local-set-key [C-end]    'reftex-toc-bottom          )
  (local-set-key [C-S-home] 'reftex-toc-top-mark        )
  (local-set-key [C-S-end]  'reftex-toc-bottom-mark     )
  (local-set-key [prior]    'reftex-toc-up-page         )
  (local-set-key [next]     'reftex-toc-down-page       )
  (local-set-key [left]     'beginning-of-line          )
  (local-set-key [right]    'reftex-toc-view-line       )
  (local-set-key [S-left]   'beginning-of-line          )
  (local-set-key [S-right]  'reftex-toc-view-line       )
  (local-set-key [C-left]   'beginning-of-line          )
  (local-set-key [C-right]  'reftex-toc-view-line       )
  (local-set-key [up]       'reftex-toc-up              )
  (local-set-key [down]     'reftex-toc-down            )
  (local-set-key [C-up]     'reftex-toc-previous-heading)
  (local-set-key [C-down]   'reftex-toc-next-heading    )
  (local-set-key [M-up]     'reftex-toc-up-view         )
  (local-set-key [M-down]   'reftex-toc-down-view       )
  (local-set-key [?\C-n]    'reftex-toc-down            )
  (local-set-key [?\C-p]    'reftex-toc-up              )
  (local-set-key [?\C-v]    'reftex-toc-down-page       )
  (local-set-key [?\C-y]    'reftex-toc-up-page         )
  (local-set-key [?/]       'isearch-forward            )
  (local-set-key [?M]       'reftex-toc-middle          )
  (local-set-key [?N]       'reftex-toc-down-view       )
  (local-set-key [?P]       'reftex-toc-up-view         )
  (local-set-key [?e]       'reftex-toc-rename-label    )
  (local-set-key [?j]       'reftex-toc-jump            )
  (local-set-key [?m]       'reftex-toc-middle          )
  (local-set-key [?n]       'reftex-toc-down            )
  (local-set-key [?p]       'reftex-toc-up              )
  (defun reftex-toc-bottom ()
    "Move to bottom."
    (interactive)
    (deactivate-mark)
    (goto-char (point-max))
    (reftex-toc-up 1))
  (defun reftex-toc-bottom-mark ()
    "Extend region to bottom."
    (interactive)
    (region-bol-bottom))
  (defun reftex-toc-down (&optional n)
    "Move down N lines."
    (interactive "p")
    (deactivate-mark)
    (beginning-of-line (+ 1 n))
    (if (eobp)(forward-line -1)))
  (defun reftex-toc-down-page (&optional n)
    "Move down N pages."
    (interactive "p")
    (reftex-toc-down (pages n)))
  (defun reftex-toc-down-view ()
    "Move down one line and view in other window."
    (interactive)
    (reftex-toc-down 1)
    (reftex-toc-view-line))
  (defun reftex-toc-middle ()
    "Move to middle."
    (interactive)
    (jump-middle)
    (reftex-toc-down)
    (reftex-toc-view-line))
  (defun reftex-toc-top ()
    "Move to top."
    (interactive)
    (deactivate-mark)
    (goto-line-lisp 4))
  (defun reftex-toc-top-mark ()
    "Extend region to top."
    (interactive)
    (region-bol-top 4))
  (defun reftex-toc-up (&optional n)
    "Move up N lines."
    (interactive "p")
    (deactivate-mark)
    (beginning-of-line (- 1 n))
    (if (< (line-number-at-pos) 4)
        (reftex-toc-top)))
  (defun reftex-toc-up-page (&optional n)
    "Move up N pages."
    (interactive "p")
    (reftex-toc-up (pages n)))
  (defun reftex-toc-up-view ()
    "Move up one line and view in other window."
    (interactive)
    (reftex-toc-up 1)
    (reftex-toc-view-line)))
(add-hook 'reftex-toc-mode-hook 'arni-reftex-toc-hook)
(defun arni-texinfo-hook ()
  (setq make-backup-files t)
  (font-lock-mode 1)
  (local-set-key [M-return]    'texinfo-insert-@item-properly )
  (local-set-key [f11]         'texinfo-show-structure        )
  (local-set-key [S-f11]       'texinfo-show-structure-nodes  )
  (local-set-key [C-f12]       'texinfo-template-mini         )
  (local-set-key [M-f12]       'texinfo-template              )
  (local-set-key [?\C-c ?\]]   'texinfo-insert-@end           )
  (local-set-key [?\C-c ?\C- ] 'texinfo-insert-@tab           )
  (local-set-key [?\C-c ?\C-a] 'texinfo-insert-@arrow         )
  (local-set-key [?\C-c ?\C-c] 'texinfo-texi2dvi-pdf          )
  (local-set-key [?\C-c ?\C-b] 'texinfo-header-icelandic      )
  (local-set-key [?\C-c ?\C-e] 'texinfo-insert-block          )
  (local-set-key [?\C-c ?\C-f] 'texinfo-insert-@file          )
  (local-set-key [?\C-c ?\C-i] 'texinfo-insert-@image         )
  (local-set-key [?\C-c ?\C-k] 'texinfo-kill-compilation      )
  (local-set-key [?\C-c ?\C-m] 'texinfo-all-menus-update-clean)
  (local-set-key [?\C-c ?\C-n] 'texinfo-insert-@node-plain    )
  (local-set-key [?\C-c ?\C-o] 'texinfo-insert-@code          )
  (local-set-key [?\C-c ?\C-q] 'texinfo-kill-compilation      )
  (local-set-key [?\C-c ?\C-s] 'texinfo-insert-@section       )
  (local-set-key [?\C-c ?\C-t] 'texinfo-master-menu           )
  (local-set-key [?\C-c ?\C-u] 'texinfo-insert-@subsection    )
  (local-set-key [?\C-c ?\C-v] 'texinfo-ghostview             )
  (local-set-key [?\C-c ?\C-x] 'makeinfo-buffer-html          )
  (local-set-key [?\C-c ?\C-z] 'makeinfo-buffer-info          )
  (defun makeinfo-buffer-html ()
    "Compile Texinfo document to HTML."
    (interactive)
    (save-buffer)
    (compile (concat "makeinfo --html --no-split " (buffer-name))))
  (defun makeinfo-buffer-info ()
    "Compile Texinfo document to Info."
    (interactive)
    (save-buffer)
    (makeinfo-buffer))
  (defun texinfo-all-menus-update-clean ()
    "Update all menus and clean whitespace trails."
    (interactive)
    (save-excursion
      (texinfo-all-menus-update)
      (clean-trails)
      (message "Done...updated all the menus.  You may save the buffer.")))
  (defun texinfo-ghostview ()
    "Open PDF in viewer."
    (interactive)
    (shell-command (concat "ghostview "
                           (file-name-sans-extension (buffer-name)) ".pdf&"))
    (delete-other-windows))
  (defun texinfo-header-icelandic ()
    "Insert lines to support Icelandic."
    (interactive "*")
    (insert "@documentlanguage is\n" "@documentencoding UTF-8\n"))
  (defun texinfo-insert-@arrow ()
    "Insert @arrow."
    (interactive "*")
    (insert "@arrow{} "))
  (defun texinfo-insert-@image ()
    "Insert @image{}."
    (interactive "*")
    (insert "@image{}")
    (backward-char))
  (defun texinfo-insert-@item-properly ()
    "Insert @item, possibly after a newline."
    (interactive "*")
    (if (not (looking-back "^[ \t]*" 10))
        (newline))
    (insert "@item "))
  (defun texinfo-insert-@node-plain ()
    "Insert @node."
    (interactive "*")
    (insert "@node "))
  (defun texinfo-insert-@tab ()
    "Insert @tab."
    (interactive "*")
    (if (not (= (char-before) #x20))
        (insert " "))
    (insert "@tab "))
  (defun texinfo-insert-node-lines-all ()
    "Insert missing @node lines with section titles."
    (interactive "*")
    (save-excursion
      (texinfo-insert-node-lines (point-min)(point-max) t)
      (if (zerop (how-many "@node Top" (point-min)(point-max)))
          (progn (goto-char (point-min))
                 (search-forward "@top")
                 (forward-line 0)
                 (insert "@node Top\n")))))
  (defun texinfo-insert-@section ()
    "Insert @section."
    (interactive "*")
    (insert "@section "))
  (defun texinfo-insert-@subsection ()
    "Insert @subsection."
    (interactive "*")
    (insert "@subsection "))
  (defun texinfo-kill-compilation ()
    "Kill Texinfo compilation process."
    (interactive)
    (kill-process (car (process-list))))
  (defun texinfo-template ()
    "Insert Texinfo template."
    (interactive "*")
    (goto-char (point-min))
    (insert "\
\\input texinfo
@c %**start of header
@setfilename template.info
@settitle Title (HTML)
@set VERSION 1.0
@c %**end of header

@copying
License.
@end copying

@titlepage
@title Title (PDF)
@subtitle Subtitle
@subtitle Version @value{VERSION}
@author Author
@page
@vskip 0pt plus 1filll
@insertcopying
@end titlepage

@contents

@ifnottex
@node Top
@top Title (Info)
Version @value{VERSION}
@end ifnottex

@menu
* First::
* Index::
@end menu

@node    First
@chapter First

@cindex concept
This is first.

@node       Index
@unnumbered Index

@printindex cp

@bye
")
    (goto-char (point-min)))
  (defun texinfo-template-mini ()
    "Insert Texinfo template for PDF output only."
    (interactive "*")
    (goto-char (point-min))
    (insert "\
\\input texinfo
@c %**start of header
@setfilename template.info
@settitle Title
@set VERSION 1.0
@c %**end of header

@copying
License.
@end copying

@titlepage
@title Title
@subtitle Subtitle
@subtitle Version @value{VERSION}
@author Author
@page
@vskip 0pt plus 1filll
@insertcopying
@end titlepage

@contents

@chapter First

@cindex concept
This is first.

@unnumbered Index

@printindex cp

@bye
")
    (goto-char (point-min)))
  (defun texinfo-show-structure-nodes ()
    "Show structure of Texinfo document, including nodes."
    (interactive)
    (texinfo-show-structure t))
  (defun texinfo-texi2dvi-pdf ()
    "Compile Texinfo document to PDF."
    (interactive)
    (save-buffer)
    (if (one-window-p)(split-window-right))
    (compile (concat "texi2dvi -c -p -t @finalout " (buffer-name)))
    (maximize-window-top)))
(add-hook 'texinfo-mode-hook 'arni-texinfo-hook)
;;-----------
;; 6.14 Lisp
;;-----------
(defun arni-lisp-hook ()
  (setq make-backup-files t)
  (setq comment-column 0)
  (arni-colors)
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "magenta2")
  (setq paragraph-start ";\\|[ \t]*$")
  (local-set-key [f9]    'eval-line-and-step      )
  (local-set-key [f11]   'dot-emacs-outline       )
  (local-set-key [C-f12] 'lisp-template           )
  (local-set-key [?\C-j] 'region-forward-paragraph) ; fill-paragraph-forward
  (local-set-key [?\C-m] 'newline-and-indent      ) ; return
  (local-set-key [?\C-c ?\C- ] 'lisp-comment-inline   )
  (local-set-key [?\C-c ?\C-b] 'byte-compile-file     )
  (local-set-key [?\C-c ?\C-c] 'eval-buffer-update    )
  ;; Suppress warnings about free variables
  (local-set-key [?\C-c ?\C-e] 'byte-compile-dot-emacs)
  (local-set-key [?\C-c ?\C-j] 'eval-line             ) ; like ess
  (local-set-key [?\C-c ?\C-l] 'eval-line             ) ; intuitive
  (local-set-key [?\C-c ?\C-n] 'eval-line-and-step    )
  (local-set-key [?\C-c ?\C-r] 'eval-region           )
  (defun byte-compile-dot-emacs ()
    "Compile .emacs, to check warnings other than free variables."
    (interactive)
    (require 'bytecomp)
    (setq byte-compile-warnings '(not free-vars))
    (setq byte-compile-dest-file-function
          (lambda (arg)(concat temporary-file-directory ".emacs.elc")))
    (byte-compile-file "~/.emacs"))
  (defun dot-emacs-outline ()
    "Navigate within .emacs using `outline-mode'."
    (interactive)
    (outline-mode)
    (setq outline-regexp ";; [0-9][\\.]?")
    (outline-mode)
    (outline-hide-body)
    (setq outline-top-level 4)
    (setq outline-previous-mode '(emacs-lisp-mode)))
  (defun eval-buffer-update ()
    "Evaluate buffer and update Emacs-Lisp mode."
    (interactive)
    (eval-buffer)
    (emacs-lisp-mode))
  (defun eval-line ()
    "Execute current line as Lisp code."
    (interactive)
    (eval-region (line-beginning-position)(line-end-position)))
  (defun eval-line-and-step ()
    "Execute current line as Lisp code and move to next line."
    (interactive)
    (eval-line)
    (forward-line))
  (defun lisp-comment-inline ()
    "Insert ;"
    (interactive "*")
    (insert " ; "))
  (defun lisp-template ()
    "Insert Lisp template."
    (interactive "*")
    (insert "(defun  ()
  \"\"
  (interactive)
  ())
")
    (search-backward " ()\n  ")))
(add-hook 'emacs-lisp-mode-hook       'arni-lisp-hook)
(add-hook 'lisp-mode-hook             'arni-lisp-hook)
(add-hook 'lisp-interaction-mode-hook 'arni-lisp-hook)
;;-----------------
;; 6.15 Postscript
;;-----------------
(defun arni-ps-hook ()
  (message nil)
  (local-unset-key [127]) ; reactivate backward-delete-char
  (local-set-key [?\C-c ?\C- ] 'pdf-clean   )
  (local-set-key [?\C-c ?\C-v] 'ps-ghostview)
  (defun pdf-clean ()
    "Call pdf-clean-* functions to replace stamps from PDF document."
    (interactive "*")
    (fundamental-mode) ; called by 'pdfclean' shell script
    (if (re-search-forward "^(Downloaded [Bb]y[:]? \\[.*)Tj$" nil t)
        (progn (goto-char (point-min))
               (pdf-clean-1)))
    (if (re-search-forward "^(Can. J. Fish. Aquat. Sci. Downloaded.*)Tj$" nil t)
        (progn (goto-char (point-min))
               (pdf-clean-2)))
    (if (re-search-forward "^(Downloaded from .*)Tj$" nil t)
        (progn (goto-char (point-min))
               (pdf-clean-3)))
    (if (re-search-forward "^(This content downloaded from .*)Tj$" nil t)
        (progn (goto-char (point-min))
               (pdf-clean-4))))
  (defun pdf-clean-1 ()
    "Replace PDF stamps like (Downloaded by [Arni Magnusson] ...) with spaces."
    (interactive "*")
    (let ((count 0))
      (fundamental-mode)
      (while (re-search-forward "^(Downloaded [Bb]y[:]? \\[.*)Tj$" nil t)
        (move-to-column 1)
        (blank-to-paren)
        (setq count (+ count 1)))
      (move-to-column 0)
      (ps-mode)
      (message "Removed %d PDF stamps" count)))
  (defun pdf-clean-2 ()
    "Replace 2-part PDF stamps like (Can. J. Fish...)
and (For personal use only. ) with spaces."
    (interactive "*")
    (let ((count 0))
      (fundamental-mode)
      (while (re-search-forward
              "^(Can. J. Fish. Aquat. Sci. Downloaded.*)Tj$" nil t)
        (move-to-column 1)
        (blank-to-paren))
      (goto-char (point-min))
      (while (re-search-forward "^(For personal use only. )Tj$" nil t)
        (move-to-column 1)
        (blank-to-paren)
        (setq count (+ count 1)))
      (move-to-column 0)
      (ps-mode)
      (message "Removed %d PDF stamps" count)))
  (defun pdf-clean-3 ()
    "Replace 3-part PDF stamps like with (Downloaded from ) with spaces."
    (interactive "*")
    (let ((count 0))
      (fundamental-mode)
      (while (re-search-forward "^(Downloaded from .*)Tj$" nil t)
        (if (string-match "^(Downloaded from )Tj$" (match-string 0))
            (re-search-backward "^(" nil t 3))
        (if (string-match "^(Downloaded from https://academic.oup.com.*)Tj$"
                          (match-string 0))
            (re-search-backward "^(" nil t 1))
        (dotimes (i 3)
          (re-search-forward "^(")
          (if (= (char-before) #x28)(blank-to-paren)))
        (setq count (+ count 1)))
      (move-to-column 0)
      (ps-mode)
      (message "Removed %d PDF stamps" count)))
  (defun pdf-clean-4 ()
    "Replace 4-part PDF stamps like (This content downloaded from ...)
with spaces."
    (interactive "*")
    (let ((count 0)
          (case-fold-search nil))
      (fundamental-mode)
      (while (re-search-forward "^(This content downloaded from .*)Tj$" nil t)
        (move-to-column 1)
        (blank-to-paren))
      (goto-char (point-min))
      (while (re-search-forward "^(All use subject to )Tj$" nil t)
        (move-to-column 1)
        (blank-to-paren))
      (goto-char (point-min))
      (while (re-search-forward "^(JSTOR Terms and Conditions)Tj$" nil t)
        (move-to-column 1)
        (blank-to-paren))
      (goto-char (point-min))
      (while (re-search-forward "^0 0 1 RG$" nil t)
        (replace-match "1 1 1 RG")
        (setq count (+ count 1)))
      (move-to-column 0)
      (ps-mode)
      (message "Removed %d PDF stamps" count)))
  (defun ps-ghostview ()
    "Open postscript file in viewer."
    (interactive)
    (shell-command (concat "ghostview \"" (buffer-name) "\"&"))
    (delete-other-windows)))
(add-hook 'ps-mode-hook 'arni-ps-hook) ; see also doc-view-mode-hook
;;------------
;; 6.16 Python
;;------------
(defun arni-python-hook ()
  (message nil)
  (local-set-key [f9]          'python-run)
  (local-set-key [M-return]    'python-run)
  (local-set-key [?\C-c ?\C-c] 'python-run)
  (local-set-key [?\C-c ?\C-v] 'python-run)
  (defun python-run ()
    "Run Python script"
    (interactive)
    (save-buffer)
    (shell-command (concat "python " (buffer-name)))))
(add-hook 'python-mode-hook 'arni-python-hook)
;;--------
;; 6.17 R
;;--------
;; Hook                    When  *R*  R-mode  Rd-mode  Notes
;; eval-after-load         load   x   x       x        workaround
;; ess-pre-run-hook        *R*    x                    workaround
;; comint-mode-hook        *R*    x                    *R/SQL* session
;; inferior-ess-mode-hook  *R*    x                    *R* session
;; ess-post-run-hook       *R*    x                    workaround
;; ess-mode-hook           edit       x                R-mode
;; Rd-mode-hook            man                x
;; ess-roxy-mode-hook      edit       x
(defun arni-ess-load-hook ()
  (setq inferior-R-args "--no-restore-data --quiet --save"))
(eval-after-load "ess-site" (arni-ess-load-hook)) ; ess-mode-load-hook
(defun arni-ess-pre-run-hook ()(setq ess-ask-for-ess-directory nil))
(add-hook 'ess-pre-run-hook 'arni-ess-pre-run-hook)
(defun arni-inferior-ess-hook ()
  (setq ess-startup-directory 'default-directory)
  (setq ess-use-tracebug nil)
  (set-face-attribute 'comint-highlight-input nil
                      :foreground "gray20") ; previous commands (bold)
  (set-face-attribute 'font-lock-constant-face nil
                      :underline -) ; <-, library, source, [1] output
  (set-face-attribute 'font-lock-type-face nil :foreground -) ; TRUE
  (local-unset-key [?\C-\M-m]) ; avoid ess-dirs
  (local-unset-key [?\C-y]   ) ; reactivate scroll-down-command
  (local-set-key [f11]               'ess-rdired-arni                      )
  (local-set-key [?\C-c C-backspace] 'ess-graphics-off                     )
  (local-set-key [?\C-c ?\C- ]       'ess-switch-to-edit-buffer            )
  (local-set-key [?\C-c ?\C-d]       'ess-dump-object-into-edit-buffer     )
  (local-set-key [?\C-c ?\C-h]       'ess-history                          )
  (local-set-key [?\C-c ?\C-l]       'comint-clear-window-keep-current-line)
  (defun ess-display-help-on-object (object)
    "Open help page in browser." ; override original function with same name
    (process-send-string ess-current-process-name
                         (concat "help(" object ",help_type='HTML')")))
  (defun ess-graphics-off () ; Provide (ess-graphics-off) both in *R* and R-mode
    "Close all graphics devices."
    (interactive)
    (ess-eval-linewise "graphics.off()"))
  (defun ess-history ()
    "Open R history file in other window."
    (interactive)
    (find-file-other-window ess-history-file)
    (font-lock-mode 0)
    (goto-char (point-max))
    (forward-line -1))
  (defalias 'R-history 'ess-history)
  (defun ess-rdired-arni ()
    "Run ess-rdired in large window."
    (interactive) ; ess-rdired runs no hook, so set keys here
    (ess-rdired)
    (delete-other-windows)
    (ess-rdired-next-line 1)
    (define-key ess-rdired-mode-map [down-mouse-1] 'ess-rdired-mouse-view)
    (define-key ess-rdired-mode-map [C-home]   'ess-rdired-top      )
    (define-key ess-rdired-mode-map [C-end]    'ess-rdired-bottom   )
    (define-key ess-rdired-mode-map [C-S-home] 'ess-rdired-top-mark )
    (define-key ess-rdired-mode-map [C-S-end]  'region-bol-bottom   )
    (define-key ess-rdired-mode-map [prior]    'ess-rdired-up-page  )
    (define-key ess-rdired-mode-map [next]     'ess-rdired-down-page)
    (define-key ess-rdired-mode-map [?\t]      'ess-rdired-other    )
    (define-key ess-rdired-mode-map [backtab]  'delete-other-windows)
    (define-key ess-rdired-mode-map [?\C-m]    'ess-rdired-other    ) ; return
    (define-key ess-rdired-mode-map [left]     'delete-other-windows)
    (define-key ess-rdired-mode-map [right]    'ess-rdired-other    )
    (define-key ess-rdired-mode-map [up]       'ess-rdired-up       )
    (define-key ess-rdired-mode-map [down]     'ess-rdired-down     )
    (define-key ess-rdired-mode-map [S-up]     'ess-rdired-up-mark  )
    (define-key ess-rdired-mode-map [S-down]   'region-bol-down     )
    (define-key ess-rdired-mode-map [C-up]     'ess-rdired-up-3     )
    (define-key ess-rdired-mode-map [C-down]   'ess-rdired-down-3   )
    (define-key ess-rdired-mode-map [M-up]     'ess-rdired-up-view  )
    (define-key ess-rdired-mode-map [M-down]   'ess-rdired-down-view)
    (define-key ess-rdired-mode-map [?\C-n]    'ess-rdired-down     )
    (define-key ess-rdired-mode-map [?\C-p]    'ess-rdired-up       )
    (define-key ess-rdired-mode-map [?\C-v]    'ess-rdired-down-page)
    (define-key ess-rdired-mode-map [?\C-y]    'ess-rdired-up-page  )
    (define-key ess-rdired-mode-map [?/]       'isearch-forward     )
    (define-key ess-rdired-mode-map [?M]       'ess-rdired-middle   )
    (define-key ess-rdired-mode-map [?N]       'ess-rdired-down-view)
    (define-key ess-rdired-mode-map [?P]       'ess-rdired-up-view  )
    (define-key ess-rdired-mode-map [?m]       'ess-rdired-middle   )
    (define-key ess-rdired-mode-map [?n]       'ess-rdired-down     )
    (define-key ess-rdired-mode-map [?o]       'ess-rdired-other    )
    (define-key ess-rdired-mode-map [?p]       'ess-rdired-up       )
    (define-key ess-rdired-mode-map [?q]
                'ess-rdired-quit-clean) ; ess-rdired-quit
    (define-key ess-rdired-mode-map [?v]
                'ess-rdired-other)) ; ess-rdired-view
  (defun ess-rdired-bottom ()
    "Move to bottom."
    (interactive)
    (deactivate-mark)
    (goto-char (point-max))
    (ess-rdired-previous-line 1))
  (defun ess-rdired-down (&optional n)
    "Move down N lines."
    (interactive "p")
    (deactivate-mark)
    (forward-line n)
    (if (eobp)
        (ess-rdired-previous-line 1)
      (ess-rdired-move-to-object)))
  (defun ess-rdired-down-3 ()
    "Move down 3 lines."
    (interactive)
    (ess-rdired-down 3))
  (defun ess-rdired-down-page (&optional n)
    "Move down N pages."
    (interactive "p")
    (ess-rdired-down (pages n)))
  (defun ess-rdired-down-view ()
    "Move down one line and view in other window."
    (interactive)
    (ess-rdired-next-line 1)
    (ess-rdired-other))
  (defun ess-rdired-middle ()
    "Move to middle."
    (interactive)
    (ess-rdired-top)
    (forward-line (middle-from-here))
    (ess-rdired-move-to-object))
  (defun ess-rdired-other ()
    "Show R object in other window."
    (interactive)
    (if (one-window-p)(split-window-right))
    (ess-rdired-view))
  (defun ess-rdired-quit-clean ()
    "Quit ess-rdired."
    (interactive)
    (if (get-buffer "*R view*")
        (kill-buffer "*R view*"))
    (delete-other-windows)
    (ess-rdired-quit))
  (defun ess-rdired-top ()
    "Move to top."
    (interactive)
    (deactivate-mark)
    (goto-char (point-min))
    (ess-rdired-next-line 1))
  (defun ess-rdired-top-mark ()
    "Extend region to top."
    (interactive)
    (region-bol-top 2))
  (defun ess-rdired-up (&optional n)
    "Move up N lines."
    (interactive "p")
    (deactivate-mark)
    (forward-line (- n))
    (if (< (line-number-at-pos) 2)
        (ess-rdired-top)
      (ess-rdired-move-to-object)))
  (defun ess-rdired-up-3 ()
    "Move up 3 lines."
    (interactive)
    (ess-rdired-previous-line 3))
  (defun ess-rdired-up-mark (&optional n)
    "Extend region up N lines."
    (interactive "p")
    (region-bol-up n)
    (if (< (line-number-at-pos) 2)
        (dired-top-mark)))
  (defun ess-rdired-up-page (&optional n)
    "Move up N pages."
    (interactive "p")
    (ess-rdired-up (pages n)))
  (defun ess-rdired-up-view ()
    "Move up one line and view in other window."
    (interactive)
    (ess-rdired-previous-line 1)
    (ess-rdired-other))
  (defun ess-smart-S-assign ()
    "Insert underscore."
    (interactive "*")
    (insert "_"))
  (defun ess-switch-to-edit-buffer ()
    "Switch to R edit buffer."
    (interactive)
    (let* ((buffers (buffer-list))
           (modes (mapcar (lambda (b)(with-current-buffer b major-mode))
                          buffers))
           (pointer (- (length buffers)(length (member 'ess-mode modes)))))
      (switch-to-buffer-other-window (nth pointer buffers)))))
(add-hook 'inferior-ess-mode-hook 'arni-inferior-ess-hook)
(defun arni-ess-post-run-hook ()
  (message nil)
  (ess-eval-linewise "options(continue='  ',width=80)" t))
(add-hook 'ess-post-run-hook 'arni-ess-post-run-hook)
(defun arni-ess-hook ()
  (if (equal (current-buffer) inferior-ess--last-started-process-buffer)
      (inferior-ess-mode))
  (setq make-backup-files t)
  (electric-indent-mode)
  (setq ess-eval-visibly-p nil)
  (setq ess-r-package-auto-set-evaluation-env nil) ; prevent pkg environment
  (setq ess-style 'OWN) ; and then modify entries within ess-own-style-list
  (setf (cdr (assoc 'ess-indent-offset ess-own-style-list)) 2)
  (setf (cdr (assoc 'ess-indent-with-fancy-comments ess-own-style-list)) nil)
  (font-lock-add-keywords nil '(("taf\\.png(\"\\(.*?\\)\""
                                 (1 font-lock-builtin-face t))))
  (set-face-attribute 'font-lock-constant-face
                      nil :underline - ) ; <-, library, source, [1] output
  (set-face-attribute 'font-lock-type-face     nil :foreground -) ; TRUE
  (local-unset-key [?\t]     ) ; reactivate indent-or-complete
  (local-unset-key [C-return]) ; reactivate recentf-open-files
  (local-unset-key [?\C-c ?.]) ; reactivate comment-line-or-region
  (local-unset-key [?\C-x ?n]) ; reactivate narrow-to-defun-or-region
  (local-unset-key [?\C-\M-m]) ; reactivate recentf-open-files
  (local-unset-key [?\C-y]   ) ; reactivate scroll-down-command
  (local-unset-key [?\C-\M-a]) ; reactivate copy-buffer
  (local-unset-key [?\C-\M-e]) ; reactivate query-replace-regexp
  (local-unset-key [?_]      ) ; reactivate self-insert-command
  (local-unset-key [?,]      ) ; reactivate self-insert-command
  (local-set-key [f9]                'ess-eval-region-or-line-and-step)
  (local-set-key [S-f9]              'ess-save-buffer-and-eval        )
  (local-set-key [f10]               'R-format-code                   )
  (local-set-key [S-f10]             'R-format-code-longline-nocomment)
  (local-set-key [f11]               'R-outline                       )
  (local-set-key [S-f11]             'hs-minor-mode                   )
  (local-set-key [C-f12]             'R-template-roxygen              )
  (local-set-key [?\C-c C-backspace] 'ess-graphics-off                )
  (local-set-key [M-return]  'ess-eval-region-or-function-or-paragraph-and-step)
  (local-set-key [?\C-c up]          'ess-eval-buffer-from-beg-to-here)
  (local-set-key [?\C-c down]        'ess-eval-buffer-from-here-to-end)
  (local-set-key [C-M-down]          'ess-eval-line-and-step          )
  (local-set-key [?\C-c ?\C-a]       'ess-roxy-insert-code            )
  (local-set-key [?\C-c ?\C-d]       'ess-eval-buffer-and-go          ) ; [map]
  (local-set-key [?\C-c ?\C-i]
                 'ess-roxy-insert-import) ; ess-complete-object-name-deprecated
  (local-set-key [?\C-c ?\C-l] 'ess-clear-R-window) ; ess-load-file
  (local-set-key [?\C-c ?\C-m] 'ess-eval-region-or-function-or-paragraph)
  (local-set-key [?\C-c ?\C-p]
                 'ess-roxy-insert-param) ; ess-eval-paragraph-and-step
  (local-set-key [?\C-c ?\C-v]
                 'ess-save-buffer-and-eval) ; ess-display-help-on-object
  (local-set-key [?\C-c ?\C-x] 'ess-eval-command        )
  (local-set-key [?\C-c ?\C- ] 'ess-switch-to-end-of-ESS)
  (local-set-key [?{]          'ess-electric-brace-open )
  (local-set-key [?}]          'ess-electric-brace-close)
  (defun ess-clear-R-window ()
    "Run `comint-clear-window' in *R* window to clear screen."
    (interactive)
    (let ((old-window (selected-window)))
      (select-window
       (get-buffer-window inferior-ess--last-started-process-buffer))
      (comint-clear-window)
      (select-window old-window)))
  (defun ess-electric-brace-close ()
    "Insert } and indent line."
    (interactive "*")
    (insert "}")
    (unindent-line)
    (indent-according-to-mode))
  (defun ess-electric-brace-open ()
    "Insert { and indent line."
    (interactive "*")
    (insert "{")
    (unindent-line)
    (indent-according-to-mode))
  (defun ess-eval-command (cmd)
    "Evaluate R command."
    (interactive "sCommand: ")
    (ess-eval-linewise cmd))
  (defun ess-graphics-off () ; Provide (ess-graphics-off) both in *R* and R-mode
    "Close all graphics devices."
    (interactive)
    (ess-eval-linewise "graphics.off()"))
  (defun ess-roxy-insert-code (&optional arg)
    "Insert \\code{} around object at point,
or \\code{\\link{}} if ARG is non-nil."
    (interactive "*P")
    (let ((open (if arg "\\code{\\link{" "\\code{"))
          (close (if arg "}}" "}")))
      (insert open)
      (if (eolp)
          (progn (insert close)
                 (search-backward "{")
                 (forward-char))
        (re-search-forward "[\n, ]" nil t)
        (backward-char)
        (if (= (char-before) ?.)
            (backward-char))
        (insert close))))
  (defun ess-roxy-insert-import ()
    "Insert @importFrom."
    (interactive "*")
    (cycle-spacing)
    (insert "@importFrom pkg fun"))
  (defun ess-roxy-insert-newline ()
    "Insert empty #' line."
    (interactive "*")
    (let ((col (current-column)))
      (beginning-of-line)
      (insert "#'\n")
      (move-to-column col)))
  (defun ess-roxy-insert-param ()
    "Insert @param."
    (interactive "*")
    (cycle-spacing)
    (insert "@param "))
  (defun ess-save-buffer-and-eval ()
    "Save buffer and evaluate."
    (interactive)
    (save-excursion
      (if (buffer-file-name)(save-buffer))
      (ess-eval-buffer nil)))
  (defun R-format-code ()
    "Format R in Arni style."
    (interactive "*")
    (let ((old-bsize (buffer-size)))
      (clean-trails)
      (untabify-buffer)
      (save-excursion
        (goto-char (point-min))
        (if (= (char-after) ?\")
            (progn (delete-char 1)
                   (search-forward "\"" nil t)
                   (replace-match ""))) ; "myfunc"
        (goto-char (point-min))
        (while (search-forward "function (" nil t)
          (replace-match "function(")) ; function
        (goto-char (point-min))
        (while (search-forward "for (" nil t)(replace-match "for("    )) ; for
        (goto-char (point-min))
        (while (search-forward "if ("  nil t)(replace-match "if("     )) ; if
        (goto-char (point-min))
        (while (search-forward " ~ "   nil t)(replace-match "~"       )) ; ~
        (goto-char (point-min))
        (while (search-forward " = "   nil t)(replace-match "="       )) ; =
        (goto-char (point-min))
        (while (search-forward " + "   nil t)(replace-match "+"       )) ; +
        (goto-char (point-min))
        (while (search-forward " - "   nil t)(replace-match "-"       )) ; -
        (goto-char (point-min))
        (while (search-forward " * "   nil t)(replace-match "*"       )) ; *
        (goto-char (point-min))
        (while (search-forward " / "   nil t)(replace-match "/"       )) ; /
        (goto-char (point-min))
        (while (search-forward "{"     nil t)(replace-match "\n{\n"   )) ; {
        (goto-char (point-min))
        (while (search-forward "}"     nil t)(replace-match "\n}\n"   )) ; {
        (goto-char (point-min))
        (while (re-search-forward "\n[ \n]+" nil t)(replace-match "\n")) ; \n
        (clean-trails)
        (indent-buffer))
      (if (= (buffer-size) old-bsize)
          (message "Formatted R code (still %d bytes)" (buffer-size))
        (message "Formatted R code (%d->%d bytes)" old-bsize (buffer-size)))))
  (defun R-format-code-longline-nocomment ()
    "Format R code in Arni style, with long lines and no comments."
    (interactive "*")
    (let ((old-bsize (buffer-size)))
      (R-format-code)
      (delete-comments)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward ",\n[\t ]*"   nil t)(replace-match ", ")) ; ,
        (goto-char (point-min))
        (while (re-search-forward "~\n[\t ]*"   nil t)(replace-match "~ ")) ; ~
        (goto-char (point-min))
        (while (re-search-forward "=\n[\t ]*"   nil t)(replace-match "= ")) ; =
        (goto-char (point-min))
        (while (re-search-forward "+\n[\t ]*"   nil t)(replace-match "+ ")) ; +
        (goto-char (point-min))
        (while (re-search-forward "-\n[\t ]*"   nil t)(replace-match "- ")) ; -
        (goto-char (point-min))
        (while (re-search-forward "*\n[\t ]*"   nil t)(replace-match "* ")) ; *
        (goto-char (point-min))
        (while (re-search-forward "/\n[\t ]*"   nil t)(replace-match "/ ")) ; /
        (goto-char (point-min))
        (while (re-search-forward "&\n[\t ]*"   nil t)(replace-match "& ")) ; &
        (goto-char (point-min))
        (while (re-search-forward "|\n[\t ]*"   nil t)(replace-match "| ")) ; |
        (goto-char (point-min))
        (while (re-search-forward "(\n[\t ]*"   nil t)(replace-match "( ")) ; (
        (goto-char (point-min))
        (while (re-search-forward "\n[\t ]*)"   nil t)(replace-match ")" )) ; )
        (goto-char (point-min))
        (while (re-search-forward "\\[\n[\t ]*" nil t)(replace-match "[ ")) ; [
        (goto-char (point-min))
        (while (re-search-forward "\n[\t ]*\\]" nil t)(replace-match "]"))) ; ]
      (R-format-code)
      (if (= (buffer-size) old-bsize)
          (message "Formatted R code (still %d bytes)" (buffer-size))
        (message "Formatted R code (%d->%d bytes)" old-bsize (buffer-size)))))
  (defun R-header-to-roxy ()
    "Convert R function from Arni-style comment header to Roxygen format."
    (interactive "*")
    (require 'drag-stuff)
    (goto-char (point-min))
    (while (= (char-after (line-beginning-position 2)) ?#)
      (pull-line-down 1))
    (kill-line -1)
    (insert "#' @export\n\n")
    (goto-char (point-min))
    (kill-line 4)
    (goto-char (point-min))
    (while (re-search-forward "###" nil t)(replace-match "#'"))
    (goto-char (point-min))
    (while (re-search-forward " +#$" nil t)(replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "Purpose:  " nil t)(replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "Args:    "  nil t)(replace-match "@param"))
    (goto-char (point-min))
    (while (re-search-forward "Notes:   "  nil t)(replace-match "@note\n#'"))
    (goto-char (point-min))
    (while (re-search-forward "Returns: "  nil t)(replace-match "@return\n#'"))
    (goto-char (point-min))
    (kill-new (buffer-substring-no-properties (point-min)
                                              (line-beginning-position 3)))
    (yank)
    (goto-char (+ (point-min) 3))
    (titlecase-dwim))
  (defun R-template-roxygen ()
    "Insert Roxygen template."
    (interactive "*")
    (goto-char (point-min))
    (insert "
#' Title Case
#'
#' What the function does.
#'
#' @param arg meaning.
#'
#' @return
#' Type of output.
#'
#' @export

")
    (goto-char (point-min))
    (delete-char 1)
    (forward-char 3))
  (defun R-outline ()
    "Navigate within R code using `outline-mode'."
    (interactive)
    (outline-mode)
    (setq outline-regexp " *## [0-9]")
    (outline-mode)
    (outline-hide-body)
    (setq outline-previous-mode '(R-mode))))
(add-hook 'ess-mode-hook 'arni-ess-hook)
(defun arni-Rd-hook ()
  (message nil)
  (setq-local indent-line-function 'Rd-mode-indent-line)
  (setq make-backup-files t)
  (setq save-abbrevs nil)
  (font-lock-mode 1)
  (set-face-attribute 'font-lock-constant-face nil :weight 'bold :underline -)
  (local-unset-key [?\t]) ; reactivate indent-or-complete
  (local-set-key [f11]         'Rd-outline               )
  (local-set-key [?\C-c ?\C- ] 'Rd-table-sep             )
  (local-set-key [?\C-c ?\C-c] 'Rd-compile-html          )
  (local-set-key [?\C-c ?\C-l] 'Rd-link                  )
  (local-set-key [?\C-c ?\C-m] 'Rd-get-latex-master      )
  (local-set-key [?\C-c ?\C-t] 'Rd-compile-latex         )
  (local-set-key [?\C-c ?\C-v] 'Rd-view-html             )
  (local-set-key [?\C-j]       'Rd-fill-paragraph-forward)
  (defun Rd-compile-html ()
    "Compile Rd file to HTML."
    (interactive)
    (save-buffer)
    (compile (concat "R CMD Rdconv -t html -o "
                     (file-name-sans-extension (buffer-name)) ".html "
                     (buffer-name))))
  (defun Rd-compile-latex ()
    "Compile Rd file to LaTeX."
    (interactive)
    (save-buffer)
    (compile (concat "R CMD Rdconv -t latex -o "
                     (file-name-sans-extension (buffer-name)) ".tex "
                     (buffer-name))))
  (defun Rd-fill-paragraph-forward ()
    "Justify and go to end of paragraph."
    (interactive "*")
    (fill-paragraph nil)
    (clean-trails)
    (message nil)
    (forward-paragraph))
  (defun Rd-link ()
    "Insert \\code{\\link{}}."
    (interactive "*")
    (insert "\\code{\\link{}}")
    (search-backward "}}"))
  (defun Rd-outline ()
    "Navigate within .Rd file using `outline-mode'."
    (interactive)
    (outline-mode)
    (setq outline-regexp "\\\\nam\\|\\\\....")
    (outline-mode)
    (outline-hide-sublevels 4)
    (setq outline-previous-mode '(Rd-mode)))
  (defun Rd-table-sep ()
    "Insert \\tab"
    (interactive "*")
    (insert " \\tab "))
  (defun Rd-view-html ()
    "View HTML file with same prefix as current Rd file."
    (interactive)
    (browse-url (concat (file-name-sans-extension (buffer-file-name))
                        ".html"))))
(add-hook 'Rd-mode-hook 'arni-Rd-hook)
(defun arni-Rnw-hook ()
  (message nil)
  (local-set-key [?\C-c ?\C-w] 'ess-swv-weave))
(add-hook 'Rnw-mode-hook 'arni-Rnw-hook)
(defun arni-roxy-hook ()
  (define-key ess-roxy-mode-map [?\C-c ?\C-o] 'ess-roxy-insert-newline)) ; [map]
(add-hook 'ess-roxy-mode-hook 'arni-roxy-hook)
(defun Rni ()
  "Start interactive R session."
  (interactive)
  (let ((was-one-window (one-window-p))
        (original-other-buffer (window-buffer (next-window)))
        (original-window (selected-window)))
    (R)
    (if was-one-window
        (delete-other-windows)                 ; single-window R session
      (switch-to-buffer original-other-buffer) ; revert secondary window
      (select-window original-window)          ; go to main window
      (switch-to-buffer inferior-ess--last-started-process-buffer)) ; show *R*
    (comint-clear-window)))
;;----------
;; 6.18 SQL
;;----------
(defun arni-sql-hook ()
  (setq make-backup-files t)
  (sql-set-product 'oracle)
  (setq indent-line-function 'indent-relative-definitely)
  (setq tab-stop-list '(4 8))
  (arni-colors)
  (set-face-attribute 'font-lock-builtin-face nil :foreground - :weight 'bold)
  (set-face-attribute 'font-lock-keyword-face nil               :weight 'bold)
  (local-set-key [M-return] 'newline-and-indent)
  (if (string-equal (buffer-name) "*SQL*")
      (sql-interactive-mode)))
(add-hook 'sql-mode-hook 'arni-sql-hook)
(defun arni-sql-interactive-hook ()
  (add-to-list 'same-window-buffer-names "*SQL*")
  (setq indent-line-function 'ignore))
(add-hook 'sql-interactive-mode-hook 'arni-sql-interactive-hook)
(defun sql-oracle-default()
  "Start Oracle session as default user."
  (interactive)
  (require 'sql)
  (setq sql-user "/")
  (defalias 'sql-get-login 'ignore)
  (sql-oracle)
  (message nil)
  (sleep-for 0.4)
  (comint-send-string sql-buffer
                      "SET EMBED ON FEEDBACK OFF PAGESIZE 50000 PAUSE OFF \
SQLPROMPT '> ' UNDERLINE OFF LINESIZE 60")
  (comint-clear-window))
;;----------
;; 6.19 TMB
;;----------
(require 'tmb nil t)
(defun arni-tmb-hook ()
  ;; (setq tmb-compile-args ",'-fno-gnu-unique -O0 -Wall'") ; in .emacs-linux.el
  ;; (setq tmb-debug-args ",'-fno-gnu-unique -g -O0'") ; in .emacs-linux.el
  (set-face-attribute 'font-lock-warning-face nil :weight 'normal     )
  (set-face-attribute 'tmb-block-face         nil :foreground "sienna")
  (set-face-attribute 'tmb-data-face          nil :foreground "sienna")
  (set-face-attribute 'tmb-parameter-face     nil :foreground "sienna")
  (set-face-attribute 'tmb-report-face        nil :foreground "sienna")
  ;; Unset C-c C-h from c++ hook to show an overview of keybindings
  (local-unset-key [?\C-c ?\C-h])
  (local-set-key [f7]          'tmb-clean           )
  (local-set-key [f8]          'tmb-compile         )
  (local-set-key [f9]          'tmb-run             )
  (local-set-key [f10]         'tmb-debug           )
  (local-set-key [f11]         'tmb-open            )
  (local-set-key [C-f12]       'tmb-template-mini   )
  (local-set-key [?\C-c C-backspace] 'tmb-clean     ) ; show keybinding in menu
  (local-set-key [?\C-c ?\C-a] 'tmb-run-any         )
  (local-set-key [?\C-c ?\C-b] 'tmb-run             )
  (local-set-key [?\C-c ?\C-c] 'tmb-compile         )
  (local-set-key [?\C-c ?\C-d] 'tmb-debug           )
  (local-set-key [?\C-c ?\C-l] 'tmb-show-compilation)
  (local-set-key [?\C-c ?\C-m] 'tmb-make            ) ; show keybinding in menu
  (local-set-key [?\C-c ?\C-v] 'tmb-run            ))
(add-hook 'tmb-mode-hook 'arni-tmb-hook)
;;---------
;; 6.20 VB
;;---------
(defun arni-visual-basic-hook ()
  (abbrev-mode 0)
  (setq save-abbrevs nil)
  (setq visual-basic-mode-indent 2)
  (arni-colors)
  (set-face-attribute 'font-lock-function-name-face nil :foreground "brown4"))
(add-hook 'visual-basic-mode-hook 'arni-visual-basic-hook)
;;==============================================================================
;;
;; 7  OTHER MODES
;;
;;==============================================================================
;; Settings, faces, keys, functions
;;------------
;; 7.1  Align
;;------------
(defun arni-align-hook ()
  (add-to-list 'align-text-modes 'admb-mode)
  (add-to-list 'align-rules-list
               '(text-column-whitespace
                 (regexp . "\\(\\S-\\)\\([ \t]+\\)")
                 (group . 2)(modes . align-text-modes)(repeat . t))))
;; To shrink initial whitespace too, use (regexp . "\\(^\\|\\S-\\)\\([ \t]+\\)")
(add-hook 'align-load-hook 'arni-align-hook)
;;--------------
;; 7.2  Apropos
;;--------------
(defun arni-apropos-hook ()
  (setq apropos-compact-layout t)
  (local-set-key [C-home]   'apropos-top         )
  (local-set-key [C-S-home] 'apropos-top-mark    )
  (local-set-key [C-end]    'apropos-bottom      )
  (local-set-key [prior]    'apropos-up-page     )
  (local-set-key [next]     'apropos-down-page   )
  (local-set-key [?\t]      'apropos-down-view   )
  (local-set-key [backtab]  'apropos-up-view     )
  (local-set-key [left]     'delete-other-windows)
  (local-set-key [right]    'apropos-follow      )
  (local-set-key [up]       'apropos-up          )
  (local-set-key [down]     'apropos-down        )
  (local-set-key [M-up]     'apropos-up-view     )
  (local-set-key [M-down]   'apropos-down-view   )
  (local-set-key [?\C-n]    'apropos-down        )
  (local-set-key [?\C-p]    'apropos-up          )
  (local-set-key [?\C-y]    'apropos-up-page     )
  (local-set-key [?\C-v]    'apropos-down-page   )
  (local-set-key [? ]       'apropos-follow      )
  (local-set-key [?/]       'isearch-forward     )
  (local-set-key [?M]       'apropos-middle      )
  (local-set-key [?N]       'apropos-down-view   )
  (local-set-key [?P]       'apropos-up-view     )
  (local-set-key [?m]       'apropos-middle      )
  (local-set-key [?n]       'apropos-down        )
  (local-set-key [?p]       'apropos-up          )
  (local-set-key [?v]       'apropos-follow      )
  (defun apropos-bottom ()
    "Move to bottom."
    (interactive)
    (goto-char (point-max))
    (apropos-up 1))
  (defun apropos-down (&optional n)
    "Move down N lines."
    (interactive "p")
    (deactivate-mark)
    (end-of-line)
    (ignore-errors (forward-button n nil))
    (beginning-of-line))
  (defun apropos-down-page (&optional n)
    "Move down N pages."
    (interactive "p")
    (scroll-up)
    (apropos-up 1))
  (defun apropos-down-view ()
    "Move down one line and view in other window."
    (interactive)
    (apropos-down 1)
    (apropos-follow))
  (defun apropos-middle ()
    "Move to middle."
    (interactive)
    (apropos-top)
    (forward-line (middle-from-here))
    (apropos-up 1)
    (apropos-down 1))
  (defun apropos-top ()
    "Move to top."
    (interactive)
    (deactivate-mark)
    (goto-char (point-min))
    (forward-button 1)
    (beginning-of-line))
  (defun apropos-top-mark ()
    "Extend region to top."
    (interactive)
    (region-bol-top 6))
  (defun apropos-up (&optional n)
    "Move up N lines."
    (interactive "p")
    (deactivate-mark)
    (if (< (line-number-at-pos) 7)
        (apropos-top)
      (progn (backward-button n nil)
             (beginning-of-line))))
  (defun apropos-up-page (&optional n)
    "Move up N pages."
    (interactive "p")
    (scroll-down)
    (apropos-down 1))
  (defun apropos-up-view ()
    "Move up one line and view in other window."
    (interactive)
    (apropos-up 1)
    (apropos-follow)))
(add-hook 'apropos-mode-hook 'arni-apropos-hook)
;;-------------
;; 7.3  Buffer
;;-------------
(defun arni-buffer-menu-hook ()
  (require 'ebuff-menu)
  (local-set-key [backspace] 'Buffer-menu-unmark-up        )
  (local-set-key [C-home]    'Buffer-menu-top              )
  (local-set-key [C-S-home]  'Buffer-menu-top-mark         )
  (local-set-key [C-end]     'Buffer-menu-bottom           )
  (local-set-key [C-S-end]   'Buffer-menu-bottom-mark      )
  (local-set-key [prior]     'Buffer-menu-up-page          )
  (local-set-key [next]      'Buffer-menu-down-page        )
  (local-set-key [?\t]       'Buffer-menu-down             )
  (local-set-key [backtab]   'Buffer-menu-up               )
  (local-set-key [left]      'delete-other-windows         )
  (local-set-key [right]     'Buffer-menu-view-other       )
  (local-set-key [S-left]    'delete-other-windows         )
  (local-set-key [S-right]   'Buffer-menu-view-other       )
  (local-set-key [C-left]    'delete-other-windows         )
  (local-set-key [C-right]   'Buffer-menu-view-other       )
  (local-set-key [up]        'Buffer-menu-up               )
  (local-set-key [down]      'Buffer-menu-down             )
  (local-set-key [S-up]      'Buffer-menu-up-mark          )
  (local-set-key [S-down]    'Buffer-menu-down-mark        )
  (local-set-key [C-up]      'Buffer-menu-up-3             )
  (local-set-key [C-down]    'Buffer-menu-down-3           )
  (local-set-key [M-up]      'Buffer-menu-up-view          )
  (local-set-key [M-down]    'Buffer-menu-down-view        )
  (local-set-key [?\C-n]     'Buffer-menu-down             )
  (local-set-key [?\C-p]     'Buffer-menu-up               )
  (local-set-key [?\C-v]     'Buffer-menu-down-page        )
  (local-set-key [?\C-y]     'Buffer-menu-up-page          )
  (local-set-key [?/]        'isearch-forward              )
  (local-set-key [?M]        'Buffer-menu-middle           )
  (local-set-key [?N]        'Buffer-menu-down-view        )
  (local-set-key [?P]        'Buffer-menu-up-view          )
  (local-set-key [?U]        'Buffer-menu-unmark-all       )
  ;; [?b] for electric-buffer-list, already in list-buffers
  (local-set-key [?b]        'Buffer-menu-bury             )
  (local-set-key [?d]        'Buffer-menu-mark-down-delete )
  (local-set-key [?f]        'Buffer-menu-toggle-files-only)
  (local-set-key [?h]        'Buffer-menu-toggle-files-only)
  (local-set-key [?i]        'Buffer-menu-unmark-up        )
  (local-set-key [?m]        'Buffer-menu-mark-down        )
  (local-set-key [?n]        'Buffer-menu-down             )
  (local-set-key [?o]
                 'Buffer-menu-view-other) ; Buffer-menu-other-window
  (local-set-key [?p]        'Buffer-menu-up               )
  (local-set-key [?q]        'kill-this-buffer             )
  (local-set-key [?t]
                 'Buffer-menu-toggle-files-only) ; Buffer-menu-visit-tags-table
  (local-set-key [?u]        'Buffer-menu-unmark-down      )
  ;; [?v] for list-buffers, already in electric-buffer-list
  (local-set-key [?v]        'Electric-buffer-menu-mode-view-buffer)
  (defun Buffer-menu-bottom ()
    "Move to bottom."
    (interactive)
    (deactivate-mark)
    (goto-char (point-max))
    (forward-line -1))
  (defun Buffer-menu-bottom-mark ()
    "Extend region to bottom."
    (interactive)
    (region-bol-bottom))
  (defun Buffer-menu-down (&optional n)
    "Move down N lines."
    (interactive "p")
    (deactivate-mark)
    (forward-line n)
    (if (eobp)(forward-line -1)))
  (defun Buffer-menu-down-3 ()
    "Move down 3 lines."
    (interactive)
    (Buffer-menu-down 3))
  (defun Buffer-menu-down-mark (&optional n)
    "Extend region down N lines."
    (interactive "p")
    (region-bol-down n))
  (defun Buffer-menu-down-page (&optional n)
    "Move down N pages."
    (interactive "p")
    (Buffer-menu-down (pages n)))
  (defun Buffer-menu-down-view ()
    "Move down one line and view in other window."
    (interactive)
    (Buffer-menu-down 1)
    (Buffer-menu-view-other))
  (defun Buffer-menu-mark-down ()
    "Mark and move down."
    (interactive)
    (Buffer-menu-mark)
    (if (eobp)(Buffer-menu-bottom)))
  (defun Buffer-menu-mark-down-delete ()
    "Mark for deletion and move down."
    (interactive)
    (Buffer-menu-delete)
    (if (eobp)(Buffer-menu-bottom)))
  (defun Buffer-menu-middle ()
    "Move to middle."
    (interactive)
    (Buffer-menu-top)
    (forward-line (middle-from-here)))
  (defun Buffer-menu-top ()
    "Move to top."
    (interactive)
    (deactivate-mark)
    (goto-char (point-min)))
  (defun Buffer-menu-top-mark ()
    "Extend region to top."
    (interactive)
    (region-bol-top 1))
  (defun Buffer-menu-unmark-all ()
    "Unmark all buffers."
    (interactive)
    (save-excursion
      (Buffer-menu-top)
      (while (not (eobp))(Buffer-menu-unmark))))
  (defun Buffer-menu-unmark-down ()
    "Unmark and move down."
    (interactive)
    (Buffer-menu-unmark)
    (if (eobp)(Buffer-menu-bottom)))
  (defun Buffer-menu-unmark-up ()
    "Unmark and move up."
    (interactive)
    (Buffer-menu-unmark t))
  (defun Buffer-menu-up (&optional n)
    "Move up N lines."
    (interactive "p")
    (deactivate-mark)
    (forward-line (- n)))
  (defun Buffer-menu-up-3 ()
    "Move up 3 lines."
    (interactive)
    (forward-line -3))
  (defun Buffer-menu-up-page (&optional n)
    "Move up N pages."
    (interactive "p")
    (Buffer-menu-up (pages n)))
  (defun Buffer-menu-up-mark (&optional n)
    "Extend region up N lines."
    (interactive "p")
    (region-bol-up n))
  (defun Buffer-menu-up-view ()
    "Move up one line and view in other window."
    (interactive)
    (Buffer-menu-up 1)
    (Buffer-menu-view-other))
  (defun Buffer-menu-view-other ()
    "View buffer in other window."
    (interactive)
    (if (one-window-p)(split-window-right))
    (Buffer-menu-switch-other-window)))
(add-hook 'Buffer-menu-mode-hook 'arni-buffer-menu-hook)
(add-hook 'electric-buffer-menu-mode-hook 'arni-buffer-menu-hook)
(defun arni-ibuffer-hook ()(ibuffer-do-sort-by-alphabetic))
(add-hook 'ibuffer-hook 'arni-ibuffer-hook)
(defun arni-ibuffer-mode-hook ()
  (setq ibuffer-display-summary nil) ; don't show buffer count and total bytes
  (setq ibuffer-expert t           ) ; only ask before killing edited buffers
  (setq ibuffer-movement-cycle nil ) ; don't jump between top and bottom
  (setq ibuffer-deletion-face 'font-lock-comment-face)
  (setq ibuffer-marked-face   'font-lock-builtin-face)
  (set-face-attribute 'font-lock-type-face nil
                      :foreground (fg 'font-lock-comment-face))
  (local-unset-key [?\M-n]) ; reactivate bs-cycle-next
  (local-unset-key [?\M-p]) ; reactivate bs-cycle-previous
  (local-set-key [f5]       'ibuffer-update         ) ; revert-buffer
  (local-set-key [f10]      'ibuffer-do-sort-by-alphabetic)
  (local-set-key [f11]      'ibuffer-do-sort-by-size)
  (local-set-key [f12]      'ibuffer-do-sort-by-major-mode)
  (local-set-key [C-home]   'ibuffer-top            )
  (local-set-key [C-S-home] 'ibuffer-top-mark       )
  (local-set-key [C-end]    'ibuffer-bottom         )
  (local-set-key [prior]    'ibuffer-up-page        )
  (local-set-key [next]     'ibuffer-down-page      )
  (local-set-key [?\t]      'ibuffer-down           )
  (local-set-key [backtab]  'ibuffer-up             )
  (local-set-key [left]   'delete-other-windows) ; ibuffer-backward-filter-group
  (local-set-key [right]    'ibuffer-view-other) ; ibuffer-forward-filter-group
  (local-set-key [S-left]   'delete-other-windows   )
  (local-set-key [S-right]  'ibuffer-view-other     )
  (local-set-key [C-left]   'delete-other-windows   )
  (local-set-key [C-right]  'ibuffer-view-other     )
  (local-set-key [up]       'ibuffer-up             ) ; ibuffer-backward-line
  (local-set-key [down]     'ibuffer-down           ) ; ibuffer-forward-line
  (local-set-key [S-up]     'ibuffer-up-mark        )
  (local-set-key [S-down]   'region-bol-down        )
  (local-set-key [C-up]     'ibuffer-up-3           )
  (local-set-key [C-down]   'ibuffer-down-3         )
  (local-set-key [M-up]     'ibuffer-up-view        )
  (local-set-key [M-down]   'ibuffer-down-view      )
  (local-set-key [?\C-n]    'ibuffer-down           )
  (local-set-key [?\C-p]    'ibuffer-up             )
  (local-set-key [?\C-v]    'ibuffer-down-page      )
  (local-set-key [?\C-y]    'ibuffer-up-page        )
  (local-set-key [?*] 'ibuffer-mark-special-buffers ) ; [map]
  (local-set-key [?/] 'isearch-forward              ) ; [map]
  (local-set-key [?E] 'ibuffer-do-replace-regexp    ) ; ibuffer-do-eval
  (local-set-key [?F] 'ibuffer-do-occur) ; ibuffer-do-shell-command-file
  (local-set-key [?K] 'ibuffer-do-delete            )
  (local-set-key [?M] 'ibuffer-middle) ; ibuffer-do-toggle-modified
  (local-set-key [?N]
                 'ibuffer-down-view) ; ibuffer-do-shell-command-pipe-replace
  (local-set-key [?P] 'ibuffer-up-view              ) ; ibuffer-do-print
  (local-set-key [?U] 'ibuffer-unmark-all-marks) ; ibuffer-do-replace-regexp
  (local-set-key [?d] 'ibuffer-mark-down-delete     ) ; ibuffer-mark-for-delete
  (local-set-key [?e] 'ibuffer-do-query-replace-regexp) ; ibuffer-visit-buffer
  (local-set-key [?f] 'ibuffer-mark-by-name-regexp  ) ; ibuffer-visit-buffer
  (local-set-key [?i] 'ibuffer-unmark-up            )
  (local-set-key [?h] 'ibuffer-hide-lines           ) ; describe-mode
  (local-set-key [?k] 'ibuffer-mark-down-delete     ) ; ibuffer-do-kill-lines
  (local-set-key [?m] 'ibuffer-mark-down            ) ; ibuffer-mark-forward
  (local-set-key [?n] 'ibuffer-down                 ) ; ibuffer-forward-line
  (local-set-key [?o] 'ibuffer-view-other) ; ibuffer-visit-buffer-other-window
  (local-set-key [?p] 'ibuffer-up                   ) ; ibuffer-backward-line
  (local-set-key [?q] 'kill-this-buffer             ) ; ibuffer-quit
  (local-set-key [?r] 'ibuffer-do-eval              )
  (local-set-key [?u] 'ibuffer-unmark-down          ) ; ibuffer-unmark-forward
  (local-set-key [?v] 'ibuffer-view                 ) ; ibuffer-do-view
  (defun ibuffer-bottom ()
    "Move to bottom."
    (interactive)
    (deactivate-mark)
    (goto-char (point-max))
    (ibuffer-backward-line))
  (defun ibuffer-down (&optional n)
    "Move down N lines."
    (interactive "p")
    (deactivate-mark)
    (ibuffer-forward-line n)
    (if (< (line-number-at-pos) 4)
        (ibuffer-top))
    (if (eobp)(ibuffer-bottom)))
  (defun ibuffer-down-3 ()
    "Move down 3 lines."
    (interactive)
    (ibuffer-down 3))
  (defun ibuffer-down-page (&optional n)
    "Move down N pages."
    (interactive "p")
    (ibuffer-down (pages n)))
  (defun ibuffer-down-view ()
    "Move down one line and view in other window."
    (interactive)
    (ibuffer-down 1)
    (ibuffer-view-other))
  (defun ibuffer-hide-lines ()
    "Hide marked lines, or current line if none are marked."
    (interactive)
    (if (zerop (ibuffer-count-marked-lines))
        (ibuffer-mark-forward 1)) ; improved ibuffer-do-kill-lines
    (let ((count (ibuffer-map-marked-lines #'(lambda (buf mark) 'kill))))
      (message "Hid %d lines" count)))
  (defun ibuffer-mark-down ()
    "Mark and move down."
    (interactive)
    (ibuffer-mark-forward 1)
    (if (eobp)(ibuffer-bottom)))
  (defun ibuffer-mark-down-delete ()
    "Mark for deletion and move down."
    (interactive)
    (ibuffer-mark-for-delete 1)
    (if (eobp)(ibuffer-bottom)))
  (defun ibuffer-middle ()
    "Move to middle."
    (interactive)
    (ibuffer-top)
    (ibuffer-forward-line (middle-from-here)))
  (defun ibuffer-top ()
    "Move to top."
    (interactive)
    (deactivate-mark)
    (goto-line-lisp 3)
    (ibuffer-forward-line))
  (defun ibuffer-top-mark ()
    "Extend region to top."
    (interactive)
    (region-bol-top 4))
  (defun ibuffer-unmark-all-marks ()
    "Unmark all buffers."
    (interactive)
    (ibuffer-unmark-all (string-to-char "*")))
  (defun ibuffer-unmark-down ()
    "Unmark and move down."
    (interactive)
    (ibuffer-unmark-forward 1)
    (if (eobp)(ibuffer-bottom)))
  (defun ibuffer-unmark-up ()
    "Unmark and move up."
    (interactive)
    (ibuffer-unmark-backward 1)
    (if (< (line-number-at-pos) 4)
        (ibuffer-top)))
  (defun ibuffer-up (&optional n)
    "Move up N lines."
    (interactive "p")
    (deactivate-mark)
    (ibuffer-backward-line n)
    (if (< (line-number-at-pos) 4)
        (ibuffer-top)))
  (defun ibuffer-up-3 ()
    "Move up 3 lines."
    (interactive)
    (ibuffer-up 3))
  (defun ibuffer-up-page (&optional n)
    "Move up N pages."
    (interactive "p")
    (ibuffer-up (pages n)))
  (defun ibuffer-up-mark (&optional n)
    "Extend region up N lines."
    (interactive "p")
    (region-bol-up n)
    (if (< (line-number-at-pos) 4)
        (ibuffer-top-mark)))
  (defun ibuffer-up-view ()
    "Move up one line and view in other window."
    (interactive)
    (ibuffer-up 1)
    (ibuffer-view-other))
  (defun ibuffer-view ()
    "View buffer."
    (interactive)
    (view-buffer (ibuffer-current-buffer t)))
  (defun ibuffer-view-other ()
    "View buffer in other window."
    (interactive)
    (if (one-window-p)(split-window-right))
    (ibuffer-visit-buffer-other-window-noselect)))
(add-hook 'ibuffer-mode-hook 'arni-ibuffer-mode-hook)
;;----------------
;; 7.4  ChangeLog
;;----------------
(defun arni-change-log-hook ()
  (font-lock-mode 1)
  (set-face-attribute 'change-log-name nil :underline t :inherit -)
  (local-set-key [M-return] 'change-log-newline)
  (defun change-log-newline ()
    "Insert newline and add new entry."
    (interactive)
    (insert "\n\n\t* ")))
(add-hook 'change-log-mode-hook 'arni-change-log-hook)
;;-------------
;; 7.5  Comint
;;-------------
;; Interactive sessions (*gud-program*, *R*, *shell*, *SQL*)
(defun arni-comint-hook ()
  (setq show-trailing-whitespace nil)(setq comint-input-ring-size 5000)
  (local-unset-key [?\C-c ? ]) ; reactivate hl-line-mode
  (local-unset-key [C-up]    ) ; reactivate previous-line
  (local-unset-key [C-down]  ) ; reactivate next-line
  (local-unset-key [?\C-d]   ) ; delete-char
  (local-unset-key [?\M-n]   ) ; reactivate bs-cycle-next
  (local-unset-key [?\M-p]   ) ; reactivate bs-cycle-previous
  (local-unset-key [?\M-r]   ) ; reactivate Rni
  (local-set-key   [M-up]        'comint-previous-matching-input-from-input)
  (local-set-key   [M-down]      'comint-next-matching-input-from-input    )
  (global-set-key  [C-f4]        'kill-process-or-buffer                   )
  (global-set-key  [M-f4]        'kill-process-or-emacs                    )
  (local-set-key   [home]        'comint-bol-nomark                        )
  (local-set-key   [S-home]      'comint-bol-mark                          )
  (local-set-key   [C-pause]     'comint-interrupt-subjob                  )
  (local-set-key   [backspace]   'comint-backward-delete-char              )
  (local-set-key   [C-backspace] 'comint-backward-delete-word              )
  (local-set-key   [left]        'comint-left                              )
  (local-set-key   [S-left]      'comint-left-mark                         )
  (local-set-key   [C-left]      'comint-left-word                         )
  (local-set-key   [C-S-left]    'comint-left-word-mark                    )
  (local-set-key   [up]          'comint-previous-or-up                    )
  (local-set-key   [down]        'comint-next-or-down                      )
  (global-set-key  [?\C-x ?\C-c] 'kill-process-or-emacs                    )
  (global-set-key  [?\C-x ?k]    'kill-process-or-buffer                   )
  (local-set-key   [?\C-x ?l]    'comint-flush-window                      )
  (local-set-key   [?\M-k]       'kill-process-or-buffer                   )
  (local-set-key   [?\C-,]       'comint-left-word                         )
  (local-set-key   [?\C-<]       'comint-left-word-mark                    )
  (local-set-key   [?\C-b]       'comint-left                              )
  (local-set-key   [?\C-l]       'comint-clear-window-keep-current-line    )
  (local-set-key   [?\C-n]       'comint-next-or-down                      )
  (local-set-key   [?\C-p]       'comint-previous-or-up                    )
  (defun comint-backward-delete-char ()
    "Delete one character backward, unless at prompt."
    (interactive "*")
    (if (not (= (point)(comint-line-beginning-position)))
        (delete-char -1)))
  (defun comint-backward-delete-word ()
    "Delete previous word, unless at prompt."
    (interactive "*")
    (if (not (= (point)(comint-line-beginning-position)))
        (backward-delete-word 1)))
  (defun comint-bol-mark ()
    "Select region from here to beginning of line, excluding the prompt."
    (interactive)
    (if (not mark-active)(push-mark))
    (comint-bol)
    (region-set))
  (defun comint-bol-nomark ()
    "Go to beginning of line, excluding the prompt."
    (interactive)
    (deactivate-mark)
    (comint-bol))
  (defun comint-clear-window ()
    "Clear from session window."
    (interactive)
    (delete-region (point-min)(point-max))
    (comint-send-input)
    (goto-char (point-min))
    (delete-char 1)
    (end-of-line))
  (defun comint-clear-window-keep-current-line ()
    "Clear session window, but keep current line."
    (interactive)
    (let ((col (current-column)))
      (kill-new (buffer-substring-no-properties (comint-line-beginning-position)
                                                (line-end-position)))
      (delete-region (point-min)(point-max))
      (comint-send-input)
      (goto-char (point-min))
      (delete-char 1)
      (end-of-line)
      (yank)
      (current-kill 1)
      (sleep-for 0.01)
      (move-to-column (max (- (comint-line-beginning-position) 1) col))))
  (defun comint-flush-window ()
    "Flush session window, showing only the current line."
    (interactive)
    (recenter 0))
  (defun comint-left (&optional n)
    "Move left N columns, unless at prompt."
    (interactive "p")
    (deactivate-mark)
    (if (not (= (point)(comint-line-beginning-position)))
        (backward-char n)))
  (defun comint-left-mark (&optional n)
    "Extend region left N columns, unless at prompt."
    (interactive "p")
    (if (not (= (point)(comint-line-beginning-position)))
        (region-backward-char n)))
  (defun comint-left-word (&optional n)
    "Move left N words."
    (interactive "p")
    (if (not (= (point)(comint-line-beginning-position)))
        (backward-word n)))
  (defun comint-left-word-mark (&optional n)
    "Extend region left N words."
    (interactive "p")
    (if (not (= (point)(comint-line-beginning-position)))
        (region-backward-word n)))
  (defun comint-next-or-down (&optional n)
    "Cycle history N forward if in last line, otherwise move N lines down."
    (interactive "p")
    (if (= (line-number-at-pos)(line-number-at-pos (point-max)))
        (comint-next-input n)
      (forward-line n)))
  (defun comint-previous-or-up (&optional n)
    "Cycle history N backward if in last line, otherwise move N lines up."
    (interactive "p")
    (if (= (line-number-at-pos)(line-number-at-pos (point-max)))
        (comint-previous-input n)
      (forward-line (- n))))
  (defun kill-process-or-emacs ()
    "Stop process if any is active, otherwise quit Emacs."
    (interactive)
    (if (> (length (process-list)) 0) ; any active processes?
        (let ((active-process-buffer
               (buffer-name
                (process-buffer
                 (car (last (process-list))))))) ; last process for GDB
          (cond ((string-match "^\\*gud-" active-process-buffer)
                 (switch-to-buffer active-process-buffer)
                 (comint-quit-subjob))
                ((string-match "^\\*R"   active-process-buffer)
                 (switch-to-buffer active-process-buffer)
                 (ess-quit "--no-save"))
                ((string-match "^\\*SQL" active-process-buffer)
                 (switch-to-buffer active-process-buffer)
                 (comint-send-string sql-buffer "quit\n"))
                (t (switch-to-buffer active-process-buffer)
                   (kill-process (car (last (process-list)))))))
      (save-buffers-kill-emacs)))
  (defun kill-process-or-buffer ()
    "Stop process if current buffer has one, otherwise close current buffer."
    (interactive)
    (if (get-buffer-process (current-buffer)) ; active process in this buffer?
        (cond ((string-match "^\\*R" (buffer-name (current-buffer)))
               (ess-quit "--no-save"))
              ((string-match "^\\*SQL" (buffer-name (current-buffer)))
               (comint-send-string sql-buffer "quit\n"))
              (t (kill-process (car (process-list)))))
      (kill-buffer (current-buffer)))))
(add-hook 'comint-mode-hook 'arni-comint-hook)
;;------------------
;; 7.6  Compilation
;;------------------
(defun arni-compilation-hook () ; c++ and grep
  (setq compilation-scroll-output 'first-error)
  (setq compilation-skip-to-next-location nil)
  (set-face-attribute 'compilation-column-number nil :inherit -)
  (set-face-attribute 'compilation-info nil
                      :foreground - :weight - :inherit font-lock-keyword-face)
  (set-face-attribute 'compilation-line-number nil :inherit -)
  (set-face-attribute 'compilation-warning nil
                      :foreground - :weight - :inherit font-lock-builtin-face)
  (set-face-attribute 'escape-glyph nil
                      :foreground (fg 'font-lock-comment-face) :weight -) ; ^L
  (local-unset-key [?\M-n]) ; reactivate bs-cycle-next
  (local-unset-key [?\M-p]) ; reactivate bs-cycle-previous
  (local-set-key [C-home]      'compilation-top         )
  (local-set-key [C-end]       'compilation-bottom      )
  (local-set-key [C-S-home]    'compilation-top-mark    )
  (local-set-key [C-S-end]     'compilation-bottom-mark )
  (local-set-key [?\t]         'compilation-down-view   )
  (local-set-key [backtab]     'compilation-up-view     )
  (local-set-key [up]          'compilation-up          )
  (local-set-key [down]        'compilation-down        )
  (local-set-key [M-up]        'compilation-up-view     )
  (local-set-key [M-down]      'compilation-down-view   )
  (local-set-key [?\C-c ?\C-q] 'kill-process-now        )
  (local-set-key [? ]          'compilation-view        )
  (local-set-key [?.]          'compilation-down        )
  (local-set-key [?,]          'compilation-up          )
  (local-set-key [?/]          'isearch-forward         )
  (local-set-key [?M]          'compilation-middle      )
  (local-set-key [?N]          'compilation-down-view   )
  (local-set-key [?P]          'compilation-up-view     )
  (local-set-key [?m]          'compilation-middle      )
  (local-set-key [?n]          'compilation-down        )
  (local-set-key [?p]          'compilation-up          )
  (local-set-key [?q]          'kill-buffer-maybe-window)
  (local-set-key [?v]          'compilation-view        )
  (defun compilation-bottom ()
    "Move to bottom."
    (interactive)
    (deactivate-mark)
    (goto-char (point-max))
    (compilation-up 1))
  (defun compilation-bottom-mark ()
    "Extend region to bottom."
    (interactive)
    (region-bol-bottom)
    (forward-line -2))
  (defun compilation-down (&optional n)
    "Move down N errors."
    (interactive "p")
    (deactivate-mark)
    (next-error-follow-minor-mode 0)
    (dotimes (i n)
      (compilation-next-error 1)))
  (defun compilation-down-view ()
    "Move down one line and view in other window."
    (interactive)
    (deactivate-mark)
    (next-error-follow-minor-mode t)
    (compilation-next-error 1))
  (defun compilation-middle ()
    "Move to middle."
    (interactive)
    (jump-middle)
    (compilation-down 1))
  (defun compilation-top ()
    "Move to top."
    (interactive)
    (deactivate-mark)
    (goto-char (point-min))
    (compilation-down 1))
  (defun compilation-top-mark ()
    "Extend region to top."
    (interactive)
    (region-bol-top 5))
  (defun compilation-up (&optional n)
    "Move up N errors."
    (interactive "p")
    (deactivate-mark)
    (next-error-follow-minor-mode 0)
    (dotimes (i n)
      (compilation-previous-error 1)))
  (defun compilation-up-view ()
    "Move down one line and view in other window."
    (interactive)
    (deactivate-mark)
    (next-error-follow-minor-mode t)
    (compilation-previous-error 1))
  (defun compilation-view ()
    "View in other window."
    (interactive)
    (forward-line -1)
    (compilation-down-view)))
(add-hook 'compilation-mode-hook 'arni-compilation-hook)
;;-----------------
;; 7.7  Completion
;;-----------------
(defun arni-completion-hook ()
  (local-set-key [?\t]     'next-completion     )
  (local-set-key [backtab] 'previous-completion )
  (local-set-key [?n]      'next-completion     )
  (local-set-key [?p]      'previous-completion))
(add-hook 'completion-list-mode-hook 'arni-completion-hook)
;;-----------
;; 7.8  Conf
;;-----------
(defun arni-conf-hook ()
  ;; [section]
  ;; space:   par val #comment
  ;; unix:    par=val #comment
  ;; windows: par=val par val ;comment #gadgetpar
  (setq indent-line-function 'indent-relative-definitely)
  (arni-colors)
  (set-face-attribute 'font-lock-builtin-face nil :foreground "brown4")
  (set-face-attribute 'font-lock-type-face nil
                      :foreground (fg 'font-lock-keyword-face) :weight 'bold)
  (set-face-attribute 'font-lock-variable-name-face nil
                      :foreground - :weight 'bold)
  (local-unset-key [?\C-c ? ])
  (local-set-key [?\C-c ?\C- ] 'conf-space-keywords))
(add-hook 'conf-mode-hook 'arni-conf-hook)
(defun arni-conf-windows-hook ()
  (font-lock-add-keywords
   nil '(("\\(#\\w*\\)" (1 font-lock-builtin-face)))) ; #gadgetpar
  (font-lock-add-keywords
   nil '(("^[ \t]*\\w*" .
          font-lock-variable-name-face))) ; par=val, par val, par:val, text
  (font-lock-add-keywords
   nil '(("^[ \t]*\\[\\(\\w*\\)\\]"
          (1 font-lock-type-face))))) ; reactivate [section] explicitly
(add-hook 'conf-windows-mode-hook 'arni-conf-windows-hook)
;;----------
;; 7.9  CSV
;;----------
(defvar csv-separators '(","))
(defvar csv-splash t "Whether to show initial help in minibuffer.")
(defun arni-csv-hook ()
  (load-library "csv-nav")
  (set-face-attribute 'csv-separator-face nil :foreground -)
  (setq buffer-invisibility-spec t)
  (local-set-key [?\t]         'forward-sexp-start )
  (local-set-key [backtab]     'backward-sexp-start)
  (local-set-key [M-up]        'csv-up-view        )
  (local-set-key [M-down]      'csv-down-view      )
  (local-set-key [?\C-c ?\C-/] 'csv-help           )
  (defun csv-down (&optional n)
    "Move down N lines."
    (interactive "p")
    (deactivate-mark)
    (forward-line n)
    (if (eobp)(forward-line -1)))
  (defun csv-down-view ()
    "Move down one line and view in other window."
    (interactive)
    (csv-down 1)
    (csv-view))
  (defun csv-help ()
    "Show `csv-mode' keybindings."
    (interactive)
    (let ((hints '("C-c C-/: This help" "C-c C-a: Align (visually)"
                   "C-1 C-c C-a: Align (insert spaces)"
                   "C-c C-u:  Unalign" "M-up & M-down: View"))
          (sep "       "))
      (message (concat sep (mapconcat 'eval hints sep))))
    (setq csv-splash nil))
  (defun csv-nav-parse-line ()
    "Override `csv-nav-parse-line' to support `csv-separators'."
    (let ((start (point))
          result)
      (with-syntax-table csv-nav-syntax-table
        (while start
          (skip-syntax-forward "^.\" ")
          (cond ((= (char-after)(string-to-char (car csv-separators)))
                 (setq result (cons (csv-nav-parse-field start) result))
                 (setq start (1+ (point)))
                 (forward-char 1))
                ((= (char-after) ?\n)
                 (setq result (cons (csv-nav-parse-field start) result))
                 (setq start nil)
                 (forward-char 1))
                ((= (char-after) ?\")
                 (forward-sexp 1))
                (t (forward-char 1))))
        (nreverse result))))
  (defun csv-up (&optional n)
    "Move up N lines."
    (interactive "p")
    (deactivate-mark)
    (forward-line (- n)))
  (defun csv-up-view ()
    "Move up one line and view in other window."
    (interactive)
    (csv-up 1)
    (csv-view))
  (defun csv-view ()
    "View current line in other window."
    (interactive)
    (csv-nav-mode)
    (csv-nav-edit)
    (other-window 1)
    (csv-mode)
    (message nil))
  (if csv-splash (csv-help)))
(add-hook 'csv-mode-hook 'arni-csv-hook)
(defun csv-mode-force ()
  "Edit comma-separated text file."
  (interactive)
  (if (featurep 'csv-mode)(unload-feature 'csv-mode))
  (csv-mode)) ; in case space-mode has already been called
(defun space-mode ()
  "Edit space-separated text file."
  (interactive)
  (setq csv-separators '(" "))
  (csv-mode)) ; csv-separators must be set before loading csv-mode
(defun tab-mode ()
  "Edit tab-separated text file."
  (interactive)
  (setq csv-separators '("\t"))
  (csv-mode)) ; csv-separators must be set before loading csv-mode
;;----------------
;; 7.10 Customize
;;----------------
(defun arni-Custom-hook ()
  (message nil)
  (set-face-attribute 'custom-group-tag    nil :foreground -        ) ; group
  (set-face-attribute 'custom-variable-tag nil :foreground "brown4")) ; variable
(add-hook 'Custom-mode-hook 'arni-Custom-hook)
;;-----------
;; 7.11 Diff
;;-----------
(defun arni-diff-hook ()
  (setq show-trailing-whitespace nil)
  (set-face-attribute 'diff-added nil :foreground (fg 'font-lock-keyword-face))
  (set-face-attribute 'diff-removed
                      nil :foreground (fg 'font-lock-builtin-face))
  (local-unset-key [?\M-k]) ; reactivate kill-this-buffer
  (local-unset-key [?\M-n]) ; reactivate bs-cycle-next
  (local-unset-key [?\M-o]) ; reactivate other-window
  (local-unset-key [?\M-p]) ; reactivate bs-cycle-previous
  (local-set-key [?\C-m] 'diff-goto-source     ) ; return
  (local-set-key [?k]    'diff-hunk-kill       )
  (local-set-key [?n]    'diff-hunk-next       )
  (local-set-key [?p]    'diff-hunk-prev       )
  (local-set-key [?u]    'yank                 )
  (local-set-key [?v]    'diff-goto-source-stay)
  (defun diff-goto-source-stay ()
    "View diff in source file and stay."
    (interactive)
    (let ((diff-window (selected-window)))
      (diff-goto-source)
      (select-window diff-window))))
(add-hook 'diff-mode-hook 'arni-diff-hook)
(defun arni-ediff-hook ()
  (setq auto-hscroll-mode nil)
  (setq ediff-split-window-function 'split-window-right) ; windows side by side
  (setq ediff-window-setup-function
        'ediff-setup-windows-plain) ; minibuffer control
  (set-face-attribute 'ediff-current-diff-A nil :background "salmon"
                      :foreground -) ; current line A
  (set-face-attribute 'ediff-current-diff-B nil :background "salmon"
                      :foreground -) ; current line B
  (set-face-attribute 'ediff-current-diff-C nil :background "salmon"
                      :foreground -) ; current line C
  (set-face-attribute 'ediff-even-diff-A    nil :background "gold"
                      :foreground -) ; noncurrent line A
  (set-face-attribute 'ediff-even-diff-B    nil :background "gold"
                      :foreground -) ; noncurrent line B
  (set-face-attribute 'ediff-even-diff-C    nil :background "gold"
                      :foreground -) ; noncurrent line C
  (set-face-attribute 'ediff-fine-diff-A    nil :background "rosybrown2"
                      :foreground -) ; current diff A
  (set-face-attribute 'ediff-fine-diff-B    nil :background "rosybrown2"
                      :foreground -) ; current diff B
  (set-face-attribute 'ediff-fine-diff-C    nil :background "rosybrown2"
                      :foreground -) ; current diff C
  (set-face-attribute 'ediff-odd-diff-A     nil :background "gold"
                      :foreground -) ; noncurrent line A
  (set-face-attribute 'ediff-odd-diff-B     nil :background "gold"
                      :foreground -) ; noncurrent line B
  (set-face-attribute 'ediff-odd-diff-C     nil :background "gold"
                      :foreground -) ; noncurrent line C
  (defun ediff-copy-right ()
    "Copy region from buffer A to buffer B."
    (interactive)
    (let ((line-A (with-current-buffer ediff-buffer-A (line-number-at-pos)))
          (line-B (with-current-buffer ediff-buffer-B (line-number-at-pos))))
      (ediff-copy-A-to-B nil)
      (ediff-update-diffs)
      (goto-line-lisp line-A ediff-buffer-A)
      (recenter)
      (goto-line-lisp line-B ediff-buffer-B)
      (recenter)
      (select-window (get-buffer-window "*Ediff Control Panel*"))))
  (defun ediff-copy-left ()
    "Copy region from buffer B to buffer A."
    (interactive)
    (let ((line-A (with-current-buffer ediff-buffer-A (line-number-at-pos)))
          (line-B (with-current-buffer ediff-buffer-B (line-number-at-pos))))
      (ediff-copy-B-to-A nil)
      (ediff-update-diffs)
      (goto-line-lisp line-A ediff-buffer-A)
      (recenter)
      (goto-line-lisp line-B ediff-buffer-B)
      (recenter)
      (select-window (get-buffer-window "*Ediff Control Panel*"))))
  (defun ediff-jump-last ()
    "Select last difference."
    (interactive)
    (ediff-jump-to-difference ediff-number-of-differences))
  (defun ediff-jump-middle ()
    "Select middle of all differences."
    (interactive)
    (ediff-jump-to-difference (/ ediff-number-of-differences 2)))
  (defun ediff-scroll-bottom ()
    "Scroll both windows to end of buffer."
    (interactive)
    (with-current-buffer ediff-buffer-A
      (goto-char (point-min))
      (recenter)
      (goto-char (point-max)))
    (with-current-buffer ediff-buffer-B
      (goto-char (point-min))
      (recenter)
      (goto-char (point-max)))
    (select-window (get-buffer-window "*Ediff Control Panel*")))
  (defun ediff-scroll-down (&optional n)
    "Scroll both windows N lines down."
    (interactive "p")
    (ediff-scroll-vertically (- n)))
  (defun ediff-scroll-down-3 ()
    "Scroll both windows 3 lines down."
    (interactive)
    (ediff-scroll-down 3))
  (defun ediff-scroll-down-full (&optional n)
    "Scroll both windows N pages down."
    (interactive "p")
    (ediff-scroll-down (* n (- (window-body-height ediff-window-A)
                               next-screen-context-lines))))
  (defun ediff-scroll-left (&optional n)
    "Scroll both windows N columns left."
    (interactive "p")
    (let ((cur-scroll (window-hscroll ediff-window-A))
          (remember-A ediff-window-A)
          (remember-B ediff-window-B))
      (select-window remember-A)
      (set-window-hscroll remember-A (- cur-scroll n))
      (select-window remember-B)
      (set-window-hscroll remember-B (- cur-scroll n))
      (select-window (get-buffer-window "*Ediff Control Panel*"))))
  (defun ediff-scroll-left-3 ()
    "Scroll both windows 3 columns left."
    (interactive)
    (ediff-scroll-left 3))
  (defun ediff-scroll-left-full (&optional n)
    "Scroll both windows N pages left."
    (interactive "p")
    (ediff-scroll-left (* n (- (window-width ediff-window-A)
                               next-screen-context-lines))))
  (defun ediff-scroll-right (&optional n)
    "Scroll both windows N columns left."
    (interactive "p")
    (let ((cur-scroll (window-hscroll ediff-window-A))
          (remember-A ediff-window-A)
          (remember-B ediff-window-B))
      (select-window remember-A)
      (set-window-hscroll remember-A (+ cur-scroll n))
      (select-window remember-B)
      (set-window-hscroll remember-B (+ cur-scroll n))
      (select-window (get-buffer-window "*Ediff Control Panel*"))))
  (defun ediff-scroll-right-3 ()
    "Scroll both windows 3 column left."
    (interactive)
    (ediff-scroll-right 3))
  (defun ediff-scroll-right-full (&optional n)
    "Scroll both windows N pages right."
    (interactive "p")
    (ediff-scroll-right (* n (- (window-width ediff-window-A)
                                next-screen-context-lines))))
  (defun ediff-scroll-top ()
    "Scroll both windows to beginning of buffer."
    (interactive)
    (ediff-operate-on-windows 'goto-char (point-min)))
  (defun ediff-scroll-up (&optional n)
    "Scroll both windows N lines up."
    (interactive "p")
    (ediff-scroll-vertically n))
  (defun ediff-scroll-up-3 ()
    "Scroll both windows 3 lines up."
    (interactive)
    (ediff-scroll-up 3))
  (defun ediff-scroll-up-full (&optional n)
    "Scroll both windows N pages up."
    (interactive "p")
    (ediff-scroll-up (* n (- (window-body-height ediff-window-A)
                             next-screen-context-lines))))
  (defun ediff-split-left-right ()
    "Arrange Ediff windows on left and right side of screen."
    (interactive)
    (setq ediff-split-window-function 'split-window-right)
    (ediff-recenter 'no-rehighlight))
  (defun ediff-split-top-bottom ()
    "Arrange Ediff windows on top and bottom of screen."
    (interactive)
    (setq ediff-split-window-function 'split-window-below)
    (ediff-recenter 'no-rehighlight)))
(add-hook 'ediff-mode-hook 'arni-ediff-hook)
(defun arni-ediff-cleanup-hook ()(setq auto-hscroll-mode t))
(add-hook 'ediff-cleanup-hook 'arni-ediff-cleanup-hook)
(defun arni-ediff-keymap-setup-hook ()
  (define-key ediff-mode-map [f5]    'ediff-revert-buffers-then-recompute-diffs)
  (define-key ediff-mode-map [home]     'ediff-jump-to-difference )
  (define-key ediff-mode-map [end]      'ediff-jump-last          )
  (define-key ediff-mode-map [S-home]   'ediff-jump-to-difference )
  (define-key ediff-mode-map [S-end]    'ediff-jump-last          )
  (define-key ediff-mode-map [C-home]   'ediff-scroll-top         )
  (define-key ediff-mode-map [C-end]    'ediff-scroll-bottom      )
  (define-key ediff-mode-map [C-S-home] 'ediff-scroll-top         )
  (define-key ediff-mode-map [C-S-end]  'ediff-scroll-bottom      )
  (define-key ediff-mode-map [prior]    'ediff-scroll-up-full     )
  (define-key ediff-mode-map [next]     'ediff-scroll-down-full   )
  (define-key ediff-mode-map [C-prior]  'ediff-scroll-left-full   )
  (define-key ediff-mode-map [C-next]   'ediff-scroll-right-full  )
  (define-key ediff-mode-map [?\t]      'ediff-next-difference    )
  (define-key ediff-mode-map [backtab]  'ediff-previous-difference)
  (define-key ediff-mode-map [? ]
              'ediff-toggle-skip-similar) ; ediff-next-difference
  (define-key ediff-mode-map [left]     'ediff-copy-left          )
  (define-key ediff-mode-map [right]    'ediff-copy-right         )
  (define-key ediff-mode-map [S-left]   'ediff-scroll-left        )
  (define-key ediff-mode-map [S-right]  'ediff-scroll-right       )
  (define-key ediff-mode-map [C-left]   'ediff-scroll-left-3      )
  (define-key ediff-mode-map [C-right]  'ediff-scroll-right-3     )
  (define-key ediff-mode-map [M-left]   'ediff-split-top-bottom   )
  (define-key ediff-mode-map [M-right]  'ediff-split-left-right   )
  (define-key ediff-mode-map [up]       'ediff-previous-difference)
  (define-key ediff-mode-map [down]     'ediff-next-difference    )
  (define-key ediff-mode-map [S-up]     'ediff-scroll-up          )
  (define-key ediff-mode-map [S-down]   'ediff-scroll-down        )
  (define-key ediff-mode-map [C-up]     'ediff-scroll-up-3        )
  (define-key ediff-mode-map [C-down]   'ediff-scroll-down-3      )
  (define-key ediff-mode-map [M-up]     'ediff-previous-difference)
  (define-key ediff-mode-map [M-down]   'ediff-next-difference    )
  (define-key ediff-mode-map [?\C-n]    'ediff-next-difference    )
  (define-key ediff-mode-map [?\C-p]    'ediff-previous-difference)
  (define-key ediff-mode-map [?\C-v]    'ediff-scroll-down-full   )
  (define-key ediff-mode-map [?\C-y]    'ediff-scroll-up-full     )
  (define-key ediff-mode-map [?\C-z] 'ediff-revert-buffers-then-recompute-diffs)
  (define-key ediff-mode-map [?\M-n]    'ediff-next-difference    )
  (define-key ediff-mode-map [?\M-p]    'ediff-previous-difference)
  (define-key ediff-mode-map [?\\]      'ediff-swap-buffers       )
  (define-key ediff-mode-map [?<]       'ediff-scroll-left-full   )
  (define-key ediff-mode-map [?>]       'ediff-scroll-right-full  )
  (define-key ediff-mode-map [?M]
              'ediff-jump-middle) ; ediff-show-current-session-meta-buffer
  (define-key ediff-mode-map [?N]       'ediff-next-difference    )
  (define-key ediff-mode-map [?P]       'ediff-previous-difference)
  (define-key ediff-mode-map [?U] 'ediff-revert-buffers-then-recompute-diffs)
  (define-key ediff-mode-map [?c]       'ediff-recenter           )
  (define-key ediff-mode-map [?i] 'ediff-toggle-ignore-case) ; ediff-status-info
  (define-key ediff-mode-map [?m]
              'ediff-jump-middle) ; ediff-toggle-wide-display
  (define-key ediff-mode-map [?u] 'ediff-revert-buffers-then-recompute-diffs))
(add-hook 'ediff-keymap-setup-hook 'arni-ediff-keymap-setup-hook)
(defun arni-ediff-startup-hook ()(message nil))
(add-hook 'ediff-startup-hook 'arni-ediff-startup-hook)
;;------------
;; 7.12 Dired
;;------------
(defvar dired-by nil
  "Current sorting rule (nil:name, S:size, t:time, X:extension).")
(defvar dired-dot nil
  "Non-nil if dot files are currently shown in dired.
See `dired-toggle-dot-files'.")
(defun arni-dired-hook ()
  (require 'dired-x) ; provide dired-do-find-marked-files
  (setq dired-actual-switches dired-fmt         ) ; set default dired format
  (setq dired-free-space-args "-Ph"             ) ; show GB rather than KB
  (setq dired-use-ls-dired nil                  ) ; use Lisp rather than ls
  (setq ls-lisp-dirs-first t                    ) ; sort dirs before files
  (setq ls-lisp-format-time-list
        '("%e-%b-%Y %k:%M" "%e-%b-%Y %k:%M")) ; date format for recent/old files
  (setq ls-lisp-ignore-case t                   ) ; sort files ignoring case
  (setq ls-lisp-use-insert-directory-program nil) ; use Lisp rather than ls
  (setq ls-lisp-use-localized-time-format t   ) ; force ls-lisp-format-time-list
  (setq ls-lisp-verbosity nil                 ) ; suppress links/user/group info
  (set-face-attribute 'dired-flagged nil
                      :foreground - :inherit font-lock-comment-face)
  (set-face-attribute 'dired-header  nil :underline t :inherit -       )
  (set-face-attribute 'dired-ignored nil :inherit -                    )
  (set-face-attribute 'dired-mark    nil :foreground "brown4"          )
  (set-face-attribute 'dired-marked  nil :foreground "red" :underline -)
  (set-face-attribute 'dired-warning nil :inherit -                    )
  (local-unset-key [?\M-o]) ; reactivate other-window
  (local-set-key [f9]        'dired-by-N                 )
  (local-set-key [f10]       'dired-by-S                 )
  (local-set-key [f11]       'dired-by-T                 )
  (local-set-key [f12]       'dired-by-X                 )
  (local-set-key [C-home]    'dired-top                  )
  (local-set-key [C-end]     'dired-bottom               )
  (local-set-key [C-S-home]  'dired-top-mark             )
  (local-set-key [C-S-end]   'region-bol-bottom          )
  (local-set-key [prior]     'dired-up-page              )
  (local-set-key [next]      'dired-down-page            )
  (local-set-key [backspace] 'dired-up-directory         )
  (local-set-key [?\t]       'dired-enter-or-view        )
  (local-set-key [backtab]   'dired-up-directory         )
  ;; dired-x must be explicitly loaded to run `dired-do-find-marked-files'
  (local-set-key [S-return]  'dired-do-find-marked-files )
  (local-set-key [left]      'dired-up-directory         )
  (local-set-key [right]     'dired-enter-or-view        )
  (local-set-key [S-left]    'dired-up-directory         )
  (local-set-key [S-right]   'dired-enter-or-view        )
  (local-set-key [C-left]    'dired-up-directory         )
  (local-set-key [C-right]   'dired-view-other           )
  (local-set-key [up]        'dired-up                   ) ; dired-previous-line
  (local-set-key [down]      'dired-down                 ) ; dired-next-line
  (local-set-key [S-up]      'dired-up-mark              )
  (local-set-key [S-down]    'region-bol-down            )
  (local-set-key [C-up]      'dired-up-3                 )
  (local-set-key [C-down]    'dired-down-3               )
  (local-set-key [M-up]      'dired-up-view              )
  (local-set-key [M-down]    'dired-down-view            )
  (local-set-key [?\M-s]     'dired-do-isearch-regexp    )
  (local-set-key [?\C-n]     'dired-down                 )
  (local-set-key [?\C-p]     'dired-up                   )
  (local-set-key [?\C-v]     'dired-down-page            )
  (local-set-key [?\C-y]     'dired-up-page              )
  (local-set-key [?/]        'isearch-forward            )
  (local-set-key [?*]        'dired-change-marks         ) ; [map]
  (local-set-key [?\;]       'dired-mark-files-regexp    )
  (local-set-key [?\:]       'dired-mark-files-containing-regexp) ; [map]
  (local-set-key [?<]        'dired-up-directory         ) ; dired-prev-dirline
  (local-set-key [?>]        'dired-enter-or-view        ) ; dired-next-dirline
  (local-set-key [?,]        'dired-up-directory         )
  (local-set-key [?.]        'dired-enter-or-view      ) ; dired-clean-directory
  (local-set-key [?\C-.]     'dired-toggle-dot           )
  (local-set-key [?C]        'dired-compare-directories  ) ; dired-do-copy
  (local-set-key [?D]        'dired-mark-directories     ) ; dired-do-delete
  (local-set-key [?E]        'dired-do-query-replace-regexp)
  (local-set-key [?F]        'dired-mark-files-containing-regexp)
  (local-set-key [?K]        'dired-create-directory     )
  (local-set-key [?M]        'dired-middle               ) ; dired-do-chmod
  (local-set-key [?N]        'dired-down-view            )
  (local-set-key [?P]        'dired-up-view              )
  (local-set-key [?R]        'dired-do-rename-regexp     ) ; dired-do-rename
  (local-set-key [?S]        'dired-do-isearch           ) ; dired-do-symlink
  (local-set-key [?V]        'dired-view-file            ) ; dired-do-run-mail
  (local-set-key [?a]        'dired-do-chmod       ) ; dired-find-alternate-file
  (local-set-key [?b]        'dired-do-byte-compile      )
  (local-set-key [?c]        'dired-do-copy              )
  (local-set-key [?e]        'dired-mark-executables     ) ; dired-find-file
  (local-set-key [?f]        'dired-mark-files-regexp    ) ; dired-find-file
  (local-set-key [?g]       'dired-mark-files-containing-regexp) ; revert-buffer
  (local-set-key [?h]        'dired-do-kill-lines        ) ; describe-mode
  (local-set-key [?i]        'dired-unmark-up      ) ; dired-maybe-insert-subdir
  (local-set-key [?l]        'dired-downcase             ) ; dired-do-redisplay
  (local-set-key [?m]        'dired-mark-down            ) ; dired-mark
  (local-set-key [?n]        'dired-down                 ) ; dired-next-line
  (local-set-key [?o]        'dired-view-other  ) ; dired-find-file-other-window
  (local-set-key [?p]        'dired-up                   ) ; dired-previous-line
  (local-set-key [?q]        'dired-quit                 ) ; quit-window
  (local-set-key [?r]        'dired-do-rename            )
  (local-set-key [?u]        'dired-unmark-down          ) ; dired-unmark
  (local-set-key [?v]        'dired-view-other           ) ; dired-view-file
  (local-set-key [?z]        'dired-do-compress          )
  (defun dired-bottom ()
    "Move to bottom."
    (interactive)
    (deactivate-mark)(goto-char (point-max))(dired-previous-line 1))
  (defun dired-by-X ()
    "Sort files by filename extension."
    (interactive)
    (setq dired-by "X")
    (setq dired-actual-switches (concat dired-fmt (if dired-dot "A") dired-by))
    (revert-buffer)
    (message "Sorted by EXTENSION"))
  (defun dired-by-N ()
    "Sort files by name."
    (interactive)
    (setq dired-by nil)
    (setq dired-actual-switches (concat dired-fmt (if dired-dot "A") dired-by))
    (revert-buffer)
    (message "Sorted by NAME"))
  (defun dired-by-S ()
    "Sort files by size."
    (interactive)
    (setq dired-by "S")
    (setq dired-actual-switches (concat dired-fmt (if dired-dot "A") dired-by))
    (revert-buffer)
    (message "Sorted by SIZE"))
  (defun dired-by-T ()
    "Sort files by modification time."
    (interactive)
    (setq dired-by "t")
    (setq dired-actual-switches (concat dired-fmt (if dired-dot "A") dired-by))
    (revert-buffer)
    (message "Sorted by TIME"))
  (defun dired-down (&optional n)
    "Move down N lines."
    (interactive "p")
    (deactivate-mark)
    (forward-line n)
    (if (< (line-number-at-pos) 3)
        (dired-top)
      (dired-move-to-filename))
    (if (eobp)(dired-bottom)))
  (defun dired-down-3 ()
    "Move down 3 lines."
    (interactive)
    (dired-down 3))
  (defun dired-down-page (&optional n)
    "Move down N pages."
    (interactive "p")
    (dired-down (pages n)))
  (defun dired-down-view ()
    "Move down one line and view in other window."
    (interactive)
    (dired-down 1)
    (dired-view-other))
  (defun dired-enter-or-view ()
    "Enter directory, or view file in other window."
    (interactive)
    (if (file-directory-p (dired-get-file-for-visit))
        (dired-find-file)
      (dired-view-other)))
  (defun dired-mark-down (&optional n)
    "Mark N items and move to next line."
    (interactive "p")
    (dired-mark n)
    (if (eobp)(dired-bottom)))
  (defun dired-middle ()
    "Move to middle."
    (interactive)
    (dired-top)
    (dired-next-line (middle-from-here)))
  (defun dired-quit ()
    "Quit dired."
    (interactive)
    (dolist (B dired-buffers)
      (kill-buffer (cdr B)))
    (delete-other-windows))
  (defun dired-toggle-dot ()
    "Toggle whether dot files are shown in dired."
    (interactive)
    (setq dired-dot (not dired-dot))
    (setq dired-actual-switches (concat dired-fmt (if dired-dot "A") dired-by))
    (revert-buffer)
    (message "Dot files %s" (if dired-dot "ON" "OFF")))
  (defun dired-top ()
    "Move to top."
    (interactive)
    (deactivate-mark)
    (goto-line-lisp 2)
    (dired-next-line 1))
  (defun dired-top-mark ()
    "Extend region to top."
    (interactive)
    (region-bol-top 3))
  (defun dired-unmark-down (&optional n)
    "Unmark N items and move to next line."
    (interactive "p")
    (dired-unmark n)
    (dired-up 1)
    (dired-down 1))
  (defun dired-unmark-up (&optional n)
    "Unmark N items and move to previous line."
    (interactive "p")
    (dotimes (i n)
      (dired-unmark 1)
      (dired-up 2)))
  (defun dired-up (&optional n)
    "Move up N lines."
    (interactive "p")
    (deactivate-mark)
    (forward-line (- n))
    (if (< (line-number-at-pos) 3)
        (dired-top)
      (dired-move-to-filename)))
  (defun dired-up-3 ()
    "Move up 3 lines."
    (interactive)
    (dired-up 3))
  (defun dired-up-mark (&optional n)
    "Extend region up N lines."
    (interactive "p")
    (region-bol-up n)
    (if (< (line-number-at-pos) 3)
        (dired-top-mark)))
  (defun dired-up-page (&optional n)
    "Move up N pages."
    (interactive "p")
    (dired-up (pages n)))
  (defun dired-up-view ()
    "Move up one line and view in other window."
    (interactive)
    (dired-up 1)
    (dired-view-other))
  (defun dired-view-other ()
    "View file or directory in other window."
    (interactive)
    (if (one-window-p)(split-window-right))
    (dired-display-file)))
(add-hook 'dired-mode-hook 'arni-dired-hook)
;;--------------
;; 7.13 DocView
;;--------------
(defun arni-doc-view-hook ()
  (local-set-key [home]    'doc-view-first-page                  )
  (local-set-key [C-home]  'doc-view-first-page                  )
  (local-set-key [prior]   'doc-view-scroll-down-or-previous-page)
  (local-set-key [C-prior] 'doc-view-previous-page               )
  (local-set-key [next]    'doc-view-scroll-up-or-next-page      )
  (local-set-key [C-next]  'doc-view-next-page                   )
  (local-set-key [end]     'doc-view-last-page                   )
  (local-set-key [C-end]   'doc-view-last-page                   )
  (local-set-key [left]    'image-backward-hscroll               )
  (local-set-key [C-left]  'scroll-right                         )
  (local-set-key [right]   'image-forward-hscroll                )
  (local-set-key [C-right] 'scroll-left                          )
  (local-set-key [up]      'image-previous-line                  )
  (local-set-key [down]    'image-next-line                      )
  (local-set-key [?\C--]   'doc-view-shrink                      )
  (local-set-key [?\C-=]   'doc-view-enlarge                     )
  (local-set-key [?\C-v]   'doc-view-scroll-up-or-next-page      )
  (local-set-key [?\C-y]   'doc-view-scroll-down-or-previous-page)
  (local-set-key [?0]      'doc-view-100-dpi                     )
  (local-set-key [?1]      'doc-view-200-dpi                     )
  (local-set-key [?2]      'doc-view-400-dpi                     )
  (local-set-key [?=]      'doc-view-enlarge                     )
  (local-set-key [?g]      'doc-view-goto-page                   )
  (defun doc-view-100-dpi ()
    "View document at 100 dpi."
    (interactive)
    (setq doc-view-resolution 100)
    (doc-view-enlarge 1))
  (defun doc-view-200-dpi ()
    "View document at 200 dpi."
    (interactive)
    (setq doc-view-resolution 200)
    (doc-view-enlarge 1))
  (defun doc-view-400-dpi ()
    "View document at 400 dpi."
    (interactive)
    (setq doc-view-resolution 400)
    (doc-view-enlarge 1)))
(add-hook 'doc-view-mode-hook 'arni-doc-view-hook)
;;-------------
;; 7.14 Finder
;;-------------
(defun arni-finder-hook ()
  (local-set-key [?v] 'finder-view)
  (defun finder-view ()
    "View keywords within a finder category."
    (interactive)
    (finder-select)
    (finder-list-keywords)
    (balance-windows)))
(add-hook 'finder-mode-hook 'arni-finder-hook)
;;-----------
;; 7.15 Grep
;;-----------
(defun arni-grep-hook ()
  (other-window 1)
  (delete-other-windows)) ; inherits compilation-mode-hook
(add-hook 'grep-setup-hook 'arni-grep-hook)
;;-----------
;; 7.16 Help
;;-----------
(defun arni-help-hook ()
  (view-mode 0)
  (set-face-attribute 'escape-glyph nil
                      :foreground (fg 'font-lock-comment-face) :weight -) ; ^L
  (local-set-key [backspace] 'help-go-back            )
  (local-set-key [?\t]       'forward-button          )
  (local-set-key [backtab]   'backward-button         )
  (local-set-key [M-up]      'help-jump-up            )
  (local-set-key [M-down]    'help-jump-down          )
  (local-set-key [?/]        'isearch-forward         )
  (local-set-key [?M]        'jump-middle             )
  (local-set-key [?N]        'help-jump-down          )
  (local-set-key [?P]        'help-jump-up            )
  (local-set-key [?m]        'jump-middle             )
  (local-set-key [?n]        'next-line               )
  (local-set-key [?p]        'previous-line           )
  (local-set-key [?q]        'kill-buffer-maybe-window)
  (defun help-jump-down (&optional n)
    "Move N nodes forward."
    (interactive "p")
    (deactivate-mark)
    (forward-button n))
  (defun help-jump-up (&optional n)
    "Move N nodes backward."
    (interactive "p")
    (deactivate-mark)
    (backward-button n)))
(add-hook 'help-mode-hook 'arni-help-hook)
;;-----------
;; 7.17 Hexl
;;-----------
(defun arni-hexl-hook ()
  (local-unset-key [?\M-c]) ; reactivate copy-line-or-region
  (local-set-key [?\t]         'ignore                  )
  (local-set-key [backtab]     'ignore                  )
  (local-set-key [?\C-c ?\C-d] 'hexl-insert-decimal-char)
  (local-set-key [?\C-c ?\C-q] 'hexl-mode-exit          )
  (local-set-key [?\C-c ?\C-s] 'hexl-insert-hex-string  )
  (local-set-key [?\C-c ?\C-x] 'hexl-insert-hex-char   ))
(add-hook 'hexl-mode-hook 'arni-hexl-hook)
;;---------------
;; 7.18 Hideshow
;;---------------
(defun arni-hs-hook ()
  (hs-hide-all)
  (message nil)
  (setq hs-isearch-open t)
  (define-key hs-minor-mode-map [mouse-1] 'hs-mouse-select)
  (define-key hs-minor-mode-map [?\C-m]   'hs-minor-mode  ) ; return
  (define-key hs-minor-mode-map [escape]  'hs-minor-mode  )
  (defun hs-mouse-select ()
    "Select position and turn off hs-minor-mode."
    (interactive)
    (hs-minor-mode 0)
    (beginning-of-line)))
(add-hook 'hs-minor-mode-hook 'arni-hs-hook)
;;----------------
;; 7.19 Highlight
;;----------------
(defun arni-hi-lock-hook ()
  (define-key hi-lock-map [?\C-x ?w] nil)) ; reactivate widen
(add-hook 'hi-lock-mode-hook 'arni-hi-lock-hook)
(defun arni-highlight-changes-hook ()
  (message " ") ; (message nil)
  (if highlight-changes-mode
      (progn (local-set-key [M-left]  'highlight-changes-rotate-faces   )
             (local-set-key [M-right] 'highlight-changes-rotate-faces   )
             (local-set-key [M-up]    'highlight-changes-previous-change)
             (local-set-key [M-down]  'highlight-changes-next-change    )
             (set-face-attribute 'highlight-changes
                                 nil :background "gray70" :foreground - )
             (set-face-attribute 'highlight-changes-delete
                                 nil :background "gray70" :foreground -
                                 :underline nil)
             (set-face-attribute 'highlight-changes-1
                                 nil :background "gray77" :foreground - )
             (set-face-attribute 'highlight-changes-2
                                 nil :background "gray77" :foreground - )
             (set-face-attribute 'highlight-changes-3
                                 nil :background "gray77" :foreground - )
             (set-face-attribute 'highlight-changes-4
                                 nil :background "gray77" :foreground - )
             (set-face-attribute 'highlight-changes-5
                                 nil :background "gray77" :foreground - )
             (set-face-attribute 'highlight-changes-6
                                 nil :background "gray77" :foreground -))
    (progn (local-unset-key [M-left]     )
           (local-unset-key [M-right]    )
           (local-unset-key [M-up]       )
           (local-unset-key [M-down]     )
           (local-unset-key [?\C-c ?-]))))
(add-hook 'highlight-changes-mode-hook 'arni-highlight-changes-hook)
;;------------
;; 7.20 Image
;;------------
(defun arni-image-hook ()
  (local-set-key [?\C-c ?\C-v] 'browse-url-of-file))
(add-hook 'image-mode-hook 'arni-image-hook)
;;-----------
;; 7.21 Info
;;-----------
(defalias 'info-buffer 'Info-on-current-buffer)
(defun arni-Info-hook ()
  (set-face-attribute 'info-menu-star nil :foreground -)
  (local-unset-key [?\M-n]) ; reactivate bs-cycle-next
  (local-set-key [?\C-c ?\C-c] 'text-mode           )
  (local-set-key [home]        'Info-top-node       )
  (local-set-key [end]         'Info-final-node     )
  (local-set-key [S-home]      'Info-top-node       )
  (local-set-key [S-end]       'Info-final-node     )
  (local-set-key [C-home]      'Info-top            )
  (local-set-key [C-S-home]    'Info-top-mark       )
  (local-set-key [C-S-end]     'region-bol-bottom   )
  (local-set-key [prior]       'Info-up-page        )
  (local-set-key [next]        'Info-down-page      )
  (local-set-key [backspace]   'Info-history-back   )
  (local-set-key [?\t]         'Info-next-reference )
  (local-set-key [backtab]     'Info-prev-reference )
  (local-set-key [left]        'Info-history-back   )
  (local-set-key [right]       'Info-enter          )
  (local-set-key [S-left]      'Info-up-bounded     )
  (local-set-key [S-right]     'next-line           )
  (local-set-key [C-left]      'Info-forward-node   )
  (local-set-key [C-right]     'Info-backward-node  )
  (local-set-key [M-left]      'Info-history-back   )
  (local-set-key [M-right]     'Info-history-forward)
  (local-set-key [up]          'Info-jump-up        )
  (local-set-key [down]        'Info-jump-down      )
  (local-set-key [S-up]        'Info-up-mark        )
  (local-set-key [S-down]      'region-bol-down     )
  (local-set-key [C-up]        'scroll-up-3         )
  (local-set-key [C-down]      'scroll-down-3       )
  (local-set-key [M-up]        'Info-backward-node  )
  (local-set-key [M-down]      'Info-forward-node   )
  (local-set-key [?\C-n]       'Info-jump-down      )
  (local-set-key [?\C-p]       'Info-jump-up        )
  (local-set-key [?\C-v]       'Info-down-page      )
  (local-set-key [?\C-y]       'Info-up-page        )
  (local-set-key [?,]          'Info-history-back   )
  (local-set-key [?<]          'Info-history-back   )
  (local-set-key [?.]          'Info-enter          )
  (local-set-key [?>]          'Info-history-forward)
  (local-set-key [?/]          'Info-menu           )
  (local-set-key [?H]          'Info-history        )
  (local-set-key [?M]          'Info-middle         )
  (local-set-key [?N]          'Info-forward-node   )
  (local-set-key [?P]          'Info-backward-node  )
  (local-set-key [?h]          'Info-history        )
  (local-set-key [?n]          'Info-jump-down      )
  (local-set-key [?p]          'Info-jump-up        )
  (local-set-key [?q]          'kill-this-buffer    )
  (local-set-key [?þ]          'Info-menu           )
  (defun Info-enter ()
    "Enter node."
    (interactive)
    (Info-follow-nearest-node)
    (forward-line 2))
  (defun Info-down-page (&optional n)
    "Move down N pages."
    (interactive "p")
    (deactivate-mark)
    (forward-line (pages n)))
  (defun Info-jump-down ()
    "Jump one node down on page."
    (interactive)
    (deactivate-mark)
    (Info-next-reference))
  (defun Info-jump-up ()
    "Jump one node up on page."
    (interactive)
    (deactivate-mark)
    (Info-prev-reference))
  (defun Info-middle ()
    "Move to middle."
    (interactive)
    (Info-top)
    (forward-line (middle-from-here)))
  (defun Info-top ()
    "Move to top."
    (interactive)
    (deactivate-mark)
    (goto-line-lisp 3))
  (defun Info-top-mark ()
    "Extend region to top."
    (interactive)
    (region-bol-top 1)
    (forward-line 2))
  (defun Info-up-bounded (&optional n)
    "Move N lines up."
    (interactive "p")
    (forward-line -1)
    (if (< (line-number-at-pos) 3)
        (Info-top)))
  (defun Info-up-mark (&optional n)
    "Extend region up N lines."
    (interactive "p")
    (region-bol-up n)
    (if (< (line-number-at-pos) 3)
        (Info-top-mark)))
  (defun Info-up-page (&optional n)
    "Move up N pages."
    (interactive "p")
    (deactivate-mark)
    (forward-line (- (pages n)))
    (if (< (line-number-at-pos) 3)
        (Info-top))))
(add-hook 'Info-mode-hook 'arni-Info-hook)
;;------------
;; 7.22 Magit
;;------------
(defun arni-magit-hook ()
  (message nil)
  (add-to-list 'safe-local-variable-values ; suppress warning when viewing a
               '(git-commit-major-mode . git-commit-elisp-text-mode)) ; commit
  (set-face-attribute 'magit-diff-added nil
                      :background "#bbeebb" :foreground "gray50")
  (set-face-attribute 'magit-diff-added-highlight nil
                      :background "lightgreen" :foreground "darkgreen")
  (set-face-attribute 'magit-diff-context-highlight nil
                      :background - :foreground "gray60")
  (set-face-attribute 'magit-diff-context nil :foreground "gray60")
  (set-face-attribute 'magit-diff-removed nil
                      :background "#ffbbaa" :foreground "gray50")
  (set-face-attribute 'magit-diff-removed-highlight nil
                      :background "lightsalmon" :foreground "darkred")
  (local-unset-key [?\M-n]) ; reactivate bs-cycle-next
  (local-unset-key [?\M-p]) ; reactivate bs-cycle-previous
  (local-set-key [f5] 'magit-refresh                 ) ; revert-buffer
  (local-set-key [?E] 'magit-ediff-dwim              ) ; magit-ediff [map]
  (local-set-key [?N] 'magit-section-forward-sibling )
  (local-set-key [?P] 'magit-section-backward-sibling) ; magit-push [map]
  (local-set-key [?V] 'magit-show-commit-other-window) ; magit-revert [map]
  (local-set-key [?d] 'magit-diff-dwim               ) ; magit-diff [map]
  (local-set-key [?e] 'magit-ediff-current-edits     ) ; magit-ediff-dwim
  (local-set-key [?l] 'magit-log-current             ) ; magit-log [map]
  (local-set-key [?t] 'magit-show-refs               ) ; magit-tag [map]
  (local-set-key [?v] 'magit-diff-other-window       ) ; magit-reverse
  (define-key magit-diff-mode-map [?=] 'magit-diff-more-context)
  (define-key magit-file-section-map [?a] 'magit-stage)
  (defun magit-diff-other-window ()
    "Show diff in other window."
    (interactive)
    (save-selected-window (magit-diff-dwim)))
  (defun magit-ediff-current-edits ()
    "Compare current edits with HEAD."
    (interactive)
    (magit-ediff-show-working-tree (magit-file-at-point t t)))
  (defun magit-show-commit-other-window ()
    "Show commit in other window."
    (interactive)
    (save-selected-window (magit-show-commit (magit-commit-at-point)))))
(add-hook 'magit-mode-hook 'arni-magit-hook)
;;----------
;; 7.23 Man
;;----------
(defun arni-nroff-hook ()
  (font-lock-mode 1)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'bold))
(add-hook 'nroff-mode-hook 'arni-nroff-hook)
(defvar Man-width 65)
(defun arni-Man-hook ()
  (message nil)
  (setq Man-notify-method 'bully) ; maximize window
  (local-unset-key [?\M-n]) ; reactivate bs-cycle-next
  (local-unset-key [?\M-p]) ; reactivate bs-cycle-previous
  (local-set-key [?\t]     'Man-next-section     )
  (local-set-key [backtab] 'Man-previous-section )
  (local-set-key [M-up]    'Man-previous-section )
  (local-set-key [M-down]  'Man-next-section     )
  (local-set-key [?M]      'jump-middle          )
  (local-set-key [?N]      'Man-next-section     )
  (local-set-key [?P]      'Man-previous-section))
(add-hook 'Man-mode-hook 'arni-Man-hook)
(defun arni-woman-hook ()
  (set-face-attribute 'woman-italic nil :inherit - :underline t))
(add-hook 'woman-pre-format-hook 'arni-woman-hook)
;;---------------
;; 7.24 Markdown
;;---------------
(defun arni-markdown-hook ()
  (setq make-backup-files t)
  (font-lock-mode 1) ; refresh
  (set-face-attribute 'markdown-code-face nil :background -
                      :foreground "brown4")
  (set-face-attribute 'markdown-italic-face nil :inherit -)
  (set-face-attribute 'markdown-pre-face    nil :inherit -)
  (local-unset-key [backtab]      )
  (local-unset-key [S-tab]        )
  (local-unset-key [S-iso-lefttab])
  (local-unset-key [M-left]       )
  (local-unset-key [M-right]      )
  (local-unset-key [M-up]         )
  (local-unset-key [M-down]       )
  (local-unset-key [?\C-c left]   )
  (local-unset-key [?\C-c right]  )
  (local-unset-key [?\C-c ?-]     )
  (local-unset-key [?\M-n]        )
  (local-unset-key [?\M-p]        )
  (local-set-key [f10]               'markdown-tidy       )
  (local-set-key [C-f12]             'markdown-template   )
  (local-set-key [?\C-c C-backspace] 'markdown-delete-html)
  (local-set-key [?\C-c return]      'markdown-shifttab   )
  (local-set-key [?\C-c ?\C-a]       'markdown-preview    )
  (local-set-key [?\C-c ?\C-c]       'markdown-compile    )
  (local-set-key [?\C-c ?\C-m]       'markdown-shifttab   )
  (local-set-key [?\C-c ?\C-v]       'markdown-view       )
  (local-set-key [?\C-c ?\C-z]       'markdown-peek       )
  (defun markdown-compile ()
    "Convert Markdown or R Markdown document to HTML."
    (interactive)
    (if (string-equal (downcase (file-name-extension buffer-file-name)) "rmd")
        (markdown-compile-rmd)
      (markdown-compile-md))
    (maximize-window-top))
  (defun markdown-compile-md ()
    "Convert Markdown document to HTML."
    (interactive)
    (save-buffer)
    (if (one-window-p)(split-window-right))
    (compile (concat "pandoc " buffer-file-name " > "
                     (file-name-sans-extension buffer-file-name) ".html")))
  (defun markdown-compile-rmd ()
    "Convert R Markdown document to HTML."
    (interactive)
    (save-buffer)
    (if (one-window-p)(split-window-right))
    (compile (concat "render " buffer-file-name)))
  (defun markdown-delete-html ()
    "Delete HTML file corresponding to Markdown file."
    (interactive)
    (let ((html-file
           (concat (file-name-sans-extension buffer-file-name) ".html")))
      (delete-file html-file)
      (message "Deleted %s" html-file)))
  (defun markdown-peek ()
    "Open HTML file in secondary window."
    (interactive)
    (let ((html-file
           (concat (file-name-sans-extension buffer-file-name) ".html"))
          (md-window (selected-window)))
      (if (not (file-regular-p html-file))
          (error "%s not found" html-file)
        (if (one-window-p)(split-window-right))
        (find-file-noselect html-file)
        (set-window-buffer (next-window) html-file)
        (select-window md-window))))
  (defun markdown-preview ()
    "Preview GitHub Markdown."
    (interactive)
    (save-buffer)
    (if (one-window-p)(split-window-right))
    (compile (concat "github-preview " buffer-file-name)))
  (defun markdown-template ()
    "Insert Markdown template."
    (interactive "*")
    (goto-char (point-min))
    (insert "\
Section
=======

Subsection
----------

### Subsubsection

*italic*, **bold**, `monospace`

A [link](http://example.com)

- bullet
* also bullet

1. item

line\\
break
"))
  (defun markdown-tidy ()
    "Validate HTML document with Tidy."
    (interactive)
    (let ((html-file
           (concat (file-name-sans-extension buffer-file-name) ".html")))
      (if (one-window-p)(split-window))
      (get-buffer-create "*Shell Command Output*")
      (with-current-buffer "*Shell Command Output*"
        (delete-region (point-min)(point-max))
        (shell-command (concat "tidy -e -utf8 " html-file))
        (delete-trailing-spc-tab-m)
        (message nil))
      (set-window-buffer (next-window) "*Shell Command Output*")))
  (defun markdown-view ()
    "View HTML file with same prefix as current Markdown document."
    (interactive)
    (browse-url (concat (file-name-sans-extension (buffer-file-name))
                        ".html"))))
(add-hook 'markdown-mode-hook 'arni-markdown-hook)
;;------------
;; 7.25 Occur
;;------------
(defun arni-occur-hook ()
  (local-unset-key [?\M-n]) ; reactivate bs-cycle-next
  (local-unset-key [?\M-p]) ; reactivate bs-cycle-previous
  (local-set-key [C-home]   'occur-top                    )
  (local-set-key [C-end]    'occur-bottom                 )
  (local-set-key [C-S-home] 'occur-top-mark               )
  (local-set-key [C-S-end]  'region-bol-bottom            )
  (local-set-key [prior]    'occur-up-page                )
  (local-set-key [next]     'occur-down-page              )
  (local-set-key [?\t]      'occur-down                   )
  (local-set-key [backtab]  'occur-backtab                )
  (local-set-key [up]       'occur-up                     )
  (local-set-key [down]     'occur-down                   )
  (local-set-key [S-up]     'occur-up-mark                )
  (local-set-key [S-down]   'region-bol-down              )
  (local-set-key [M-up]     'occur-up-view                )
  (local-set-key [M-down]   'occur-down-view              )
  (local-set-key [C-up]     'occur-up-3                   )
  (local-set-key [C-down]   'occur-down-3                 )
  (local-set-key [?\C-n]    'occur-down                   )
  (local-set-key [?\C-p]    'occur-up                     )
  (local-set-key [?\C-v]    'occur-down-page              )
  (local-set-key [?\C-y]    'occur-up-page                )
  (local-set-key [? ]       'occur-mode-display-occurrence)
  (local-set-key [?/]       'isearch-forward              )
  (local-set-key [?M]       'occur-middle                 )
  (local-set-key [?N]       'occur-down-view              )
  (local-set-key [?P]       'occur-up-view                )
  (local-set-key [?m]       'occur-middle                 )
  (local-set-key [?n]       'occur-down                   )
  (local-set-key [?p]       'occur-up                     )
  (local-set-key [?q]       'kill-buffer-maybe-window     )
  (local-set-key [?v]       'occur-mode-display-occurrence)
  (defun occur-bottom ()
    "Move to bottom."
    (interactive)
    (deactivate-mark)
    (goto-char (point-max))
    (occur-prev))
  (defun occur-down (&optional n)
    "Move down N occurrences."
    (interactive "p")
    (deactivate-mark)
    (occur-next n))
  (defun occur-down-3 ()
    "Move down 3 occurrences."
    (interactive)
    (occur-down 3))
  (defun occur-down-page (&optional n)
    "Move down N pages."
    (interactive "p")
    (occur-down (pages n)))
  (defun occur-down-view ()
    "Move down one occurrence and view in other window."
    (interactive)
    (occur-down 1)
    (occur-mode-display-occurrence))
  (defun occur-middle ()
    "Move to middle."
    (interactive)
    (jump-middle)
    (occur-down 1))
  (defun occur-top ()
    "Move to top."
    (interactive)
    (deactivate-mark)
    (goto-char (point-min))
    (occur-next))
  (defun occur-top-mark ()
    "Extend region to top."
    (interactive)
    (region-bol-top 2))
  (defun occur-up (&optional n)
    "Move up N occurrences."
    (interactive "p")
    (deactivate-mark)
    (occur-prev n))
  (defun occur-up-3 ()
    "Move up 3 occurrences."
    (interactive)
    (occur-prev 3))
  (defun occur-up-mark (&optional n)
    "Extend region up N lines."
    (interactive "p")
    (region-bol-up n)
    (if (< (line-number-at-pos) 2)
        (occur-top-mark)))
  (defun occur-up-page (&optional n)
    "Move up N pages."
    (interactive "p")
    (occur-up (pages n)))
  (defun occur-up-view ()
    "Move up one occurrence and view in other window."
    (interactive)
    (occur-up 1)
    (occur-mode-display-occurrence)))
(defun arni-occur-switch-hook ()
  (other-window 1)
  (occur-down 1))
(add-hook 'occur-mode-hook 'arni-occur-hook)
(add-hook 'occur-hook 'arni-occur-switch-hook)
;;----------
;; 7.26 Org
;;----------
(defun arni-org-hook ()
  (setq make-backup-files t)
  (font-lock-mode 1)
  (outline-show-all)
  (setq fill-column 72)
  (setq org-cycle-global-at-bob t)
  (setq comment-start ": ")
  (org-remove-from-invisibility-spec '(org-link))
  (setq org-descriptive-links nil) ; [[.]] from `org-toggle-link-display'
  (setq org-export-with-section-numbers 2) ; enumerate heading level 1 and 2
  (setq org-export-with-sub-superscripts nil) ; export _ and ^ to HTML as is
  (setq org-export-with-toc 2) ; create TOC with heading level 1 and 2
  (setq org-footnote-section nil) ; so section named Footnotes won't be ignored
  (setq org-html-doctype "html5")
  (setq org-html-head
        (concat "<link rel=\"stylesheet\" href=\"file:///"
                (replace-regexp-in-string
                 " " "%20" (expand-file-name "~/emacs/css/org.css"))
                "\">"))
  (setq org-html-head-include-default-style nil) ; no default style
  (setq org-html-head-include-scripts nil) ; no JavaScript
  (setq org-html-postamble nil) ; no info stamp at end
  (set-face-attribute 'font-lock-comment-face
                      nil :foreground "brown4"      ) ; # comment
  (set-face-attribute 'org-level-1 nil :underline t :weight 'bold)
  (set-face-attribute 'org-level-3 nil :foreground "turquoise4"  )
  (set-face-attribute 'org-level-4 nil :foreground "forestgreen" )
  (defun org-arni-keybindings ()
    ;; Need function to set keybindings after calling `org-html-export-to-html'
    "Set keybindings for `org-mode'."
    (interactive)
    (local-unset-key [mouse-1]  ) ; forget outline-mode
    (local-unset-key [escape]   ) ; forget outline-mode
    (local-unset-key [tab]      ) ; forget outline-mode
    (local-unset-key [backtab]  ) ; unindent, rather than visibility
    (local-unset-key [S-tab]    ) ; unindent, rather than visibility
    (local-unset-key [C-return] ) ; recentf files, rather than heading
    (local-unset-key [S-up]     ) ; extend region, rather than timestamp/item
    (local-unset-key [S-down]   ) ; extend region, rather than timestamp/item
    (local-unset-key [C-up]     ) ; backward, rather than heading
    (local-unset-key [C-down]   ) ; forward, rather than heading
    (local-unset-key [C-S-up]   ) ; pull line/region, rather than timestamp
    (local-unset-key [C-S-down] ) ; pull line/region, rather than timestamp
    (local-unset-key [M-up]     ) ; scroll other, rather than subtree
    (local-unset-key [M-down]   ) ; scroll other, rather than subtree
    (local-unset-key [left]     ) ; forget outline-mode
    (local-unset-key [right]    ) ; forget outline-mode
    (local-unset-key [S-left]   ) ; extend region, rather than todo/keyword/date
    (local-unset-key [S-right]  ) ; extend region, rather than todo/keyword/date
    (local-unset-key [C-S-left] ) ; extend region, rather than todo
    (local-unset-key [C-S-right]) ; extend region, rather than todo
    (local-unset-key [M-left]   ) ; split window, rather than heading
    (local-unset-key [M-right]  ) ; split window, rather than heading
    (local-unset-key [?\C-c ?=] ) ; diff, rather than table formula
    (local-unset-key [?\C-c ?.] ) ; comment, rather than timestamp
    (local-unset-key [?\C-c ?/] ) ; which function, rather than sparse tree
    (local-unset-key [?\C-c ? ] ) ; hl-line-mode, rather than table blank field
    (local-unset-key [?\C-']    ) ; forward, rather than agenda
    (local-unset-key [?\C-,]    ) ; forward, rather than agenda
    (local-unset-key [?\C-j]    ) ; fill paragraph, rather than return
    (local-unset-key [?\C-k]    ) ; default kill, rather than org-kill
    (local-unset-key [?\C-y]    ) ; scroll, rather than yank
    (local-unset-key [?\C-\M-\]]) ; scroll, rather than subtree
    (local-unset-key [?\C-\M-'] ) ; scroll, rather than subtree
    (local-unset-key [?\C-\M-t] ) ; transpose-sexps, rather than org-transpose
    (local-unset-key [?\M-{]    ) ; backward-paragraph, rather than org-backward
    (local-unset-key [?\M-}]    ) ; forward-paragraph, rather than org-forward
    (local-unset-key [?\M-a]    ) ; mark buffer, rather than backward
    (local-unset-key [?\M-e]    ) ; replace, rather than forward
    (local-unset-key [?\M-h]    ) ; highlight, rather than mark element
    (local-unset-key [?,]       ) ; forget outline-mode
    (local-unset-key [?.]       ) ; forget outline-mode
    (local-unset-key [?<]       ) ; forget outline-mode
    (local-unset-key [?>]       ) ; forget outline-mode
    (local-unset-key [?M]       ) ; forget outline-mode
    (local-unset-key [?N]       ) ; forget outline-mode
    (local-unset-key [?P]       ) ; forget outline-mode
    (local-unset-key [?l]       ) ; forget outline-mode
    (local-unset-key [?m]       ) ; forget outline-mode
    (local-unset-key [?n]       ) ; forget outline-mode
    (local-unset-key [?p]       ) ; forget outline-mode
    (local-unset-key [?q]       ) ; forget outline-mode
    (local-set-key [M-mouse-1]     'org-mouse-show    )
    (local-set-key [M-mouse-3]     'org-mouse-cycle   )
    (local-set-key [f11]           'org-cycle         )
    (local-set-key [C-f12]         'org-template      )
    (local-set-key [?\C-c C-backspace] 'org-html-delete)
    (local-set-key [?\t]           'org-cycle         ) ; forget outline-mode
    (local-set-key [C-S-tab]       'org-collapse-tree )
    (local-set-key [C-S-iso-lefttab] 'org-collapse-tree)
    (local-set-key [?\C-m]         'org-return        ) ; forget outline-mode
    (local-set-key [M-return]      'org-open-at-point ) ; org-meta-return
    (local-set-key [M-home]        'org-insert-h3-down)
    (local-set-key [M-end]         'org-colon-paragraph)
    (local-set-key [?\C-c C-up]    'org-shiftmetaup   ) ; alternative M-S arrows
    (local-set-key [?\C-c C-down]  'org-shiftmetadown ) ; alternative M-S arrows
    (local-set-key [?\C-c C-left]  'org-shiftmetaleft ) ; alternative M-S arrows
    (local-set-key [?\C-c C-right] 'org-shiftmetaright) ; alternative M-S arrows
    (local-set-key [?\C-c ?\C-0]   'org-insert-title  )
    (local-set-key [?\C-c ?\C-1]   'org-insert-h1     )
    (local-set-key [?\C-c ?\C-2]   'org-insert-h2     )
    (local-set-key [?\C-c ?\C-3]   'org-insert-h3     )
    (local-set-key [?\C-c ?\C-/]   'org-word-help     )
    (local-set-key [?\C-c ?\C-a]   'org-collapse-tree ) ; org-attach
    (local-set-key [?\C-c ?\C-c]   'org-html-write    ) ; org-ctrl-c-ctrl-c
    (local-set-key [?\C-c ?\C-d]   'org-toc-3         ) ; org-deadline
    (local-set-key [?\C-c ?\C-e]   'org-copy-visible  ) ; org-export-dispatch
    (local-set-key [?\C-c ?\C-f]   'org-toc-4) ; org-forward-heading-same-level
    (local-set-key [?\C-c ?\C-l]   'outline-show-all  ) ; org-insert-link
    (local-set-key [?\C-c ?\C-r]   'org-insert-R-block) ; org-reveal
    (local-set-key [?\C-c ?\C-s]   'org-toc-2         ) ; org-schedule
    (local-set-key [?\C-c ?\C-t]   'org-ascii-write   ) ; org-todo
    (local-set-key [?\C-c ?\C-u]   'org-up-element    ) ; outline-up-heading
    (local-set-key [?\C-c ?\C-v]   'org-html-view     ) ; [map]
    (local-set-key [?\C-c ?\C-w]   'org-word-convert  ) ; org-refile
    (local-set-key [?\C-c ?\C-z]   'org-narrow-to-subtree)) ; org-add-note
  (org-arni-keybindings)
  (defun org-ascii-write ()
    "Export buffer to text file."
    (interactive)
    (org-ascii-export-to-ascii)
    (arni-org-hook))
  (defun org-collapse-tree ()
    "Collapse tree and return to beginning of file."
    (interactive)
    (outline-hide-sublevels 1)
    (goto-char (point-min)))
  (defun org-colon-paragraph ()
    "Put colon in front of paragraph and move down."
    (interactive)
    (region-forward-paragraph 1)
    (comment-line-or-region)
    (forward-line))
  (defun org-html-delete ()
    "Delete HTML file corresponding to Org file."
    (interactive)
    (let ((html-file
           (concat (file-name-sans-extension buffer-file-name) ".html")))
      (delete-file html-file)
      (message "Deleted %s" html-file)))
  (defun org-html-view ()
    "View HTML file with same prefix as current Org file."
    (interactive)
    (browse-url (concat (file-name-sans-extension (buffer-file-name)) ".html")))
  (defun org-html-write ()
    "Export buffer to HTML and run personal hook."
    (interactive)
    (save-buffer)
    (org-html-export-to-html)
    (org-arni-keybindings))
  (defun org-insert-h1 ()
    "Insert heading of level 1."
    (interactive "*")
    (beginning-of-line)
    (insert "* "))
  (defun org-insert-h2 ()
    "Insert heading of level 2."
    (interactive "*")
    (beginning-of-line)
    (insert "** "))
  (defun org-insert-h3 ()
    "Insert heading of level 3."
    (interactive "*")
    (beginning-of-line)
    (insert "*** "))
  (defun org-insert-h3-down ()
    "Insert heading of level 3 and move down."
    (interactive "*")
    (org-insert-h3)
    (forward-line 2))
  (defun org-insert-R-block ()
    "Insert R block."
    (interactive "*")
    (insert "#+begin_src R\n\n#+end_src R\n")
    (forward-line -2))
  (defun org-insert-title ()
    "Insert title."
    (interactive "*")
    (insert "#+TITLE: "))
  (defun org-justify-down ()
    "Justify paragraph and move down."
    (interactive "*")
    (fill-paragraph-forward 1)
    (forward-line))
  (defun org-mouse-cycle (event)
    "Position cursor and cycle visibility."
    (interactive "e")
    (mouse-set-point event)
    (org-cycle))
  (defun org-mouse-show (event)
    "Position cursor and show all."
    (interactive "e")
    (mouse-set-point event)
    (outline-show-all))
  (defun org-return-down ()
    "Insert empty line and move down."
    (interactive "*")
    (beginning-of-line)
    (insert "\n")
    (forward-line))
  (defun org-template ()
    "Insert minimal Org template."
    (interactive "*")
    (goto-char (point-min))
    (insert "#+TITLE: \n\n* ")
    (backward-char 4))
  (defun org-toc-2 ()
    "Show outline level 2."
    (interactive)
    (org-global-cycle 2))
  (defun org-toc-3 ()
    "Show outline level 3."
    (interactive)
    (org-global-cycle 3))
  (defun org-toc-4 ()
    "Show outline level 4."
    (interactive)
    (org-global-cycle 4))
  (defun org-word-convert ()
    "Convert Word headings to Org format."
    (interactive "*")
    (set-buffer-file-coding-system 'utf-8-unix t)
    (goto-char (point-min))
    (insert "#+TITLE: ")
    (forward-line)
    (delete-region (point)(line-end-position))
    (delete-region (point)(re-search-forward "^1\t" nil t))
    (insert "1\t") ; delete TOC
    (goto-char (point-min))
    (while (re-search-forward "^[0-9]*\t\\(.*\\)" nil t)
      (replace-match "\n* \\1\n"))
    (goto-char (point-min))
    (while (re-search-forward "^[0-9]*\.[0-9]*\t\\(.*\\)" nil t)
      (replace-match "\n** \\1\n"))
    (goto-char (point-min))
    (while (search-forward "\n\n\n" nil t)
      (replace-match "\n\n"))
    (goto-char (point-max))
    (delete-blank-lines)
    (goto-char (point-min))
    (message "Converted Word headings to Org format.
1. [down] and [M-down].\n2. [M-home], [M-end], [M-up]."))
  (defun org-word-help ()
    "Show keybindings to convert Word document to Org format."
    (interactive)
    (message "1. [down] and [M-down].\n2. [M-home], [M-end], [M-up].")))
(add-hook 'org-mode-hook 'arni-org-hook)
;;--------------
;; 7.27 Outline
;;--------------
(defvar outline-previous-mode '(text-mode)
  "Mode to return to. See `outline-return'.")
(defvar outline-top-level 1
  "Top outline level, to anchor the `outline-hide' cursor
to the shortest `outline-regexp'.")
(defun arni-outline-hook ()
  (set-face-attribute 'font-lock-warning-face
                      nil :foreground (fg 'font-lock-keyword-face) :weight -)
  (set-face-attribute 'outline-1 nil :inherit font-lock-keyword-face)
  (set-face-attribute 'outline-2 nil :inherit font-lock-keyword-face)
  (set-face-attribute 'outline-3 nil :inherit font-lock-keyword-face)
  (set-face-attribute 'outline-4 nil :inherit font-lock-keyword-face)
  (set-face-attribute 'outline-5 nil :inherit font-lock-keyword-face)
  (set-face-attribute 'outline-6 nil :inherit font-lock-keyword-face)
  (set-face-attribute 'outline-7 nil :inherit font-lock-keyword-face)
  (set-face-attribute 'outline-8 nil :inherit font-lock-keyword-face)
  (local-set-key [mouse-1]         'outline-mouse-select       )
  (local-set-key [escape]          'outline-window-or-return   )
  (local-set-key [f11]             'outline-return             )
  (local-set-key [?\t]             'outline-toggle-children    )
  (local-set-key [backtab]         'outline-toggle-children    )
  (local-set-key [C-tab]           'outline-show-all           )
  (local-set-key [C-S-tab]         'outline-hide-sublevels     )
  (local-set-key [C-S-iso-lefttab] 'outline-hide-sublevels     ) ; linux C-S-tab
  (local-set-key [?\C-m]           'outline-return             ) ; return
  (local-set-key [left]            'outline-hide-entry         )
  (local-set-key [right]           'outline-show-entry         )
  (local-set-key [M-left]          'outline-hide               )
  (local-set-key [M-right]         'outline-show               )
  (local-set-key [C-up]            'outline-previous-heading   )
  (local-set-key [C-down]          'outline-next-heading       )
  (local-set-key [M-up]            'outline-backward-same-level)
  (local-set-key [M-down]          'outline-forward-same-level )
  (local-set-key [?,]              'outline-hide-entry         )
  (local-set-key [?.]              'outline-show-entry         )
  (local-set-key [?<]              'outline-hide               )
  (local-set-key [?>]              'outline-show               )
  (local-set-key [?\C-\M-\]]       'outline-move-subtree-up    )
  (local-set-key [?\C-\M-']        'outline-move-subtree-down  )
  (local-set-key [?M]              'jump-middle                )
  (local-set-key [?N]              'outline-forward-same-level )
  (local-set-key [?P]              'outline-backward-same-level)
  (local-set-key [?l]              'recenter-top-bottom        )
  (local-set-key [?m]              'jump-middle                )
  (local-set-key [?n]              'outline-next-heading       )
  (local-set-key [?p]              'outline-previous-heading   )
  (local-set-key [?q]              'outline-return             )
  (defun outline-hide ()
    "Hide subheadings and return to upper heading."
    (interactive)
    (deactivate-mark)
    (outline-back-to-heading)
    (if (> (outline-level) outline-top-level)
        (outline-up-heading 1))
    (outline-hide-subtree))
  (defun outline-mouse-select ()
    "Select position and return to `outline-previous-mode'."
    (interactive)
    (outline-return)
    (beginning-of-line))
  (defun outline-return ()
    "Return to `outline-previous-mode'."
    (interactive)
    (eval outline-previous-mode))
  (defun outline-show ()
    "Show subheadings."
    (interactive)
    (deactivate-mark)
    (outline-show-branches))
  (defun outline-window-or-return ()
    "Delete other windows or return to `outline-previous-mode'."
    (interactive)
    (if (> (length (window-list)) 1)
        (delete-other-windows)
      (outline-return))))
(add-hook 'outline-mode-hook 'arni-outline-hook)
;;---------------
;; 7.28 Packages
;;---------------
;; (require 'package)
;; (setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")))
;; (setq package-archives '(("marmalade" . "http://marmalade-repo.org/"   )))
;; (setq package-archives '(("melpa"     . "https://melpa.org/packages/"  )))
;; (setq package-archives '(("tromey"    . "http://tromey.com/elpa/"      )))
;; (setq package-enable-at-startup nil)
;; (package-initialize)
(defun arni-package-menu-hook ()
  (message nil)
  (local-set-key [left]   'package-menu-describe-package)
  (local-set-key [right]  'package-menu-describe-package)
  (local-set-key [up]     'backward-button              )
  (local-set-key [down]   'forward-button               )
  (local-set-key [M-up]   'package-menu-up-view         )
  (local-set-key [M-down] 'package-menu-down-view       )
  (local-set-key [?N]     'package-menu-down-view       )
  (local-set-key [?P]     'package-menu-up-view         )
  (local-set-key [?n]     'forward-button               )
  (local-set-key [?p]     'backward-button              )
  (local-set-key [?v]     'package-menu-describe-package)
  (defun package-menu-down-view ()
    "Move down one package and view description in other window."
    (interactive)
    (forward-button 1)
    (package-menu-describe-package))
  (defun package-menu-up-view ()
    "Move up one package and view description in other window."
    (interactive)
    (backward-button 1)
    (package-menu-describe-package)))
(add-hook 'package-menu-mode-hook 'arni-package-menu-hook)
;;-------------
;; 7.29 Proced
;;-------------
(defun arni-proced-hook ()
  (local-set-key [?h] 'proced-omit-processes)
  (local-set-key [?r] 'revert-buffer       ))
(add-hook 'proced-mode-hook 'arni-proced-hook)
;;--------------
;; 7.30 Recentf
;;--------------
(defun arni-recentf-dialog-hook ()
  (local-unset-key [escape])
  (local-set-key [f5]        'recentf-cleanup        )
  (local-set-key [C-home]    'recentf-top            )
  (local-set-key [C-end]     'recentf-bottom         )
  (local-set-key [prior]     'recentf-up-page        )
  (local-set-key [next]      'recentf-down-page      )
  (local-set-key [backspace] 'recentf-cleanup        )
  (local-set-key [?\t]       'recentf-down           )
  (local-set-key [backtab]   'recentf-up             )
  (local-set-key [?\C-m]     'widget-button-press-eol) ; return
  (local-set-key [up]        'recentf-up             )
  (local-set-key [down]      'recentf-down           )
  (local-set-key [S-up]      'region-bol-up          )
  (local-set-key [S-down]    'region-bol-down        )
  (local-set-key [C-up]      'recentf-up-3           )
  (local-set-key [C-down]    'recentf-down-3         )
  (local-set-key [M-up]      'recentf-up-3           )
  (local-set-key [M-down]    'recentf-down-3         )
  (local-set-key [?\C-n]     'recentf-down           )
  (local-set-key [?\C-p]     'recentf-up             )
  (local-set-key [?\C-v]     'recentf-down-page      )
  (local-set-key [?\C-y]     'recentf-up-page        )
  (local-set-key [?/]        'isearch-forward        )
  (local-set-key [? ]        'widget-button-press-eol)
  (local-set-key [?M]        'recentf-middle         )
  (local-set-key [?N]        'recentf-down-3         )
  (local-set-key [?P]        'recentf-up-3           )
  (local-set-key [?d]        'widget-button-press    )
  (local-set-key [?e]        'recentf-edit-list      )
  (local-set-key [?r]        'recentf-cleanup        )
  (local-set-key [?x]        'recentf-edit-expunge   )
  (local-set-key [?m]        'recentf-middle         )
  (local-set-key [?n]        'recentf-down           )
  (local-set-key [?p]        'recentf-up             )
  (defun recentf-bottom ()
    "Move to bottom."
    (interactive)
    (deactivate-mark)
    (goto-char (point-max))
    (recentf-up 1))
  (defun recentf-down (&optional n)
    "Move down N lines."
    (interactive "p")
    (deactivate-mark)
    (beginning-of-line (+ n 1))
    (let ((last (- (count-lines (point-min)(point-max)) 2)))
      (if (> (line-number-at-pos) last)
          (goto-line-lisp last)))
    (widget-forward 1))
  (defun recentf-down-3 ()
    "Move down 3 lines."
    (interactive)
    (recentf-down 3))
  (defun recentf-down-page (&optional n)
    "Move down N pages."
    (interactive "p")
    (recentf-down (pages n)))
  (defun recentf-edit-expunge ()
    "Expunge marked files from history."
    (interactive)
    (recentf-edit-list-validate))
  (defun recentf-middle ()
    "Move to middle."
    (interactive)
    (jump-middle)
    (widget-forward 1))
  (defun recentf-top ()
    "Move to bottom."
    (interactive)
    (deactivate-mark)
    (goto-line-lisp 3)
    (recentf-down 1))
  (defun recentf-up (&optional n)
    "Move up N lines."
    (interactive "p")
    (deactivate-mark)
    (end-of-line (- 2 n))
    (if (< (line-number-at-pos) 5)
        (progn (goto-line-lisp 5)
               (end-of-line)))
    (widget-backward 1))
  (defun recentf-up-3 ()
    "Move up 3 lines."
    (interactive)
    (recentf-up 3))
  (defun recentf-up-page (&optional n)
    "Move up N pages."
    (interactive "p")
    (recentf-up (pages n)))
  (defun widget-button-press-eol ()
    "Open file in current line."
    (interactive)
    (widget-button-press (line-end-position))))
(add-hook 'recentf-dialog-mode-hook 'arni-recentf-dialog-hook)
;;-------------
;; 7.31 Regexp
;;-------------
(defun arni-reb-hook ()
  (reb-toggle-case)
  (local-set-key [?\C-c ?\C-q] 'reb-quit-gracefully)
  (local-set-key [?\C-n]       'reb-next-match     )
  (local-set-key [?\C-p]       'reb-prev-match     )
  (local-set-key [?\C-r]       'reb-prev-match     )
  (local-set-key [?\C-s]       'reb-next-match     )
  (set-face-attribute 'reb-match-0 nil :background "gold")
  (defun reb-quit-gracefully ()
    "Quit re-builder and restore case-fold-search."
    (interactive)
    (reb-quit)
    (reb-toggle-case)
    (kill-buffer "*RE-Builder*")))
(add-hook 'reb-mode-hook 'arni-reb-hook)
;;-----------
;; 7.32 reST
;;-----------
(defun arni-rst-hook ()
  (font-lock-mode 1))
(add-hook 'rst-mode-hook 'arni-rst-hook)
;;-------------
;; 7.33 Search
;;-------------
(setq isearch-lax-whitespace nil)
(defun arni-isearch-hook ()
  (define-key isearch-mode-map [127]
              'isearch-del-char) ; backspace in GUI and terminal
  (define-key isearch-mode-map [?\t]   'isearch-complete     )
  (define-key isearch-mode-map [?\C-y] 'isearch-page-up      ) ; reactivate pgup
  (define-key isearch-mode-map [?\M- ] 'isearch-toggle-lax-whitespace)
  (define-key isearch-mode-map [?\M-c] 'isearch-copy-line    ) ; reactivate copy
  (define-key isearch-mode-map [?\M-e] 'isearch-query-replace)
  (define-key isearch-mode-map [?\M-f] 'isearch-occur        )
  (define-key isearch-mode-map [?\M-r] 'isearch-toggle-regexp)
  (define-key isearch-mode-map [?\M-s] 'isearch-edit-string  )
  (define-key isearch-mode-map [?\M-t] 'isearch-toggle-case-fold)
  (define-key isearch-mode-map [?\M-w] 'isearch-toggle-word  )
  (defun isearch-copy-line ()
    "Copy line and exit isearch."
    (interactive)
    (isearch-exit)
    (copy-line-or-region))
  (defun isearch-page-up ()
    "Page up from isearch."
    (interactive)
    (scroll-down)
    (isearch-dehighlight)
    (lazy-highlight-cleanup)))
(add-hook 'isearch-mode-hook 'arni-isearch-hook)
;;---------------
;; 7.34 Speedbar
;;---------------
(defun arni-speedbar-hook ()
  (setq speedbar-frame-parameters
        (append (list (car speedbar-frame-parameters))
                (list '(width . 24))
                (nthcdr 2 speedbar-frame-parameters))) ; width
  (setq speedbar-show-unknown-files t)
  (set-face-attribute 'speedbar-file-face nil :foreground -)
  (set-face-attribute 'speedbar-directory-face
                      nil :foreground - :inherit font-lock-keyword-face)
  (define-key speedbar-file-key-map [backspace] 'speedbar-up-directory         )
  (define-key speedbar-file-key-map [left]      'speedbar-up-directory         )
  (define-key speedbar-file-key-map [right]     'speedbar-edit-line            )
  (define-key speedbar-file-key-map [up]        'speedbar-restricted-prev      )
  (define-key speedbar-file-key-map [down]      'speedbar-restricted-next      )
  (define-key speedbar-file-key-map [?.]        'speedbar-toggle-show-all-files)
  (define-key speedbar-file-key-map [?N]        'speedbar-scroll-up            )
  (define-key speedbar-file-key-map [?P]        'speedbar-scroll-down          )
  (define-key speedbar-file-key-map [?c]        'speedbar-item-copy            )
  (define-key speedbar-file-key-map [?d]        'speedbar-item-delete          )
  (define-key speedbar-file-key-map [?h]        'speedbar-toggle-show-all-files)
  (define-key speedbar-file-key-map [?i]        'speedbar-item-info            )
  (define-key speedbar-file-key-map [?r]        'speedbar-item-rename          )
  (define-key speedbar-file-key-map [?s]        'speedbar-toggle-sorting       )
  (define-key speedbar-file-key-map [?t]        'speedbar-toggle-show-all-files)
  (define-key speedbar-file-key-map [?u]        'speedbar-up-directory        ))
(add-hook 'speedbar-mode-hook 'arni-speedbar-hook)
;;-------------
;; 7.35 Tabbar
;;-------------
(defun arni-tabbar-hook ()
  (defun tabbar-common ()
    '("Common"))
  (setq tabbar-buffer-groups-function 'tabbar-common) ; all tabs in one group
  (setq tabbar-separator '(1.4))
  (set-face-attribute 'tabbar-default    nil :background "gray65" :foreground -)
  (set-face-attribute 'tabbar-button     nil :box -                            )
  (set-face-attribute 'tabbar-selected
                      nil :background (bg 'default) :foreground - :box -)
  (set-face-attribute 'tabbar-unselected nil :box -                            )
  (global-set-key [M-home] 'tabbar-mode       )
  (global-set-key [M-end]  'tabbar-local-mode)) ; end-of-buffer
(add-hook 'tabbar-init-hook 'arni-tabbar-hook)
(defun arni-tabbar-quit-hook ()
  (global-set-key [M-home] 'beginning-of-buffer-other-window) ; reactivate
  (global-set-key [M-end]  'end-of-buffer-other-window     ))
(add-hook 'tabbar-quit-hook 'arni-tabbar-quit-hook)
;;-----------
;; 7.36 Text
;;-----------
(defun arni-text-hook ()
  (setq indent-line-function 'indent-relative)
  (auto-fill-mode t)
  (setq fill-column 80)
  (font-lock-mode 0)
  (arni-colors)
  (local-unset-key [?\M-s]) ; reactivate highlight-and-count-regexp
  (local-set-key [?\t]      'indent-relative        )
  (local-set-key [f11]      'text-outline           )
  (local-set-key [?\C-x ?m] 'simplify-vertical-space)
  (defun text-outline ()
    "Navigate within NEWS file using `outline-mode'."
    (interactive)
    (outline-mode)
    (setq outline-regexp " *[-*o]+ ")
    (outline-mode)
    (setq outline-previous-mode '(text-mode))))
(add-hook 'text-mode-hook 'arni-text-hook)
(defun arni-mail-hook ()
  (setq fill-column 74))
(add-hook 'mail-mode-hook 'arni-mail-hook)
(defun arni-message-hook ()
  (set-face-attribute 'message-cited-text
                      nil :foreground - :inherit font-lock-comment-face))
(add-hook 'message-mode-hook 'arni-message-hook)
(defalias 'longlines-mode 'visual-line-mode)
;; (setq visual-line-fringe-indicators '(nil right-curly-arrow))
;;---------
;; 7.37 VC
;;---------
(defun vc-diff-select ()
  "Diff two selected file versions."
  (interactive)
  (vc-diff t))
(defun arni-log-edit-hook ()
  (setq require-final-newline nil)
  (setq fill-column 72)
  (mark-buffer))
(add-hook 'log-edit-mode-hook 'arni-log-edit-hook)
(defun arni-log-view-hook ()
  (set-face-attribute 'log-view-message
                      nil :foreground (fg 'font-lock-comment-face))
  (local-unset-key [?\M-n]) ; reactivate bs-cycle-next
  (local-unset-key [?\M-p]) ; reactivate bs-cycle-previous
  (local-set-key [?q] 'kill-buffer-maybe-window))
(add-hook 'log-view-mode-hook 'arni-log-view-hook)
(defun arni-vc-dir-hook ()
  (local-set-key [f5]    'vc-dir-refresh      )
  (local-set-key [left]  'vc-diff-stay        )
  (local-set-key [right] 'vc-dir-display-file )
  (local-set-key [up]    'vc-dir-previous-line)
  (local-set-key [down]  'vc-dir-next-line    )
  (local-set-key [?d]    'vc-diff-stay        ) ; vc-dir-clean-files
  (local-set-key [?r]    'vc-dir-refresh      )
  (local-set-key [?v]    'vc-dir-display-file ) ; vc-next-action
  (defun vc-diff-stay ()
    "View diff in source file and stay."
    (interactive)
    (let ((vc-window (selected-window)))
      (vc-diff)
      (select-window vc-window))))
(add-hook 'vc-dir-mode-hook 'arni-vc-dir-hook)
;;==============================================================================
;; 8  ENABLE
;;==============================================================================
;;---------------
;; 8.1  Commands
;;---------------
(put 'downcase-region  'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left      'disabled nil)
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)
