;;; PostScript Option
(with-eval-after-load 'ps-mule
  (set-variable 'ps-print-color-p t)
  (set-variable 'ps-paper-type 'a4)     ; paper size
  (set-variable 'ps-lpr-command "pdfpreview.bat")
  (set-variable 'ps-printer-name nil)
  (set-variable 'ps-printer-name-option nil)
  (set-variable 'ps-multibyte-buffer 'non-latin-printer) ; for printing Japanese
  (set-variable 'ps-n-up-printing 1)    ; print n-page per 1 paper
  ;; Margin
  (set-variable 'ps-left-margin 40)
  (set-variable 'ps-right-margin 40)
  (set-variable 'ps-top-margin 40)
  (set-variable 'ps-bottom-margin 40)
  (set-variable 'ps-n-up-margin 20)
  ;; Header/Footer setup
  (set-variable 'ps-print-header t)   ; buffer name, page number, etc.
  (set-variable 'ps-print-footer nil) ; page number
  ;; font
  (set-variable 'ps-font-size '(10 . 12))
  (set-variable 'ps-header-font-size '(10 . 12))
  (set-variable 'ps-header-title-font-size '(12 . 14))
  (set-variable 'ps-header-font-family 'Helvetica)   ; default
  (set-variable 'ps-line-number-font "Times-Italic") ; default
  (set-variable 'ps-line-number-font-size 8)
  ;; line-number
  (set-variable 'ps-line-number t)      ; t:print line number
  (set-variable 'ps-line-number-start 1)
  (set-variable 'ps-line-number-step 1))

(bind-key "M-p" 'ps-print-region)
