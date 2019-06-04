;;; PostScript Option
(use-package ps-mule
  :bind ("M-p"      . ps-print-region)
  :custom
  (ps-print-color-p     t)
  (ps-paper-type        'a4)            ; paper size
  (ps-lpr-command       "pdfpreview.bat")
  (ps-printer-name      nil)
  (ps-printer-name-option nil)
  (ps-multibyte-buffer  'non-latin-printer) ; for printing Japanese
  (ps-n-up-printing     1)                  ; print n-page per 1 paper
  ;; Margin
  (ps-left-margin       40)
  (ps-right-margin      40)
  (ps-top-margin        40)
  (ps-bottom-margin     40)
  (ps-n-up-margin       20)
  ;; Header/Footer setup
  (ps-print-header      t)            ; buffer name, page number, etc.
  (ps-print-footer      nil)          ; page number
  ;; font
  (ps-font-size         '(10 . 12))
  (ps-header-font-size  '(10 . 12))
  (ps-header-title-font-size '(12 . 14))
  (ps-header-font-family 'Helvetica)    ; default
  (ps-line-number-font  "Times-Italic") ; default
  (ps-line-number-font-size 8)
  ;; line-number
  (ps-line-number       t)              ; t:print line number
  (ps-line-number-start 1)
  (ps-line-number-step  1))
