;;; PostScript Option
(eval-when-compile (require 'ps-mule nil t))
(setq ps-print-color-p     t)
(setq ps-paper-type        'a4)         ; paper size
(setq ps-lpr-command       "pdfpreview.bat")
(setq ps-printer-name      nil)
(setq ps-printer-name-option nil)
(setq ps-multibyte-buffer  'non-latin-printer) ; for printing Japanese
(setq ps-n-up-printing     1)           ; print n-page per 1 paper

;; Margin
(setq ps-left-margin       40)
(setq ps-right-margin      40)
(setq ps-top-margin        40)
(setq ps-bottom-margin     40)
(setq ps-n-up-margin       20)

;; Header/Footer setup
(setq ps-print-header      t)         ; buffer name, page number, etc.
(setq ps-print-footer      nil)       ; page number

;; font
(setq ps-font-size         '(10 . 12))
(setq ps-header-font-size  '(10 . 12))
(setq ps-header-title-font-size '(12 . 14))
(setq ps-header-font-family 'Helvetica)    ; default
(setq ps-line-number-font  "Times-Italic") ; default
(setq ps-line-number-font-size 8)

;; line-number
(setq ps-line-number       t)           ; t:print line number
(setq ps-line-number-start 1)
(setq ps-line-number-step  1)
