;;; eww-mode
(require 'eww)


;;; eww 検索エンジンを google.co.jp に
(setq eww-search-prefix "http://www.google.co.jp/search?q=")


;;; eww 背景色の設定
(defvar my/eww-disable-colorize t
  "Variable for disabling eww colorize.")
(defun my/shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  "Function used to disable colorizing region on eww."
  (unless my/eww-disable-colorize
    (funcall orig start end fg)))
(advice-add 'shr-colorize-region :around 'my/shr-colorize-region--disable)
(advice-add 'eww-colorize-region :around 'my/shr-colorize-region--disable)
(defun my/eww-disable-color ()
  "Disable colorize on eww."
  (interactive)
  (setq-local my/eww-disable-colorize t)
  (eww-reload))
(defun my/eww-enable-color ()
  "Enable colorize on eww."
  (interactive)
  (setq-local my/eww-disable-colorize nil)
  (eww-reload))


;;; 現在の url を eww で開く
(defun my/browse-url-with-eww ()
  "Browse current url with eww."
  (interactive)
  (let ((url-region (bounds-of-thing-at-point 'url)))
    ;; url
    (if url-region
        (eww-browse-url (buffer-substring-no-properties (car url-region)
                                                        (cdr url-region))))
    ;; org-link
    (setq browse-url-browser-function 'eww-browse-url)
    (org-open-at-point-global)))


;;; 画像表示の設定
(defun my/eww-disable-images ()
  "Disable showing images on eww."
  (interactive)
  (setq-local shr-put-image-function 'my/shr-put-image-alt)
  (eww-reload))
(defun my/eww-enable-images ()
  "Enable showing images on eww."
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image)
  (eww-reload))
(defun my/shr-put-image-alt (spec alt &optional flags)
  "Put image alt text."
  (insert alt))

;; 始めから非表示
(defun my/eww-mode-hook--disable-image ()
  "Disable showing images with eww-mode-hook."
  (setq-local shr-put-image-function 'my/shr-put-image-alt))
(add-hook 'eww-mode-hook 'my/eww-mode-hook--disable-image)
