;;; Alt to Meta
(setq w32-alt-is-meta t)


;;; ddskk
(bind-key "C-x C-j"        'skk-mode)
(bind-key "C-x j"          'skk-auto-fill-mode)
(bind-keys :map skk-j-mode-map
           ("S-SPC" .      skk-previous-candidate)
           ("C-n" .        my/skk-next-candidate)
           ("C-p" .        my/skk-previous-candidate)
           ("<down>" .     my/skk-next-candidate)
           ("<up>" .       my/skk-previous-candidate)
           ("C-M-," .      skk-toggle-kutouten)
           ("<S-return>" . skk-undo-kakutei))
(bind-key "C-j" 'skk-kakutei helm-map)
(bind-key "C-j" 'skk-kakutei minibuffer-local-map)


;;; Helm
(bind-key "M-x"            'helm-M-x)
(bind-key "C-x C-x"        'helm-mini)
(bind-key "C-x C-f"        'helm-find-files)
(bind-key "C-x C-z"        'helm-resume)
(bind-key "C-c g"          'helm-do-ag)
(bind-key "C-c i"          'helm-semantic-or-imenu)
(bind-key "C-c k"          'helm-descbinds)
(bind-key "C-c y"          'helm-yas-complete)
(bind-key "C-c C-SPC"      'helm-all-mark-rings)
(bind-key "C-c <f1>"       'helm-info)
(bind-key "C-M-y"          'helm-show-kill-ring)
(bind-key "<M-f1>"         'helm-apropos)
(bind-key* "M-m"           'helm-migemo-mode helm-map)
(bind-keys :map helm-map
           ("TAB" .        helm-execute-persistent-action)
           ("C-z" .        helm-select-action)
           ("M-b" .        my/helm-ff-run-browse-project)
           ("<f1>" .       helm-help))


;;; helm-gtags
(bind-key "C-. C-."        'helm-gtags-find-tag-from-here)
(bind-key "C-. C-,"        'helm-gtags-pop-stack)
(bind-key "C-. t"          'helm-gtags-find-tag)
(bind-key "C-. r"          'helm-gtags-find-rtag)
(bind-key "C-. s"          'helm-gtags-find-symbol)
(bind-key "C-. f"          'helm-gtags-find-files)


;;; Projectile
(bind-keys :map projectile-mode-map
           ("C-q" .        projectile-command-map))


;;; org-mode
(bind-key "C-c a"          'org-agenda)
(bind-key "C-c c"          'org-capture)
(bind-key "C-c o"          'org-switchb)
(bind-key "C-c l"          'org-store-link)
(bind-key "C-c ("          'org-clock-in)
(bind-key "C-c )"          'org-clock-out)
(bind-key "C-M-="          'my/org-capture-task)
(bind-key "C-M--"          'my/org-capture-memo)


;;; python-mode
(bind-key "C-c q"          'quickrun)
(bind-keys :map python-mode-map
           ("TAB" .        company-complete)
           ("C-c C-d" .    flycheck-list-errors)
           ("C-c C-f" .    py-yapf-buffer)
           ("C-c C-n" .    flycheck-next-error)
           ("C-c C-p" .    flycheck-previous-error)
           ("C-c C-r" .    helm-jedi-related-names))


;;; multiple-cursor
(bind-key "C->"            'mc/mark-next-like-this)
(bind-key "C-<"            'mc/mark-previous-like-this)
(bind-key "C-c e"          'mc/edit-lines)
(bind-key "C-c h"          'mc/mark-all-like-this)


;;; expand-region
(bind-key "C-="            'er/expand-region)
(bind-key "C--"            'er/contract-region)


;;; point-undo
(bind-key "M-["            'point-undo)
(bind-key "M-]"            'point-redo)


;;; isearch
(bind-keys :map isearch-mode-map
           ("C-d" .        isearch-delete-char)
           ("C-e" .        isearch-edit-string)
           ("C-g" .        (lambda() (interactive) (isearch-done)))
           ("C-y" .        isearch-yank-kill)
           ("TAB" .        isearch-yank-word)
           ("M-s" .        helm-swoop-from-isearch))


;;; helm-swoop
(bind-key "M-s"            'helm-swoop)
(bind-keys :map helm-swoop-map
           ("C-s" .        helm-next-line)
           ("C-r" .        helm-previous-line))


;;; Company
(bind-key "TAB"            'company-complete)
(bind-key "M-/"            'company-dabbrev)
(bind-keys :map company-active-map
           ("C-d" .        company-filter-candidates)
           ("C-n" .        company-select-next)
           ("C-p" .        company-select-previous))
(bind-keys :map company-search-map
           ("C-n" .        company-select-next)
           ("C-p" .        company-select-previous))


;;; Web browse
(bind-key "C-c w"          'helm-google-suggest)
(bind-key "w"              'eww-copy-page-url eww-mode-map)


;;; Font Size
;; Adjust
(bind-key "M-<wheel-up>"   '(lambda() (interactive) (text-scale-increase 1)))
(bind-key "M-="            '(lambda() (interactive) (text-scale-increase 1)))
(bind-key "M-<wheel-down>" '(lambda() (interactive) (text-scale-decrease 1)))
(bind-key "M--"            '(lambda() (interactive) (text-scale-decrease 1)))

;; Reset
(bind-key "M-0"            '(lambda() (interactive) (text-scale-set 0)))


;;; ElScreen
(bind-key "C-}"            'elscreen-next)
(bind-key "C-{"            'elscreen-previous)
(bind-key "C-z h"          'helm-elscreen)
(bind-key "C-z"            'iconify-or-deiconify-frame elscreen-map)


;;; Move Windows
(bind-key "C-t"            'other-window)
(bind-key "M-t"            'rotate-window)


;;; Other key bindings
(define-key key-translation-map (kbd "C-h") (kbd "DEL")) ; C-hでバックスペース
(bind-key "C-m"            'newline-and-indent) ; 改行時自動インデント
(bind-key "C-x g"          'magit-status)       ; magitステータス
(bind-key "C-x k"          'kill-this-buffer)   ; バッファを閉じる
(bind-key "M-k"            'kill-this-buffer)   ; バッファを閉じる
(bind-key "C-c j"          'open-junk-file)     ; junk-file作成
(bind-key "C-c p"          'ps-print-region)    ; PDF作成
(bind-key "C-c r"          'my/revert-buffer)   ; バッファ更新
(bind-key "C-c s"          'whitespace-cleanup) ; 不要な空白を削除
(bind-key "C-c t"          'my/eshell-pop)      ; eshellを開く
(bind-key "C-c C-c"        'my/howm-save-buffer-and-kill howm-mode-map) ; メモを自動保存
(bind-key "C-c <C-return>" 'toggle-truncate-lines) ; 右端で折り返す
(bind-key "C-c TAB"        'indent-region)         ; 範囲インデント
(bind-key "C-c ,,"         'howm-menu semantic-mode-map) ; 重複を回避
(bind-key "C-t"            'other-window dired-mode-map) ; 重複を回避
(bind-key "<backtab>"      '(lambda() (interactive) (insert "	")))
                                        ; インデント
(bind-key "<f1>"           'help-for-help) ; ヘルプ参照
