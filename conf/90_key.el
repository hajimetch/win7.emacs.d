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
           ("C-S-j" .      skk-undo-kakutei))


;;; Helm
(bind-key "M-x"            'helm-M-x)
(bind-key "C-x C-f"        'helm-find-files)
(bind-key "C-x C-l"        'helm-elscreen)
(bind-key "C-x C-x"        'helm-mini)
(bind-key "C-x C-z"        'helm-resume)
(bind-key "C-c g"          'helm-do-ag)
(bind-key "C-c i"          'helm-semantic-or-imenu)
(bind-key "C-c k"          'helm-descbinds)
(bind-key "C-c y"          'helm-yas-complete)
(bind-key "C-c C-SPC"      'helm-all-mark-rings)
(bind-key "C-c <f1>"       'helm-info)
(bind-key "C-M-y"          'helm-show-kill-ring)
(bind-key "<M-f1>"         'my/helm-for-document)
(bind-key* "M-m"           'helm-migemo-mode helm-map)
(bind-keys :map helm-map
           ("TAB" .        helm-execute-persistent-action)
           ("C-z" .        helm-select-action)
           ("M-b" .        my/helm-ff-run-browse-project)
           ("<f1>" .       helm-help))


;;; projectile
(bind-key "C-x C-p"        'helm-projectile)
(bind-key "C-c C-p"        'projectile-command-map)


;;; org-mode
(bind-key "C-c a"          'org-agenda)
(bind-key "C-c c"          'org-capture)
(bind-key "C-c o"          'org-switchb)
(bind-key "C-c l"          'org-store-link)
(bind-key "C-M-="          'my/org-capture-task)
(bind-key "C-M--"          'my/org-capture-memo)
(bind-keys :map org-mode-map
           ("C-c (" .      org-clock-in)
           ("C-c )" .      org-clock-out))


;;; howm
(bind-keys :map howm-mode-map
           ("C-c C-c" .    my/howm-save-buffer-and-kill)
           ("C-c C-k" .    my/howm-kill-buffer))


;;; PL
(bind-key "C-c q"          'quickrun)
(bind-key "C-c p"          'previous-error)
(bind-key "C-c n"          'next-error)


;;; helm-gtags-mode
(setq helm-gtags-mode-hook
      '(lambda ()
         (bind-keys :map helm-gtags-mode-map
                    ("C-c . ." . helm-gtags-find-tag-from-here)
                    ("C-c . ," . helm-gtags-pop-stack)
                    ("C-c . t" . helm-gtags-find-tag)
                    ("C-c . r" . helm-gtags-find-rtag)
                    ("C-c . s" . helm-gtags-find-symbol)
                    ("C-c . f" . helm-gtags-find-files))))


;;; flycheck-mode
(bind-key "C-c C-d"        'flycheck-list-errors flycheck-mode-map)


;;; python-mode
(bind-key "C-c C-f"        'py-yapf-buffer python-mode-map)


;;; git-gutter
(bind-key "C-x p"          'git-gutter:previous-hunk)
(bind-key "C-x n"          'git-gutter:next-hunk)


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
(bind-key "<f12>"          'elscreen-next)
(bind-key "<f11>"          'elscreen-previous)
(bind-key "C-z"            'iconify-or-deiconify-frame elscreen-map)


;;; Move Windows
(bind-key "C-t"            'other-window)
(bind-key "M-t"            'rotate-window)


;;; Other key bindings
(bind-key "C-h" (kbd "DEL") key-translation-map) ; C-hでバックスペース
(bind-key "C-m"            'newline-and-indent) ; 改行時自動インデント
(bind-key "C-x g"          'magit-status)       ; magitステータス
(bind-key "C-x k"          'kill-this-buffer)   ; バッファを閉じる
(bind-key "M-k"            'kill-this-buffer)   ; バッファを閉じる
(bind-key "C-c j"          'open-junk-file)     ; junk-file作成
(bind-key "C-c r"          'my/revert-buffer)   ; バッファ更新
(bind-key "C-c s"          'whitespace-cleanup) ; 不要な空白を削除
(bind-key "C-c t"          'my/eshell-pop)      ; eshellを開く
(bind-key "C-c <C-return>" 'toggle-truncate-lines) ; 右端で折り返す
(bind-key "C-c TAB"        'indent-region)         ; 範囲インデント
(bind-key "C-t"            'other-window dired-mode-map) ; 重複を回避
(bind-key "M-p"            'ps-print-region)             ; PDF作成
(bind-key "<backtab>"      '(lambda() (interactive) (insert "	")))
                                        ; インデント
(bind-key "<f1>"           'help-for-help) ; ヘルプ参照
