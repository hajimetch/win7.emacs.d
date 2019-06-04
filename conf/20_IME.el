(use-package skk
  :bind
  (("C-x C-j"       . skk-mode)
   ("C-x j"         . skk-auto-fill-mode)
   :map skk-j-mode-map
   ("S-SPC"         . skk-previous-candidate)
   ("C-n"           . my/skk-next-candidate)
   ("C-p"           . my/skk-previous-candidate)
   ("<down>"        . my/skk-next-candidate)
   ("<up>"          . my/skk-previous-candidate)
   ("C-M-,"         . skk-toggle-kutouten)
   ("<S-return>"    . skk-undo-kakutei)
   :map isearch-mode-map
   ("C-d"           . isearch-delete-char)
   ("C-e"           . isearch-edit-string)
   ("C-g"           . (lambda() (interactive) (isearch-done)))
   ("C-y"           . isearch-yank-kill)
   ("TAB"           . isearch-yank-word)
   ("M-s"           . helm-swoop-from-isearch))

  :hook
  ((isearch-mode . skk-isearch-mode-setup)
   (isearch-mode-end . skk-isearch-mode-cleanup))

  :commands skk-wrap-newline-command

  :init (setq skk-user-directory "C:/Users/hajimetch/Dropbox/Emacs/ddskk/")
                                        ; 設定ファイルパス

  :custom
  ;; 全般
  (default-input-method "japanese-skk") ; 日本語入力にskkを使用
  (skk-server-host "localhost")           ; サーバー機能を利用
  (skk-server-portnum 55100)              ; ポートはgoogle-ime-skk
  (skk-share-private-jisyo t)             ; 複数 skk 辞書を共有
  ;; 候補表示
  (skk-show-inline t)                   ; インライン表示
  (skk-show-annotation nil)             ; 注釈は非表示
  (skk-inline-show-face                 ; フェイス
   'skk-henkan-face-default)
  ;; 動的候補表示
  (skk-dcomp-activate t)                ; 動的補完を行う
  (skk-dcomp-multiple-activate t)       ; 動的補完の複数候補表示
  (skk-dcomp-multiple-rows 10)          ; 動的補完の候補表示件数
  ;; 動作
  (skk-verbose t)                   ; 詳細なメッセージを表示
  (skk-use-kakasi t)                ; 漢字→かな変換を使う
  (skk-sticky-key ";")              ; ";"をsticky shift keyに
  (skk-comp-circulate t)            ; 見出し語の補完時の候補の表示順
  (skk-hint-start-char ?:)          ; ヒントを表示するキー
  (skk-egg-like-newline t)          ; Enterで改行しない
  (skk-auto-insert-paren t)         ; 閉じカッコを自動的に
  (skk-auto-start-henkan t)         ; 区切り文字で自動変換
  (skk-share-private-jisyo t)       ; 個人辞書を複数Emacsで共有
  (skk-delete-implies-kakutei nil)  ; ▼モードで一つ前の候補を表示する
  (skk-previous-candidate-keys '("x"))  ; 前候補表示キーからC-pを除外
  (skk-search-katakana 'jisx0201-kana)  ; カタカナを変換候補に入れる
  (skk-henkan-strict-okuri-precedence t) ; 送り仮名が厳密に正しい候補を優先して表示
  (skk-use-auto-enclose-pair-of-region t) ; リージョンを括弧で囲む
  (skk-compare-jisyo-size-when-saving nil) ; 辞書サイズをチェックkしない
  (skk-j-mode-function-key-usage 'conversion) ; fnキーを使って変換
  ;; skk-isearch
  (skk-isearch-start-mode 'latin)       ; isearch での skk の初期状態
  (search-nonincremental-instead nil)   ; Enter で終了
  ;; 言語
  (skk-japanese-message-and-error t)    ; エラーを日本語に
  (skk-show-japanese-menu t)            ; メニューを日本語に
  ;; 基本辞書
  (skk-large-jisyo "C:/Users/hajimetch/Dropbox/Emacs/ddskk/SKK-JISYO.L")
  ;; チュートリアルのパス
  (skk-tut-file "C:/Users/hajimetch/Dropbox/Emacs/ddskk/SKK.tut")

  :custom-face
  (skk-dcomp-multiple-face              ; 動的補完の複数表示郡
   ((t (:foreground "Black" :background "LightGoldenrodYellow" :bold nil))))
  (skk-dcomp-multiple-trailing-face   ; 動的補完の複数表示郡の補完部分
   ((t (:foreground "dim gray" :background "LightGoldenrodYellow" :bold nil))))
  (skk-dcomp-multiple-selected-face   ; 動的補完の複数表示郡の選択対象
   ((t (:foreground "White" :background "LightGoldenrod4" :bold nil))))

  :config
  ;; 従属パッケージ
  (use-package skk-study)               ; 変換学習機能
  (use-package skk-hint)                ; ヒント
  (use-package context-skk)             ; 自動的にモード切り替え
  ;; 個人辞書の自動保存
  (defvar my/skk-auto-save-jisyo-interval 600
    "Interval of saving jisyo.")
  (run-with-idle-timer my/skk-auto-save-jisyo-interval
                       my/skk-auto-save-jisyo-interval
                       'skk-save-jisyo)
  ;; 次候補表示
  (defun my/skk-next-candidate ()
    "Show next candidate of skk."
    (interactive)
    (cond ((eq skk-henkan-mode 'on)
           (skk-comp-wrapper t))
          ((eq skk-henkan-mode 'active)
           (skk-start-henkan t))
          (t (next-line))))
  ;; 前候補表示
  (defun my/skk-previous-candidate ()
    "Show previous candidate of skk."
    (interactive)
    (cond ((eq skk-henkan-mode 'on)
           (skk-comp-previous t))
          ((eq skk-henkan-mode 'active)
           (skk-previous-candidate t))
          (t (previous-line)))))
