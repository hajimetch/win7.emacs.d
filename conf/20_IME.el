;;; SKK 全般
(setq skk-user-directory "c:/Users/hajimetch/Dropbox/Emacs/ddskk/") ; 設定ファイルパス
(require 'skk)
(when (require 'skk nil t)
  (setq default-input-method "japanese-skk") ; emacs上での日本語入力にskkを使う
  (require 'skk-study))                      ; 変換学習機能の追加

(setq skk-server-host "localhost")           ; サーバー機能を利用
(setq skk-server-portnum 55100)              ; ポートはgoogle-ime-skk
(setq skk-share-private-jisyo t)             ; 複数 skk 辞書を共有


;;; 候補表示
(setq skk-show-candidates-always-pop-to-buffer t) ; 候補をバッファに表示
(setq skk-show-annotation t)                      ; 注釈を表示
(setq skk-annotation-delay 0)                     ; 即座に表示

;; 背景色
(setq skk-candidate-buffer-background-color "grey30")
(setq skk-candidate-buffer-background-color-odd "grey20")

;; fringe を消す
(setq skk-candidate-buffer-display-fringes nil)

;; 表示の調整
(setq skk-treat-candidate-appearance-function
      #'(lambda (candidate listing-p)
          (let* ((value (skk-treat-strip-note-from-word candidate))
                 (cand (car value))     ;候補
                 (note (cdr value))     ;注釈
                 (sep (if note          ;セパレータ
                          (propertize (if (skk-annotation-display-p 'list)
                                          " = "
                                        " !")
                                      'face 'skk-emacs-jisx0208-latin-face)
                        nil)))
            (cond (note
                   (put-text-property 0 (length cand)
                                      'face 'skk-emacs-jisx0201-face cand)
                   (put-text-property 0 (length note)
                                      'face 'skk-emacs-katakana-face note)
                   (cons cand (cons sep note)))
                  (t
                   (put-text-property 0 (length cand)
                                      'face 'skk-emacs-hiragana-face cand)
                   cand)))))


;;; 動的候補表示
(setq skk-dcomp-activate t)             ; 動的補完
(setq skk-dcomp-multiple-activate t)    ; 動的補完の複数候補表示
(setq skk-dcomp-multiple-rows 10)       ; 動的補完の候補表示件数

;; 動的補完の複数表示群のフェイス
(set-face-foreground 'skk-dcomp-multiple-face "Black")
(set-face-background 'skk-dcomp-multiple-face "LightGoldenrodYellow")
(set-face-bold 'skk-dcomp-multiple-face nil)

;; 動的補完の複数表示郡の補完部分のフェイス
(set-face-foreground 'skk-dcomp-multiple-trailing-face "dim gray")
(set-face-bold 'skk-dcomp-multiple-trailing-face nil)

;; 動的補完の複数表示郡の選択対象のフェイス
(set-face-foreground 'skk-dcomp-multiple-selected-face "White")
(set-face-background 'skk-dcomp-multiple-selected-face "LightGoldenrod4")
(set-face-bold 'skk-dcomp-multiple-selected-face nil)


;;; 動作
(require 'skk-hint)                   ; ヒントを使う
(setq skk-verbose t)                  ; 詳細なメッセージを表示
(setq skk-use-kakasi t)               ; 漢字→かな変換を使う
(setq skk-sticky-key ";")             ; ";"をsticky shift keyに
(setq skk-comp-circulate t)           ; 見出し語の補完時の候補の表示順
(setq skk-hint-start-char ?:)         ; ヒントを表示するキー
(setq skk-egg-like-newline t)         ; Enterで改行しない
(setq skk-auto-insert-paren t)        ; 閉じカッコを自動的に
(setq skk-auto-start-henkan t)        ; 区切り文字で自動変換
(setq skk-share-private-jisyo t)      ; 個人辞書を複数Emacsで共有
(setq skk-delete-implies-kakutei nil) ; ▼モードで一つ前の候補を表示する
(setq skk-previous-candidate-keys '("x")) ; 前候補表示キーからC-pを除外
(setq skk-search-katakana 'jisx0201-kana) ; カタカナを変換候補に入れる
(setq skk-henkan-strict-okuri-precedence t) ; 送り仮名が厳密に正しい候補を優先して表示
(setq skk-use-auto-enclose-pair-of-region t) ; リージョンを括弧で囲む
(setq skk-j-mode-function-key-usage 'conversion) ; fnキーを使って変換
(add-hook 'skk-load-hook                  ; 自動的に入力モードを切り替え
      (lambda ()
        (require 'context-skk)))


;;; 言語
(setq skk-japanese-message-and-error t) ; エラーを日本語に
(setq skk-show-japanese-menu t)         ; メニューを日本語に


;;; 個人辞書の自動保存
(defvar my/skk-auto-save-jisyo-interval 600
  "Interval of saving jisyo.")
(run-with-idle-timer my/skk-auto-save-jisyo-interval
                     my/skk-auto-save-jisyo-interval
                     'skk-save-jisyo)


;;; 基本辞書
(setq skk-large-jisyo "c:/Users/hajimetch/Dropbox/Emacs/ddskk/SKK-JISYO.L")


;;; 次候補を表示
(defun my/skk-next-candidate ()
  "Show next candidate of skk."
  (interactive)
  (cond ((eq skk-henkan-mode 'on)
         (skk-comp-wrapper t))
        ((eq skk-henkan-mode 'active)
         (skk-start-henkan t))
        (t (next-line))))


;;; 前候補を表示
(defun my/skk-previous-candidate ()
  "Show previous candidate of skk."
  (interactive)
  (cond ((eq skk-henkan-mode 'on)
         (skk-comp-previous t))
        ((eq skk-henkan-mode 'active)
         (skk-previous-candidate t))
        (t (previous-line))))


;;; チュートリアルのパス
(setq skk-tut-file "c:/Users/hajimetch/Dropbox/Emacs/ddskk/SKK.tut")
