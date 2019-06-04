;;; 初期画面の非表示
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)


;;; フレーム
(setq default-frame-alist
      (append '((line-spacing         . 0  ) ; 文字間隔
                (left-fringe          . 10 ) ; 左フリンジ幅
                (right-fringe         . 12 ) ; 右フリンジ幅
                (menu-bar-lines       . 1  ) ; メニューバー
                (tool-bar-lines       . nil) ; ツールバー
                (vertical-scroll-bars . nil) ; スクロールバー
                (alpha                . 95 ) ; 透明度
                ) default-frame-alist))
(setq initial-frame-alist default-frame-alist)


;;; タイトル
(when window-system
  (setq display-time-string-forms
        '((format "%s/%s/%s" year month day)
          (format "(%s:%s)" 24-hours minutes)))
  (display-time)
  (setq frame-title-format '("Emacs " emacs-version
                             " - " global-mode-string
                             (:eval (if (buffer-file-name) " - %f" " - %b")))))


;;; 行番号
;; バッファ中の行番号表示はしない(パフォーマンス対策)
(global-linum-mode 0)


;;; 空白文字
;; 空白を視覚化
(use-package whitespace :demand
  :bind ("C-c s"    . whitespace-cleanup)
  :custom
  (whitespace-style '(face              ; faceで可視化
                      tabs              ; タブ
                      trailing          ; 行末
                      spaces            ; スペース
                      empty             ; 先頭/末尾の空行
                      space-mark        ; 表示のマッピング(space)
                      tab-mark          ; 表示のマッピング(tab)
                      ))
  (whitespace-space-regexp "\\(\x3000+\\)") ; "　"と"	"を強調表示
  (whitespace-display-mappings
   '((space-mark ?\x3000 [?\□])
     (tab-mark ?\t [?\xBB ?\t]) ))
  :config
  (defvar my/whitespace-fg "red1")      ; 色設定
  (defvar my/whitespace-bg "red4")
  (defvar my/default-fg (face-attribute 'default :foreground))
  (defvar my/default-bg (face-attribute 'default :background))
  (set-face-attribute 'whitespace-trailing nil
                      :foreground my/default-fg
                      :background my/whitespace-bg)
  (set-face-attribute 'whitespace-tab nil
                      :foreground my/whitespace-fg
                      :background my/default-bg)
  (set-face-attribute 'whitespace-space nil
                      :foreground my/whitespace-fg
                      :background my/default-bg)
  (set-face-attribute 'whitespace-empty nil
                      :foreground my/default-fg
                      :background my/whitespace-bg)
  (global-whitespace-mode t))

;; タブ
(setq-default indent-tabs-mode nil)     ; タブ無効化
(setq-default tab-width 4)              ; タブ幅を 4 に設定


;;; フォント
(set-face-attribute 'default nil :family "Ricty Diminished Discord" :height 120)
(set-face-attribute 'variable-pitch nil :family "Ricty Diminished Discord" :height 120)
(set-face-attribute 'fixed-pitch nil :family "Ricty Diminished Discord" :height 120)
(set-face-attribute 'tooltip nil :family "Ricty Diminished Discord" :height 120)


;;; 括弧
;; 括弧にカラフルな色を付ける
(use-package rainbow-delimiters :ensure
  :init (use-package color)
  :hook ((prog-mode . rainbow-delimiters-mode)
         (emacs-startup . my/rainbow-delimiters-using-stronger-colors))
  :config
  (defun my/rainbow-delimiters-using-stronger-colors ()
    "Run rainbow-delimiters using stronger colors."
    (interactive)
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30)))))

;; 自動的に括弧を付ける
(use-package smartparens :ensure
  :init (require 'smartparens-config)
  :hook (prog-mode . smartparens-mode))


;;; モードライン
;; 行列番号の表示
(line-number-mode t)
(column-number-mode t)

;; リージョン内の行数と文字数をモードラインに表示する
(defun my/count-lines-and-chars ()
  "Function used to show number of lines and chars of region in modeline."
  (if mark-active
      (format "(%dlines, %dchars) "
              (count-lines (region-beginning)(region-end))
              (- (region-end)(region-beginning)))
    ""))

;; モードラインカスタマイズ
(setq-default
 mode-line-format
 `(
   ""
   (:eval (my/count-lines-and-chars))
   mode-line-mule-info
   mode-line-modified
   mode-line-frame-identification
   mode-line-buffer-identification
   " "
   " %[("
   mode-name
   mode-line-process
   "%n"
   ")%] "
   (which-func-mode ("" which-func-format " "))
   (line-number-mode
    (:eval
     (format "- L%%l/L%d" (count-lines (point-max) 1) )))
   (column-number-mode " C%c ")
   (-3 . "%p")
   )
 )

;; cp932 エンコードの表記変更
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)

;; UTF-8 エンコードの表記変更
(coding-system-put 'utf-8 :mnemonic ?U)
(coding-system-put 'utf-8-with-signature :mnemonic ?u)

;; 改行コードの表記追加
(setq eol-mnemonic-dos       ":Dos ")
(setq eol-mnemonic-mac       ":Mac ")
(setq eol-mnemonic-unix      ":Unx ")
(setq eol-mnemonic-undecided ":??? ")


;;; ハイライト
;; カーソル行ハイライト
(use-package hl-line
  :custom
  (global-hl-line-timer
   (run-with-idle-timer 0.03 t 'my/global-hl-line-timer-function))
  :config
  ;; ハイライトを無効にするメジャーモードの指定
  (defvar my/global-hl-line-timer-exclude-modes '(todotxt-mode)
    "Major mode for disabling hl-line.")
  ;; ハイライトに0.03秒の猶予を与える(パフォーマンス対策)
  (defun my/global-hl-line-timer-function ()
    "Function used to smooth cursor movement."
    (unless (memq major-mode my/global-hl-line-timer-exclude-modes)
      (global-hl-line-unhighlight-all)
      (let ((global-hl-line-mode t))
        (global-hl-line-highlight)))))

;; ハイライトで視覚的フィードバック
(use-package volatile-highlights :ensure
  :config
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  (volatile-highlights-mode t))

;; 対応する括弧をハイライト
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; 選択範囲をハイライト
(transient-mark-mode t)


;;; カーソル
(setq-default cursor-in-non-selected-windows t) ; 非アクティブでもカーソル表示
(setq-default cursor-type '(bar . 2))           ; カーソルの形状
(blink-cursor-mode 0)                           ; カーソルを点滅しない


;;; バッファ
;; 同一バッファ名にディレクトリ付与
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re "*[^*]+*"))

;; 行の文字数の目印を付ける
(use-package fill-column-indicator :ensure
  :custom
  (fci-rule-width 1)
  (fci-rule-color "dim gray")
  :config
  (define-globalized-minor-mode global-fci-mode
    fci-mode (lambda () (fci-mode t)))
  (global-fci-mode t))

;; バッファの終端を明示する
(setq-default indicate-empty-lines t)

;; バッファ再読み込み関数
(defun my/revert-buffer ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))


;;; ウィンドウ
;; ElScreen
(use-package elscreen :ensure
  :defer nil
  :bind
  ("<f12>"          . elscreen-next)
  ("<f11>"          . elscreen-previous)
  :config
  (elscreen-start)
  (bind-key "C-z" 'iconify-or-deiconify-frame elscreen-map))

;; shackle
(use-package shackle :ensure
  :custom
  (shackle-rules
   '((compilation-mode :align below :ratio 0.3)
     ("*Completions*" :align below :ratio 0.3)
     ("*Help*" :align below :ratio 0.4 :select t)
     ("*eshell*" :align below :ratio 0.4 :popup t)
     ("*候補*" :align below :ratio 0.3)
     ("*SKK annotation*" :align below :ratio 0.3)))
  :config (shackle-mode t))

;; rotete-window
(use-package rotate :ensure
  :bind
  ("M-t"            . rotate-window)
  :config
  (defadvice rotate-window              ; カーソルを元のウィンドウに残す
      (after rotate-cursor activate) (other-window 1)))


;;; スクロール
(setq scroll-preserve-screen-position t) ; カーソル位置を維持
(setq scroll-margin 0)                  ; スクロール開始の残り行数
(setq scroll-conservatively 10000)      ; 1行ずつスクロール
(setq next-screen-context-lines 1)      ; 画面スクロール時の重複行数
(setq recenter-positions '(middle top bottom)) ; recenter時のポジション
(setq hscroll-margin 1)                 ; 横スクロール開始の残り列数
(setq hscroll-step 1)                   ; 1列ずつスクロール


;;; 選択領域・カット
;; hungry-delete
(use-package hungry-delete :ensure
  :config (global-hungry-delete-mode t))

;; 矩形選択
(cua-mode t)
(setq cua-enable-cua-keys nil)

(delete-selection-mode t)       ; 選択領域も一括削除
(setq kill-whole-line t)        ; C-k で行末の改行も削除
(setq kill-read-only-ok t)      ; 読み取り専用バッファもカットでコピー


;;; その他
(setq ring-bell-function 'ignore)       ; アラートのビープ音は消す
(setq auto-image-file-mode t)           ; 画像ファイルは画像として表示
