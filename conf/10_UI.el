;;; 初期画面の非表示
(set-variable 'inhibit-startup-message t)
(set-variable 'inhibit-startup-screen t)


;;; フレーム
(set-variable 'default-frame-alist
              (append '((line-spacing         . 0  ) ; 文字間隔
                        (left-fringe          . 10 ) ; 左フリンジ幅
                        (right-fringe         . 12 ) ; 右フリンジ幅
                        (menu-bar-lines       . 1  ) ; メニューバー
                        (tool-bar-lines       . nil) ; ツールバー
                        (vertical-scroll-bars . nil) ; スクロールバー
                        (alpha                . 95 ) ; 透明度
                        ) default-frame-alist))
(set-variable 'initial-frame-alist default-frame-alist)


;;; タイトル
(when window-system
  (setq frame-title-format '("Emacs " emacs-version " - "
                             (:eval (if (buffer-file-name) " - %f" " - %b")))))


;;; 行番号
;; バッファ中の行番号表示はしない(パフォーマンス対策)
(global-linum-mode 0)


;;; 空白文字
;; 空白を視覚化
(use-package whitespace :demand
  :bind ("C-c s"    . whitespace-cleanup)
  :config
  (set-variable 'whitespace-style
                '(face                  ; faceで可視化
                  tabs                  ; タブ
                  trailing              ; 行末
                  spaces                ; スペース
                  empty                 ; 先頭/末尾の空行
                  space-mark            ; 表示のマッピング(space)
                  tab-mark              ; 表示のマッピング(tab)
                  ))
  (set-variable 'whitespace-space-regexp "\\(\x3000+\\)")
                                        ; "　"と"	"を強調表示
  (set-variable 'whitespace-display-mappings
                '((space-mark ?\x3000 [?\□])
                  (tab-mark ?\t [?\xBB ?\t]) ))
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
  :init (use-package smartparens-config)
  :hook (prog-mode . smartparens-mode))


;;; モードライン
(use-package smart-mode-line :ensure
  :init
  (use-package diminish :ensure)
  (use-package total-lines :ensure)
  :hook
  (after-init . my/set-line-numbers)
  :config
  (sml/setup)
  ;; 行列番号を表示
  (line-number-mode t)
  (column-number-mode t)
  ;; 総行数を表示
  (global-total-lines-mode t)
  (defun my/set-line-numbers ()
    "Set total line numbers to modeline."
    (setq-default mode-line-front-space
                  (append mode-line-front-space
                          '((:eval (format " (%d)" (- total-lines 1)))))))
  ;; アラート時にモードラインを光らせる(紫)
  (setq ring-bell-function
        (lambda ()
          (let ((orig-fg (face-background 'mode-line)))
            (set-face-background 'mode-line "purple4")
            (run-with-idle-timer 0.1 nil
                                 (lambda (fg) (set-face-background 'mode-line fg))
                                 orig-fg))))
  ;; 保存時にモードラインを光らせる(緑)
  (add-hook 'after-save-hook
            (lambda ()
              (let ((orig-fg (face-background 'mode-line)))
                (set-face-background 'mode-line "dark green")
                (run-with-idle-timer 0.1 nil
                                     (lambda (fg) (set-face-background 'mode-line fg))
                                     orig-fg)))))


;;; ハイライト
;; カーソル行ハイライト
(require 'hl-line)
(with-eval-after-load 'hl-line
  ;; ハイライトを無効にするメジャーモードの指定
  (defvar my/global-hl-line-timer-exclude-modes '(todotxt-mode)
    "Major mode for disabling hl-line.")
  ;; ハイライトに0.03秒の猶予を与える(パフォーマンス対策)
  (defun my/global-hl-line-timer-function ()
    "Function used to smooth cursor movement."
    (unless (memq major-mode my/global-hl-line-timer-exclude-modes)
      (global-hl-line-unhighlight-all)
      (let ((global-hl-line-mode t))
        (global-hl-line-highlight))))
  (setq global-hl-line-timer
        (run-with-idle-timer 0.03 t 'my/global-hl-line-timer-function)))

;; ハイライトで視覚的フィードバック
(use-package volatile-highlights :ensure
  :config
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  (volatile-highlights-mode t))

;; 対応する括弧をハイライト
(show-paren-mode t)
(set-variable 'show-paren-style 'mixed)

;; 選択範囲をハイライト
(transient-mark-mode t)


;;; カーソル
(setq-default cursor-in-non-selected-windows t) ; 非アクティブでもカーソル表示
(setq-default cursor-type '(bar . 2))           ; カーソルの形状
(blink-cursor-mode 0)                           ; カーソルを点滅しない


;;; バッファ
;; 同一バッファ名にディレクトリ付与
(with-eval-after-load 'uniquify
  (set-variable 'uniquify-buffer-name-style 'post-forward-angle-brackets)
  (set-variable 'uniquify-ignore-buffers-re "*[^*]+*"))

;; 行の文字数の目印を付ける
(use-package fill-column-indicator :ensure
  :config
  (set-variable 'fci-rule-width 1)
  (set-variable 'fci-rule-color "dim gray")
  (define-globalized-minor-mode global-fci-mode
    fci-mode (lambda () (fci-mode t)))
  (global-fci-mode t))

;; バッファの終端を明示する
(setq-default indicate-empty-lines t)

;; バッファ再読み込み関数
(defun my/revert-buffer ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
(bind-key "C-c r" 'my/revert-buffer)


;;; ウィンドウ
;; ElScreen
(use-package elscreen :ensure :demand
  :bind
  ("<f12>"          . elscreen-next)
  ("<f11>"          . elscreen-previous)
  :config
  (elscreen-start)
  (bind-key "C-z" 'iconify-or-deiconify-frame elscreen-map))

;; shackle
(use-package shackle :ensure
  :config
  (set-variable 'shackle-rules
                '((compilation-mode :align below :ratio 0.3)
                  ("*Completions*" :align below :ratio 0.3)
                  ("*Help*" :align below :ratio 0.4 :select t)
                  ("*eshell*" :align below :ratio 0.4 :popup t)
                  ("*候補*" :align below :ratio 0.3)
                  ("*SKK annotation*" :align below :ratio 0.3)))
  (shackle-mode t))

;; rotete-window
(use-package rotate :ensure
  :bind
  ("M-t"            . rotate-window)
  :config
  (defadvice rotate-window            ; カーソルを元のウィンドウに残す
      (after rotate-cursor activate) (other-window 1)))


;;; スクロール
(set-variable 'scroll-preserve-screen-position t) ; カーソル位置を維持
(set-variable 'scroll-margin 0)             ; スクロール開始の残り行数
(set-variable 'scroll-conservatively 10000) ; 1行ずつスクロール
(set-variable 'next-screen-context-lines 1) ; 画面スクロール時の重複行数
(set-variable 'recenter-positions '(middle top bottom)) ; recenter時のポジション
(set-variable 'hscroll-margin 1)        ; 横スクロール開始の残り列数
(set-variable 'hscroll-step 1)          ; 1列ずつスクロール


;;; 選択領域・カット
;; hungry-delete
(use-package hungry-delete :ensure
  :config (global-hungry-delete-mode t))

;; 矩形選択
(cua-mode t)
(set-variable 'cua-enable-cua-keys nil)

(delete-selection-mode t)               ; 選択領域も一括削除
(set-variable 'kill-whole-line t)       ; C-k で行末の改行も削除
(set-variable 'kill-read-only-ok t) ; 読み取り専用バッファもカットでコピー


;;; その他
(set-variable 'ring-bell-function 'ignore) ; アラートのビープ音は消す
(set-variable 'auto-image-file-mode t)  ; 画像ファイルは画像として表示
