;;;; Modified: 2019-06-19
;;; package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)


;;; refresh package information
(unless package-archive-contents
  (package-refresh-contents))


;;; ensure to use use-package
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)


;;; server start for emacsclient
(require 'server)
(set-variable 'server-auth-dir "~/.emacs.server")
(unless (eq (server-running-p) 't)
  (server-start)
  (defun my/iconify-emacs-when-server-is-done ()
    (unless server-clients (iconify-frame)))
  (add-hook 'after-init-hook 'my/iconify-emacs-when-server-is-done) ; minimize when start
  (bind-key "C-x C-c" 'server-edit))      ; do not exit when C-x C-c
(defalias 'exit 'save-buffers-kill-emacs) ; exit by M-x exit


;;; function to add load-path (including sub-directory)
(defun my/add-to-load-path (&rest paths)
  "Function to add load-path including sub-directory."
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-path
(my/add-to-load-path "elisp")


;;; load theme
(use-package tangotango-theme :no-require :defer :ensure)


;;; custom-file
(set-variable 'custom-file (locate-user-emacs-file "custom.el"))

;; if not exists, create custom-file
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; load custom-file
(load custom-file)


;;; auto-compile
(use-package auto-compile :no-require :defer :ensure
  :diminish "C"
  :hook (emacs-lisp-mode . auto-compile-mode))


;;;; 00_defaults.el
;;; Path
(set-variable 'exec-path (append exec-path '("C:/Program Files/Git/cmd")))
(set-variable 'exec-path (append exec-path '("C:/Program Files/Git/mingw64/bin")))
(set-variable 'exec-path (append exec-path '("C:/Program Files/Git/usr/bin")))


;;; Programs for Windows を指定
(setq null-device "/dev/null")
(setq find-program "\"C:/Program Files/Git/usr/bin/find.exe\"")
(setq grep-program "\"C:/Program FIles/Git/usr/bin/grep.exe\"")
(set-variable 'diff-command "\"C:/Program Files/Git/usr/bin/diff.exe\"")


;;; Desktop
(desktop-save-mode t)

;; テーマをデスクトップ復元後にロード
(add-to-list 'desktop-globals-to-save 'custom-enabled-themes)
(defun my/desktop-load-theme ()
  "Load custom theme."
  (interactive)
  (dolist (th custom-enabled-themes) (load-theme th)))
(add-hook 'desktop-after-read-hook 'my/desktop-load-theme)


;;; 文字コード
(set-language-environment "Japanese")     ; 言語環境
(set-default-coding-systems 'utf-8-unix)  ; デフォルト文字コード
(prefer-coding-system 'utf-8-unix)        ; Text File / 新規バッファ
(set-file-name-coding-system 'cp932)      ; ファイル名
(set-keyboard-coding-system 'cp932)       ; キーボード入力
(set-terminal-coding-system 'cp932)       ; ターミナル
(setq locale-coding-system 'utf-8-unix)   ; システムメッセージ

;; サブプロセスのデフォルト文字コード
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; サブプロセスに渡すパラメータの文字コードを cp932 にする
(cl-loop for (func args-pos) in '((call-process        4)
                                  (call-process-region 6)
                                  (start-process       3))
         do (eval `(advice-add ',func
                               :around (lambda (orig-fun &rest args)
                                         (if (nthcdr ,args-pos args)
                                             (setf (nthcdr ,args-pos args)
                                                   (mapcar (lambda (arg)
                                                             (if (multibyte-string-p arg)
                                                                 (encode-coding-string arg 'cp932)
                                                               arg))
                                                           (nthcdr ,args-pos args))))
                                         (apply orig-fun args))
                               '((depth . 99)))))

;; 環境依存文字 文字化け対応
(set-charset-priority 'ascii
                      'japanese-jisx0208
                      'latin-jisx0201
                      'katakana-jisx0201
                      'iso-8859-1
                      'cp1252
                      'unicode)
(set-coding-system-priority 'utf-8
                            'euc-jp
                            'iso-2022-jp
                            'cp932)


;;; 検索
;; 大文字・小文字を区別しない
(setq-default case-fold-search nil)
(set-variable 'read-buffer-completion-ignore-case t) ; バッファ名検索
(set-variable 'read-file-name-completion-ignore-case t) ; ファイル名検索

;; インクリメント検索時に縦スクロールを有効化
(set-variable 'isearch-allow-scroll nil)

;; migemo
(use-package migemo :no-require :ensure
  :if (executable-find "cmigemo")
  :config
  (set-variable 'migemo-command "cmigemo")
  (set-variable 'migemo-options '("-q" "--emacs"))
  (set-variable 'migemo-dictionary "C:/tools/cmigemo/dict/utf-8/migemo-dict")
  (set-variable 'migemo-coding-system 'utf-8-unix)
  (defvar migemo-user-dictionary nil)
  (defvar migemo-regex-dictionary nil)
  (load-library "migemo")
  (migemo-init))

;; anzu
(use-package anzu :no-require :ensure
  :config (global-anzu-mode t))


;;; バックアップ(xxx~)
(set-variable 'make-backup-files     t) ; 自動バックアップの実行有無
(set-variable 'version-control       t) ; バックアップファイルへの番号付与
(set-variable 'kept-new-versions   100) ; 最新バックアップファイルの保持数
(set-variable 'kept-old-versions     1) ; 最古バックアップファイルの保持数
(set-variable 'delete-old-versions   t) ; バックアップファイル削除の実行有無

;; バックアップ(xxx~)の格納ディレクトリ
(set-variable 'backup-directory-alist '((".*" . "C:/Users/hajimetch/Dropbox/Emacs/backups/win10")))

;; バッファ保存時に毎回バックアップする
(defun my/setq-buffer-backed-up-nil (&rest _)
  "Function used to always backup buffer when saved."
  (interactive) (setq buffer-backed-up nil))
(advice-add 'save-buffer :before 'my/setq-buffer-backed-up-nil)


;;; 自動保存ファイル(#xxx#)
;; 作成する
(set-variable 'auto-save-default     t)

;; 保存の間隔
(set-variable 'auto-save-timeout    10) ; 秒
(set-variable 'auto-save-interval  100) ; 打鍵

;; 自動保存ファイル(#xxx#)の格納ディレクトリ
(set-variable 'auto-save-file-name-transforms
              `((".*", (expand-file-name "C:/Users/hajimetch/Dropbox/Emacs/backups/win10/") t)))


;;; 自動保存のリスト(~/.emacs.d/auto-save-list/.saves-xxx)
;; 下記プレフィックスで作成する
(set-variable 'auto-save-list-file-prefix "C:/Users/hajimetch/Dropbox/Emacs/backups/mac/saves-")


;;; ロックファイル(.#xxx)
;; 作成しない
(set-variable 'create-lockfiles    nil)


;;; 特定のファイルではバックアップを作成しない
(defvar my/backup-inhibit-file-name-regexp "recentf"
  "Regexp of file name not for backup.")
(defun my/backup-enable-predicate (filename)
  "Function used to inhibit from backing up files specified by var my/backup-inhibit-file-name-regexp."
  (save-match-data
    (and (not (string-match my/backup-inhibit-file-name-regexp filename))
         (normal-backup-enable-predicate filename))))
(setq backup-enable-predicate 'my/backup-enable-predicate)


;;; recentf 関連
(use-package recentf-ext :no-require :ensure)

;; recentf から除外するファイル
(set-variable 'recentf-exclude (list "recentf"
                                     (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME"))))

;; recentf に保存するファイル数
(set-variable 'recentf-max-saved-items 1000)

;; *Messages* に不要な出力を行わないようにする
(defmacro my/with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

;; 30秒ごとに recentf を保存
(run-with-idle-timer 30 t '(lambda ()
                             (my/with-suppressed-message (recentf-save-list))))

;; recentf を自動クリーンアップしない
(set-variable 'recentf-auto-cleanup 'never)


;;; undo 関連
;; undohist
(use-package undohist :ensure
  :config
  (setq undohist-ignored-files '("COMMIT_EDITMSG"))
  (undohist-initialize))

;; Windows に対応
(defun make-undohist-file-name (file)
  "Function used to apply undohist to Windows system."
  (setq file (convert-standard-filename (expand-file-name file)))
  (if (eq (aref file 1) ?:)
      (setq file (concat "/"
                         "drive_"
                         (char-to-string (downcase (aref file 0)))
                         (if (eq (aref file 2) ?/)
                             ""
                           (if (eq (aref file 2) ?\\)
                               ""
                             "/"))
                         (substring file 2))))
  (setq file (expand-file-name
              (subst-char-in-string
               ?/ ?!
               (subst-char-in-string
                ?\\ ?!
                (replace-regexp-in-string "!" "!!"  file)))
              undohist-directory)))

;; undo-tree
(use-package undo-tree :no-require :ensure
  :diminish "UTree"
  :config (global-undo-tree-mode t))

;; point-undo
(use-package point-undo :demand
  :bind
  ("M-["            . point-undo)
  ("M-]"            . point-redo))


;;; company
;; company
(use-package company :no-require :ensure
  :diminish "Comp"
  :bind
  (("TAB"           . company-complete)
   ("M-/"           . company-dabbrev)
   :map company-active-map
   ("C-d"           . company-filter-candidates)
   ("C-n"           . company-select-next)
   ("C-p"           . company-select-previous)
   :map company-search-map
   ("C-n"           . company-select-next)
   ("C-p"           . company-select-previous))
  :config (global-company-mode t))

;; company-quickhelp
(use-package company-quickhelp :no-require :ensure
  :after company
  :config (company-quickhelp-mode t))


;;; which-key
(use-package which-key :no-require :ensure
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode t))


;;; multiple-cursor
(use-package multiple-cursors :no-require :ensure
  :bind
  (("C->"           . mc/mark-next-like-this)
   ("C-<"           . mc/mark-previous-like-this)
   ("C-c e"         . mc/edit-lines)
   ("C-c h"         . mc/mark-all-like-this)))


;;; expand-region
(use-package expand-region :no-require :ensure
  :bind
  ("C-="            . er/expand-region)
  ("C--"            . er/contract-region))


;;; outline-minor-mode
(with-eval-after-load 'outline
  (bind-keys :map outline-minor-mode-map
             ("<tab>"   . org-cycle)
             ("<C-tab>" . org-global-cycle)))


;;; ediff
;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; diff のバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)


;;; abbrev file
(set-variable 'abbrev-file-name "C:/Users/hajimetch/Dropbox/Emacs/abbrev_defs")
(set-variable 'save-abbrevs t)
(quietly-read-abbrev-file)
(set-variable 'save-abbrevs 'silently)


;;; time-stamp
(with-eval-after-load 'time-stamp
  (set-variable 'time-stamp-active t))
(add-hook 'before-save-hook 'time-stamp)


;;; その他
;; dired バッファを並べる
(set-variable 'dired-dwim-target t)

;; 画像ファイルは画像として表示
(set-variable 'auto-image-file-mode t)

;; ファイルが #! から始まる場合、+x を付けて保存する
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Error 対応 (ad-handle-definition: command got redefined)
(set-variable 'ad-redefinition-action 'accept)


;;;; 10_UI.el
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
  (setq frame-title-format '("Emacs " emacs-version
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
(use-package rainbow-delimiters :no-require :ensure
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
(use-package smartparens :no-require :ensure
  :init (use-package smartparens-config :no-require)
  :hook (prog-mode . smartparens-mode))


;;; モードライン
(use-package smart-mode-line :no-require :ensure
  :init
  (use-package diminish :no-require :ensure)
  (use-package total-lines :no-require :ensure)
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
  (set-variable 'ring-bell-function
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
(use-package fill-column-indicator :no-require :ensure
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
(use-package elscreen :no-require :demand :ensure
  :bind
  ("<f12>"          . elscreen-next)
  ("<f11>"          . elscreen-previous)
  :config
  (elscreen-start)
  (bind-key "C-z" 'iconify-or-deiconify-frame elscreen-map))

;; shackle
(use-package shackle :no-require :ensure
  :config
  (set-variable 'shackle-rules
                '((compilation-mode :align below :ratio 0.3)
                  ("*Completions*" :align below :ratio 0.3)
                  ("*Help*" :align below :ratio 0.4 :select t)
                  ("*eshell*" :align below :ratio 0.4 :popup t)))
  (shackle-mode t))

;; rotete-window
(use-package rotate :no-require :ensure
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
(use-package hungry-delete :no-require :ensure
  :config (global-hungry-delete-mode t))

;; 矩形選択
(cua-mode t)
(set-variable 'cua-enable-cua-keys nil)

(delete-selection-mode t)               ; 選択領域も一括削除
(set-variable 'kill-whole-line t)       ; C-k で行末の改行も削除
(set-variable 'kill-read-only-ok t) ; 読み取り専用バッファもカットでコピー


;;;; 20_IME.el
(unless (locate-library "skk")
  (package-install 'ddskk))
(use-package skk :no-require
  :init
  (set-variable 'skk-user-directory "C:/Users/hajimetch/Dropbox/Emacs/ddskk/")
  (use-package skk-study)               ; 変換学習機能
  (use-package skk-hint)                ; ヒント
  (use-package context-skk :no-require) ; 自動的にモード切り替え
  (use-package sticky :no-require :ensure) ; skk-sticky-keyに必要

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

  :config
  ;; 全般
  (set-variable 'default-input-method "japanese-skk") ; 日本語入力にskkを使用
  (set-variable 'skk-server-host "localhost") ; サーバー機能を利用
  (set-variable 'skk-server-portnum 55100)    ; ポートはgoogle-ime-skk
  (set-variable 'skk-share-private-jisyo t)   ; 複数 skk 辞書を共有

  ;; 候補表示
  (set-variable 'skk-show-inline t)     ; インライン表示
  (set-variable 'skk-show-annotation nil) ; 注釈は非表示
  (set-variable 'skk-inline-show-face 'skk-henkan-face-default) ; フェイス

  ;; 動的候補表示
  (set-variable 'skk-dcomp-activate t)          ; 動的補完を行う
  (set-variable 'skk-dcomp-multiple-activate t) ; 動的補完の複数候補表示
  (set-variable 'skk-dcomp-multiple-rows 10) ; 動的補完の候補表示件数

  ;; 動作
  (set-variable 'skk-verbose t)        ; 詳細なメッセージを表示
  (set-variable 'skk-use-kakasi t)     ; 漢字→かな変換を使う
  (set-variable 'skk-sticky-key ";")   ; ";"をsticky shift keyに
  (set-variable 'skk-comp-circulate t) ; 見出し語の補完時の候補の表示順
  (set-variable 'skk-hint-start-char ?:)    ; ヒントを表示するキー
  (set-variable 'skk-egg-like-newline t)    ; Enterで改行しない
  (set-variable 'skk-auto-insert-paren t)   ; 閉じカッコを自動的に
  (set-variable 'skk-auto-start-henkan t)   ; 区切り文字で自動変換
  (set-variable 'skk-share-private-jisyo t) ; 個人辞書を複数Emacsで共有
  (set-variable 'skk-delete-implies-kakutei nil) ; ▼モードで一つ前の候補を表示する
  (set-variable 'skk-previous-candidate-keys '("x")) ; 前候補表示キーからC-pを除外
  (set-variable 'skk-search-katakana 'jisx0201-kana) ; カタカナを変換候補に入れる
  (set-variable 'skk-henkan-strict-okuri-precedence t) ; 送り仮名が厳密に正しい候補を優先して表示
  (set-variable 'skk-use-auto-enclose-pair-of-region t) ; リージョンを括弧で囲む
  (set-variable 'skk-compare-jisyo-size-when-saving nil) ; 辞書サイズをチェックkしない
  (set-variable 'skk-j-mode-function-key-usage 'conversion) ; fnキーを使って変換

  ;; skk-isearch
  (set-variable 'skk-isearch-start-mode 'latin) ; isearch での skk の初期状態
  (set-variable 'search-nonincremental-instead nil) ; Enter で終了

  ;; 言語
  (set-variable 'skk-japanese-message-and-error t) ; エラーを日本語に
  (set-variable 'skk-show-japanese-menu t) ; メニューを日本語に

  ;; 基本辞書
  (set-variable 'skk-large-jisyo "C:/Users/hajimetch/Dropbox/Emacs/ddskk/SKK-JISYO.L")

  ;; チュートリアルのパス
  (set-variable 'skk-tut-file "C:/Users/hajimetch/Dropbox/Emacs/ddskk/SKK.tut")

  ;; 動的補完郡のフェイス
  (set-face-attribute 'skk-dcomp-multiple-face nil ; 複数表示郡
                      :foreground "Black"
                      :background "LightGoldenrodYellow"
                      :bold nil)
  (set-face-attribute 'skk-dcomp-multiple-trailing-face nil ; 補完部分
                      :foreground "dim gray"
                      :background "LightGoldenrodYellow"
                      :bold nil)
  (set-face-attribute 'skk-dcomp-multiple-selected-face nil ; 選択対象
                      :foreground "White"
                      :background "LightGoldenrod4"
                      :bold nil)

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


;;;; 30_helm.el
;;; Helm
(use-package helm :no-require :ensure
  :bind
  (("M-x"           . helm-M-x)
   ("C-x C-f"       . helm-find-files)
   ("C-x C-x"       . helm-mini)
   ("C-x C-z"       . helm-resume)
   ("C-c i"         . helm-semantic-or-imenu)
   ("C-c w"         . helm-google-suggest)
   ("C-c C-SPC"     . helm-all-mark-rings)
   ("C-M-y"         . helm-show-kill-ring)
   ("<f2>"          . my/helm-apropos-this)
   :map helm-map
   ("TAB"           . helm-execute-persistent-action)
   ("C-z"           . helm-select-action)
   ("M-b"           . my/helm-ff-run-browse-project)
   ("<f1>"          . helm-help))

  :config
  (bind-key* "M-m" 'helm-migemo-mode helm-map)
  (set-variable 'helm-mini-default-sources ; helm-mini に表示するソース
                '(helm-source-buffers-list
                  helm-source-recentf
                  helm-source-files-in-current-dir))
  (set-variable 'helm-candidate-number-limit 100) ; 表示する最大候補数
  (set-variable 'helm-autoresize-max-height 0) ; Helm バッファのサイズ
  (set-variable 'helm-autoresize-min-height 40)
  (set-variable 'helm-default-display-buffer-functions
                '(display-buffer-in-side-window)) ; Helm バッファは常にウィンドウの下側
  (set-variable 'helm-ff-skip-boring-files t) ; 次のファイルは非表示
  (set-variable 'helm-boring-file-regexp-list (quote ("Icon.$")))
  (set-variable 'helm-scroll-amount 8)  ; 他バッファのスクロール行数
  (set-variable 'helm-split-window-inside-p t)     ; 他バッファを保持
  (set-variable 'helm-ff-search-library-in-sexp t) ; ff でライブラリを検索
  (set-variable 'helm-ff-file-name-history-use-recentf t) ; ff でrecentf を使用

  (helm-mode t)
  (helm-migemo-mode t)
  (helm-autoresize-mode t)

  ;; helm-find-file から browse-project を呼び出す
  (defun my/helm-ff-run-browse-project ()
    "Call helm-ff-run-browse-project with C-u."
    (interactive)
    (setq current-prefix-arg '(4))
    (call-interactively 'helm-ff-run-browse-project))

  ;; カーソル位置のシンボルで helm-apropos を呼び出す
  (defun my/helm-apropos-this ()
    "helm-apropos with this symbol."
    (interactive)
    (helm-apropos (thing-at-point 'symbol)))

  ;; helm-gtags が UNC path 環境下で動作しない問題を回避
  (advice-add 'select-window
              :around (lambda (orig-fun &rest args)
                        (when (nth 0 args)(apply orig-fun args)))))


;;; helm-elscreen
(use-package helm-elscreen :no-require :ensure
  :after (helm elscreen)
  :bind ("C-x C-l"  . helm-elscreen))


;;; helm-ag(ripgrep)
(use-package helm-ag :no-require :ensure
  :after helm
  :bind ("C-c g"    . helm-do-ag)
  :config (set-variable 'helm-ag-base-command "rg --vimgrep --no-heading --smart-case"))


;;; helm-swoop
(use-package helm-swoop :no-require :ensure
  :after helm
  :bind
  (("M-s"           . helm-swoop)
   :map helm-swoop-map
   ("C-s"           . helm-next-line)
   ("C-r"           . helm-previous-line))
  :config
  (set-variable 'helm-swoop-move-to-line-cycle nil) ; リストを循環しない
  (setq helm-swoop-split-window-function 'helm-default-display-buffer))
                                        ; 常に full-width で表示


;;; helm-descbinds
(use-package helm-descbinds :no-require :ensure
  :after helm
  :bind ("C-c k"    . helm-descbinds))


;;; Yasnippet
(use-package helm-c-yasnippet :no-require :ensure
  :init (use-package yasnippet :no-require :ensure)
  :bind ("C-c y"    . helm-yas-complete)
  :config
  (set-variable 'yas-snippet-dirs
                '("C:/Users/hajimetch/Dropbox/Emacs/snippets/mysnippets" ; 自作スニペット
                  "C:/Users/hajimetch/Mac/Dropbox/Emacs/snippets/yasnippets" ; デフォルトスニペット
                  ))
  (set-variable 'helm-yas-space-match-any-greedy t)
  (add-to-list 'auto-mode-alist '("emacs.+/snippets/" . snippet-mode))
  (yas-global-mode t))


;;; Projectile
(use-package projectile :no-require :ensure
  :diminish "Prj"
  :bind-keymap* ("C-c C-p" . projectile-command-map)
  :config
  (set-variable 'projectile-git-command "fd . -0") ; fd を使用
  (set-variable 'projectile-generic-command "fd . -0")
  (set-variable 'projectile-completion-system 'helm)
  (projectile-mode t))

(use-package helm-projectile :no-require :ensure
  :after (helm projectile)
  :bind ("C-x C-p"  . helm-projectile)
  :config
  (helm-projectile-on)

  ;; helm-projectile-ag が ripgrep で動作しない問題を回避
  (defun helm-projectile-ag (&optional options)
    "Helm version of projectile-ag."
    (interactive (if current-prefix-arg (list (read-string "option: " "" 'helm-ag--extra-options-history))))
    (if (require 'helm-ag nil  'noerror)
        (if (projectile-project-p)
            (let ((helm-ag-command-option options)
                  (current-prefix-arg nil))
              (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
          (error "You're not in a project"))
      (error "helm-ag not available"))))


;;;; 40_PL.el
;; py-yapf
(use-package py-yapf :no-require :ensure
  :after python
  :bind
  (:map python-mode-map
        ("C-c C-f"  . py-yapf-buffer))
  :hook (python-mode . py-yapf-enable-on-save))

;; jedi
(use-package company-jedi :no-require :ensure
  :after (python company)
  :init (use-package jedi-core :no-require)
  :hook (python-mode . jedi:setup)
  :config
  (set-variable 'jedi:complete-on-dot t)
  (set-variable 'jedi:use-shortcuts t)
  (add-to-list 'company-backends 'company-jedi)
  (unbind-key "C-c ." jedi-mode-map))


;;; flycheck
(use-package flycheck :no-require :ensure
  :init (use-package flycheck-pos-tip :no-require :ensure)
  :bind
  (:map flycheck-mode-map
        ("C-c C-d"  . flycheck-list-errors))
  :hook (after-init . global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (flycheck-pos-tip-mode t))


;;; semantic-mode
(semantic-mode t)


;;; web-mode
(use-package web-mode :no-require :ensure
  :mode
  ("\\.html\\'"
   "\\.css\\'"
   "\\.jsx\\'"
   "\\.tpl\\.php\\'"
   "\\.ctp\\'"
   "\\.jsp\\'"
   "\\.as[cp]x\\'"
   "\\.erb\\'"))


;;; js2-mode
(use-package js2-mode :no-require :ensure
  :mode "\\.js\\'")


;;; markdown-mode
(use-package markdown-mode :no-require :ensure
  :mode "\\.md'"
  :config
  (set-variable 'markdown-command "C:/Tools/Pandoc/pandoc -s --self-contained -t html5 -c C:/Tools/Pandoc/github.css --quiet")
  (skk-wrap-newline-command markdown-enter-key))


;;; php-mode
(use-package php-mode :no-require :ensure
  :config (setq php-manual-url 'ja))


;;; ini-mode
(use-package ini-mode :no-require :ensure
  :mode "\\.ini\\'")


;;; json-mode
(use-package json-mode :no-require :ensure
  :mode "\\.json\\'")


;;; yaml-mode
(use-package yaml-mode :no-require :ensure
  :mode "\\.ya?ml$")


;;; gtags
(use-package helm-gtags :no-require :ensure
  :diminish "HGtags"
  :after helm
  :bind
  (:map helm-gtags-mode-map
        ("C-c . ."  . helm-gtags-find-tag-from-here)
        ("C-c . ,"  . helm-gtags-pop-stack)
        ("C-c . t"  . helm-gtags-find-tag)
        ("C-c . r"  . helm-gtags-find-rtag)
        ("C-c . s"  . helm-gtags-find-symbol)
        ("C-c . f"  . helm-gtags-find-files))
  :hook (python-mode . helm-gtags-mode)
  :config (set-variable 'helm-gtags-auto-update t))


;;; magit
(use-package magit :no-require :ensure
  :init (use-package ssh-agency :no-require :ensure)
  :bind ("C-x g"    . magit-status)
  :config (setenv "GIT_ASKPASS" "git-gui--askpass"))


;;; git-gutter
(use-package git-gutter :no-require :demand :ensure
  :diminish "GitG"
  :bind
  (("C-x p"         . git-gutter:previous-hunk)
   ("C-x n"         . git-gutter:next-hunk))
  :config (global-git-gutter-mode t))


;;; quickrun
(use-package quickrun :no-require :ensure
  :bind ("C-c q"    . quickrun))


;;;; 50_shell.el
;;; Eshell
;; eshell alias
(setq eshell-command-aliases-list
      (append
       (list
        (list "ll" "ls -lh")
        (list "la" "ls -a")
        (list "emacs" "find-file $1")
        (list "m" "find-file $1")
        (list "mc" "find-file $1")
        (list "d" "dired .")
        (list "l" "my/eshell-less $1 $2"))))

;; view files in eshell
(defun my/eshell-less (&rest args)
  "Invoke `view-file' on the file.
\"less +42 foo\" also goes to line 42 in the buffer."
  (interactive)
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (view-file file)
          (goto-line line))
      (view-file (pop args)))))

;; open eshell with current directory
(defun my/eshell-pop (&optional arg)
  "Popup eshell and cd to current directory."
  (interactive "P")
  (cond ((equal (buffer-name) "*eshell*")
         (kill-buffer-and-window))
        (t
         (let ((dir default-directory))
           (eshell arg)
           (cd dir))
         (goto-char (point-max)))))
(bind-key "C-c t" 'my/eshell-pop)


;;;; 60_org.el
;;; org-mode
(with-eval-after-load 'org
  (bind-keys :map org-mode-map
             ("C-c ("         . org-clock-in)
             ("C-c )"         . org-clock-out))

  (add-hook 'org-mode-hook 'turn-on-font-lock) ; 強調表示を可能に
  (add-hook 'kill-emacs-hook 'my/org-clock-out-and-save)

  (set-variable 'org-startup-with-inline-images t) ; 画像をインラインで表示
  (set-variable 'org-startup-indented t) ; インデントモードにする
  (set-variable 'org-indent-indentation-per-level 1) ; インデントの幅
  (set-variable 'org-directory "C:/Users/hajimetch/Dropbox/Emacs/org/")
  (set-variable 'org-default-notes-file "C:/Users/hajimetch/Dropbox/Emacs/org/default.org")
  (set-variable 'org-log-done 'time)    ; DONE の時刻を記録
  (set-variable 'org-use-speed-commands t)
  (set-variable 'org-todo-keywords
                '((sequence "TODO(t)" "WAIT(w)" "NOTE(n)" "|" "DONE(d)" "SOMEDAY(s)" "CANCEL(c)")))
  (set-variable 'org-agenda-files (list org-directory))
  (set-variable 'org-refile-targets '(("C:/Users/hajimetch/Dropbox/Emacs/org/task.org" :maxlevel . 3)))
  (set-variable 'org-archive-location "C:/Users/hajimetch/Dropbox/Emacs/org/archive.org::datetree/")

  (setq system-time-locale "C")               ; 文字化け対策
  (setq org-export-with-sub-superscripts nil) ; "_"の後下付き文字にしない
  (setq org-agenda-start-with-log-mode '(closed clock))
                                        ; org-agenda を Log mode で開始
  ;; org-clock
  (setq org-clock-into-drawer t)        ; LOGBOOK drawer に時間を格納
  (setq org-clock-out-remove-zero-time-clocks t) ; 1分未満を記録しない
  (setq org-clock-clocked-in-display 'frame-title) ; タスク名をタイトルバーに表示

  ;; テンプレート
  (setq org-capture-templates
        '(("t" "Task" entry
           (file+headline "C:/Users/hajimetch/Dropbox/Emacs/org/task.org" "Task")
           "* TODO %?\n%U" :empty-lines 1)
          ("T" "Task with Clipboard" entry
           (file+headline "C:/Users/hajimetch/Dropbox/Emacs/org/task.org" "Task")
           "* TODO %?\n%U\n%c" :empty-lines 1)
          ("n" "Note" entry
           (file+headline "C:/Users/hajimetch/Dropbox/Emacs/org/note.org" "Note")
           "* %?\n%U" :empty-lines 1)
          ("N" "Note with Clipboard" entry
           (file+headline "C:/Users/hajimetch/Dropbox/Emacs/org/note.org" "Note")
           "* %?\n%U\n%c" :empty-lines 1)
          ("m" "Memo" entry
           (file+headline "C:/Users/hajimetch/Dropbox/Emacs/org/note.org" "Memo")
           "* TODO %?" :empty-lines 0)
          ("M" "Memo with Clipboard(Title)" entry
           (file+headline "C:/Users/hajimetch/Dropbox/Emacs/org/note.org" "Memo")
           "* TODO %U%?" :empty-lines 0)))

  ;; カーソル位置に Task entry を追加
  (defun my/org-capture-task ()
    "Insert an org-capture Task entry at point."
    (interactive)
    (org-capture 0 "t"))

  ;; 備忘録を追加
  (defun my/org-capture-memo ()
    "Insert an org-capture Memo entry at point."
    (interactive)
    (org-capture nil "m"))

  ;; Emacs 終了時に org-clock-out
  (defun my/org-clock-out-and-save ()
    "Save buffers and stop clocking when kill emacs."
    (when (org-clock-is-active)
      (org-clock-out)
      (save-some-buffers t))))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

(bind-key "C-c a" 'org-agenda)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c o" 'org-switchb)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-M-=" 'my/org-capture-task)
(bind-key "C-M--" 'my/org-capture-memo)


;;; howm
(use-package howm :ensure :demand
  :init
  (setq howm-view-title-header "*")
  (setq howm-prefix (kbd "C-x ,"))

  :bind
  (:map howm-mode-map
        ("C-c C-c"  . my/howm-save-buffer-and-kill)
        ("C-c C-k"  . my/howm-kill-buffer))

  :config
  (set-variable 'howm-directory "C:/Users/hajimetch/Dropbox/Emacs/howm/")
  (set-variable 'howm-keyword-file (concat howm-directory ".howm-keys"))
  (set-variable 'howm-history-file (concat howm-directory ".howm-history"))
  (set-variable 'howm-menu-file (concat howm-directory "menu.txt"))
  (set-variable 'howm-menu-lang 'ja)

  ;; メモを保存と同時に閉じる
  (defun my/howm-save-buffer-and-kill()
    "Save howm buffer and exit."
    (interactive)
    (when (and (buffer-file-name)
               (howm-buffer-p))
      (save-buffer)
      (kill-buffer nil)))

  ;; メモを保存せずに閉じる
  (defun my/howm-kill-buffer()
    "Save howm buffer and exit."
    (interactive)
    (when (and (buffer-file-name)
               (howm-buffer-p))
      (kill-buffer nil))))


;;; open-junk-file
(use-package open-junk-file :no-require :ensure
  :bind ("C-c j"    . open-junk-file)
  :config (setq open-junk-file-format "C:/Users/hajimetch/Dropbox/Emacs/junk/%Y-%m-%d-%H%M%S."))


;;;; 70_eww.el
;;; eww-mode
(with-eval-after-load 'eww
  (set-variable 'eww-search-prefix "http://www.google.co.jp/search?q=")
  (bind-key "w" 'eww-copy-page-url eww-mode-map)

  ;; 背景色の設定
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

  ;; 画像表示の設定
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

  (defun my/eww-mode-hook--disable-image ()
    "Disable showing images with eww-mode-hook."
    (setq-local shr-put-image-function 'my/shr-put-image-alt))
  (add-hook 'eww-mode-hook 'my/eww-mode-hook--disable-image))

;; 現在の url を eww で開く
(defun my/browse-url-with-eww ()
  "Browse current url with eww."
  (interactive)
  (require 'eww)
  (require 'org)
  (let ((url-region (bounds-of-thing-at-point 'url)))
    (cond
     ;; url
     (url-region
      (eww-browse-url (buffer-substring-no-properties
                       (car url-region)(cdr url-region))))
     ;; org-link
     ((or (org-in-regexp org-any-link-re)
          (org-in-regexp org-ts-regexp-both nil t)
          (org-in-regexp org-tsr-regexp-both nil t))
      (let ((browse-url-browser-function 'eww-browse-url))
        (org-open-at-point-global)))
     ;; others
     (t (eww-search-words)))))
(bind-key* "C-c C-w" 'my/browse-url-with-eww)


;;;; 80_print.el
;;; PostScript Option
(set-variable 'ps-print-color-p t)
(set-variable 'ps-paper-type 'a4)       ; paper size
(set-variable 'ps-lpr-command "pdfpreview.bat")
(set-variable 'ps-printer-name nil)
(set-variable 'ps-printer-name-option nil)
(set-variable 'ps-multibyte-buffer 'non-latin-printer) ; for printing Japanese
(set-variable 'ps-n-up-printing 1)      ; print n-page per 1 paper
;; Margin
(set-variable 'ps-left-margin 40)
(set-variable 'ps-right-margin 40)
(set-variable 'ps-top-margin 40)
(set-variable 'ps-bottom-margin 40)
(set-variable 'ps-n-up-margin 20)
;; Header/Footer setup
(set-variable 'ps-print-header t)     ; buffer name, page number, etc.
(set-variable 'ps-print-footer nil)   ; page number
;; font
(set-variable 'ps-font-size '(10 . 12))
(set-variable 'ps-header-font-size '(10 . 12))
(set-variable 'ps-header-title-font-size '(12 . 14))
(set-variable 'ps-header-font-family 'Helvetica)   ; default
(set-variable 'ps-line-number-font "Times-Italic") ; default
(set-variable 'ps-line-number-font-size 8)
;; line-number
(set-variable 'ps-line-number t)        ; t:print line number
(set-variable 'ps-line-number-start 1)
(set-variable 'ps-line-number-step 1)

(bind-key "M-p" 'ps-print-region)


;;;; 90_key.el
;;; Alt to Meta
(setq w32-alt-is-meta t)


;;; PL
(bind-key "C-c p"          'previous-error)
(bind-key "C-c n"          'next-error)


;;; Font Size
;; Adjust
(bind-key "M-<wheel-up>"   '(lambda() (interactive) (text-scale-increase 1)))
(bind-key "M-="            '(lambda() (interactive) (text-scale-increase 1)))
(bind-key "M-<wheel-down>" '(lambda() (interactive) (text-scale-decrease 1)))
(bind-key "M--"            '(lambda() (interactive) (text-scale-decrease 1)))

;; Reset
(bind-key "M-0"            '(lambda() (interactive) (text-scale-set 0)))


;;; Other key bindings
(bind-key "C-h" (kbd "DEL") key-translation-map) ; C-hでバックスペース
(bind-key "C-m"            'newline-and-indent) ; 改行時自動インデント
(bind-key "C-x k"          'kill-this-buffer)   ; バッファを閉じる
(bind-key "M-k"            'kill-this-buffer)   ; バッファを閉じる
(bind-key "C-c <C-return>" 'toggle-truncate-lines) ; 右端で折り返す
(bind-key "C-c TAB"        'indent-region)         ; 範囲インデント
(bind-key* "C-t"           'other-window) ; 他のウィンドウに移動
(bind-key "<backtab>"                     ; TAB挿入
          '(lambda() (interactive) (insert "	")))
(bind-key "<f1>"           'help-for-help) ; ヘルプ参照


;; Local Variables:
;; coding: utf-8-unix
;; outline-minor-mode: t
;; flycheck-mode: nil
;; time-stamp-pattern: "10/Modified:\\\\?[ \t]+%:y-%02m-%02d\\\\?\n"
;; End:
