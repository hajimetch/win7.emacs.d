;;; Path
 ;;; Path
(setq exec-path (append exec-path '("C:/Program Files/Git/cmd")))
(setq exec-path (append exec-path '("C:/Program Files/Git/mingw64/bin")))
(setq exec-path (append exec-path '("C:/Program Files/Git/usr/bin")))


;;; Programs for Windows を指定
(setq find-program "\"C:/Program Files/Git/usr/bin/find.exe\"")
(setq grep-program "\"C:/Program FIles/Git/usr/bin/grep.exe\"")
(setq diff-command "\"C:/Program Files/Git/usr/bin/diff.exe\"")
(setq null-device "/dev/null")


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
(use-package migemo :ensure
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
(use-package anzu :ensure
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
(use-package recentf-ext :ensure)

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
(use-package undo-tree :ensure
  :diminish undo-tree-mode "UTree"
  :config (global-undo-tree-mode t))

;; point-undo
(use-package point-undo
  :bind
  ("M-["            . point-undo)
  ("M-]"            . point-redo))


;;; company
;; company
(use-package company :ensure
  :diminish company-mode "Comp"
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
(use-package company-quickhelp :ensure
  :after company
  :config (company-quickhelp-mode t))


;;; which-key
(use-package which-key :ensure
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode t))


;;; multiple-cursor
(use-package multiple-cursors :ensure
  :bind
  (("C->"           . mc/mark-next-like-this)
   ("C-<"           . mc/mark-previous-like-this)
   ("C-c e"         . mc/edit-lines)
   ("C-c h"         . mc/mark-all-like-this)))


;;; expand-region
(use-package expand-region :ensure
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

;; ファイルが #! から始まる場合、+x を付けて保存する
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Error 対応 (ad-handle-definition: command got redefined)
(set-variable 'ad-redefinition-action 'accept)
