;;; path
(require 'cl-lib)
(setq exec-path (append exec-path '("C:/Program Files/Git/cmd")))
(setq exec-path (append exec-path '("C:/Program Files/Git/mingw64/bin")))
(setq exec-path (append exec-path '("C:/Program Files/Git/usr/bin")))


;;; desktop-save-mode
(desktop-save-mode t)

;; load theme after restoring desktop
(add-to-list 'desktop-globals-to-save 'custom-enabled-themes)
(defun my/desktop-load-theme ()
  "Load custom theme."
  (interactive)
  (dolist (th custom-enabled-themes) (load-theme th)))
(add-hook 'desktop-after-read-hook 'my/desktop-load-theme)


;;; 文字コード
(set-language-environment "Japanese")

;; デフォルトの文字コード
(set-default-coding-systems 'utf-8-unix)

;; テキストファイル／新規バッファの文字コード
(prefer-coding-system 'utf-8-unix)

;; ファイル名の文字コード
(set-file-name-coding-system 'cp932)

;; キーボード入力の文字コード
(set-keyboard-coding-system 'cp932)

;; ターミナルの文字コード
(set-terminal-coding-system 'cp932)

;; システムメッセージの文字コード
(setq locale-coding-system 'utf-8-unix)

;; サブプロセスのデフォルト文字コード
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; サブプロセスに渡すパラメータの文字コードを cp932 にする
(cl-loop for (func args-pos) in '((call-process        4)
                                  (call-process-region 6)
                                  (start-process       3))
         do (eval `(advice-add ',func
                               :around (lambda (orig-fun &rest args)
                                         (setf (nthcdr ,args-pos args)
                                               (mapcar (lambda (arg)
                                                         (if (multibyte-string-p arg)
                                                             (encode-coding-string arg 'cp932)
                                                           arg))
                                                       (nthcdr ,args-pos args)))
                                         (apply orig-fun args))
                               '((depth . 99)))))

;; 環境依存文字 文字化け対応
(set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                      'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
(set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)


;;; 検索
;; 大文字・小文字を区別しないでサーチ
(setq-default case-fold-search nil)

;; バッファー名の検索
(setq read-buffer-completion-ignore-case t)

;; ファイル名の検索
(setq read-file-name-completion-ignore-case t)

;; インクリメント検索時に縦スクロールを有効化
(setq isearch-allow-scroll nil)

;; migemo
(require 'migemo)

(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "C:/tools/cmigemo/dict/utf-8/migemo-dict")
(setq migemo-coding-system 'utf-8-unix)
(defvar migemo-user-dictionary nil)
(defvar migemo-regex-dictionary nil)

(load-library "migemo")
(migemo-init)

;; anzu
(global-anzu-mode t)


;;; バックアップ(xxx~)
(setq make-backup-files     t)    ; 自動バックアップの実行有無
(setq version-control       t)    ; バックアップファイルへの番号付与
(setq kept-new-versions   100)    ; 最新バックアップファイルの保持数
(setq kept-old-versions     1)    ; 最古バックアップファイルの保持数
(setq delete-old-versions   t)    ; バックアップファイル削除の実行有無

;; 保存時に毎回バックアップ
(defun my/setq-buffer-backed-up-nil (&rest _)
  "Function used to always backup buffer when saved."
  (interactive) (setq buffer-backed-up nil))
(advice-add 'save-buffer :before 'my/setq-buffer-backed-up-nil)

;; バックアップ(xxx~)の格納ディレクトリ
(setq backup-directory-alist '((".*" . "c:/Users/hajimetch/Dropbox/Emacs/backups/win7")))


;;; 自動保存ファイル(#xxx#)
;; 作成する
(setq auto-save-default     t)

;; 保存の間隔
(setq auto-save-timeout    10)           ; 秒
(setq auto-save-interval  100)           ; 打鍵

;; 自動保存ファイル(#xxx#)の格納ディレクトリ
(setq auto-save-file-name-transforms
      `((".*", (expand-file-name "c:/Users/hajimetch/Dropbox/Emacs/backups/win7/") t)))


;;; 自動保存のリスト(~/.emacs.d/auto-save-list/.saves-xxx)
;; 作成する
(setq auto-save-list-file-prefix "c:/Users/hajimetch/Dropbox/Emacs/backups/win7/saves-")


;;; ロックファイル(.#xxx)
;; 作成しない
(setq create-lockfiles    nil)


;;; バックアップを作成しないファイルの設定
(defvar my/backup-inhibit-file-name-regexp "recentf"
  "Regexp of file name not for backup.")
(defun my/backup-enable-predicate (filename)
  "Function used to inhibit from backing up files specified by var my/backup-inhibit-file-name-regexp."
  (save-match-data
    (and (not (string-match my/backup-inhibit-file-name-regexp filename))
     (normal-backup-enable-predicate filename))))
(setq backup-enable-predicate 'my/backup-enable-predicate)


;;; recentf 関連
(require 'recentf)
(require 'recentf-ext)

;; 除外するファイル
(setq recentf-exclude '("recentf"))
(add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))

;; recentf に保存するファイルの数
(setq recentf-max-saved-items 1000)

;; *Messages* に不要な出力を行わないための設定
(defmacro my/with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

;; 30秒ごとに recentf を保存
(run-with-idle-timer 30 t '(lambda ()
   (my/with-suppressed-message (recentf-save-list))))


;;; undo 関連
;; undohist
(require 'undohist)
(undohist-initialize)
(setq undohist-ignored-files '("COMMIT_EDITMSG"))

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
(require 'undo-tree)
(global-undo-tree-mode t)


;;; abbrev file
(setq abbrev-file-name "c:/Users/hajimetch/Dropbox/Emacs/abbrev_defs")
(setq save-abbrevs t)
(quietly-read-abbrev-file)
(setq save-abbrevs 'silently)


;;; ediff
(require 'ediff)

;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; diff のバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)


;;; Company
(require 'company)
(require 'company-quickhelp)
(global-company-mode t)
(company-quickhelp-mode t)


;;; which-key
(which-key-setup-side-window-bottom)
(which-key-mode t)


;;; howm
(setq howm-view-title-header "*")
(setq howm-prefix (kbd "C-x ,"))
(require 'howm)

;; ファイルパス
(setq howm-directory "C:/Users/hajimetch/Dropbox/Emacs/howm")
(setq howm-keyword-file (concat howm-directory ".howm-keys"))
(setq howm-history-file (concat howm-directory ".howm-history"))
(setq howm-menu-file (concat howm-directory "0000-00-00-000000.txt"))

;; howm-menu の言語を日本語に
(setq howm-menu-lang 'ja)

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
    (kill-buffer nil)))


;;; Programs for Windows を指定
(setq find-program "\"C:\\Program Files\\Git\\usr\\bin\\find.exe\"")
(setq grep-program "\"C:\\Program Files\\Git\\usr\\bin\\grep.exe\"")
(setq diff-command "\"C:\\Program Files\\Git\\usr\\bin\\diff.exe\"")
(setq null-device "/dev/null")


;;; その他
;; dired バッファを並べる
(require 'dired)
(setq dired-dwim-target t)

;; ad-handle-definition 対応
(setq ad-redefinition-action 'accept)

;; Git SSH Passphrase を入力するプログラムを指定
(setenv "GIT_ASKPASS" "git-gui--askpass")

;; ファイルが #! から始まる場合、+x を付けて保存する
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
