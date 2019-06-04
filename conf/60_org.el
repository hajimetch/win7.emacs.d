;;; org-mode
(use-package org
  :bind
  (("C-c a"         . org-agenda)
   ("C-c c"         . org-capture)
   ("C-c o"         . org-switchb)
   ("C-c l"         . org-store-link)
   ("C-M-="         . my/org-capture-task)
   ("C-M--"         . my/org-capture-memo)
   :map org-mode-map
   ("C-c ("         . org-clock-in)
   ("C-c )"         . org-clock-out))

  :mode
  (("\\.org$"       . org-mode)
   ("\\.txt$"       . org-mode))

  :hook
  ((org-mode        . turn-on-font-lock)  ; org-mode で強調表示を可能に
   (kill-emacs      . my/org-clock-out-and-save))

  :custom
  (org-startup-with-inline-images t)    ; 画像をインラインで表示
  (org-startup-indented t)              ; インデントモードにする
  (org-indent-indentation-per-level 1)  ; インデントの幅を設定
  (org-directory "C:/Users/hajimetch/Dropbox/Emacs/org/") ; デフォルトディレクトリ
  (org-default-notes-file "C:/Users/hajimetch/Dropbox/Emacs/org/default.org")
                                        ; デフォルトファイル名
  (org-clock-into-drawer t)             ; LOGBOOK drawer に時間を格納
  (org-log-done 'time)                  ; DONE の時刻を記録
  (system-time-locale "C")              ; 文字化け対策
  (org-export-with-sub-superscripts nil) ; "_"の後下付き文字にしない
  ;; TODO 状態
  (org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w)" "NOTE(n)" "|" "DONE(d)" "SOMEDAY(s)" "CANCEL(c)")))
  ;; テンプレート
  (org-capture-templates
   '(("t" "Task" entry
      (file+headline "C:/Users/hajimetch/Dropbox/Emacs/org/task.org" "Task")
      "* TODO %?\n%U" :empty-lines 1)
     ("T" "Task with Clipboard" entry
      (file+headline "C:/Users/hajimetch/Dropbox/Emacs/org/task.org" "Task")
      "* TODO %?\n%U\n%c" :empty-lines 1)
     ("n" "Note" entry
      (file+headline "C:/Users/hajimetch/Dropbox/Emacs/org/note.org" "Note")"* %?\n%U" :empty-lines 1)
     ("N" "Note with Clipboard" entry
      (file+headline "C:/Users/hajimetch/Dropbox/Emacs/org/note.org" "Note")
      "* %?\n%U\n%c" :empty-lines 1)
     ("m" "Memo" entry
      (file+headline "C:/Users/hajimetch/Dropbox/Emacs/org/note.org" "Memo")
      "* TODO %?" :empty-lines 0)
     ("M" "Memo with Clipboard(Title)" entry
      (file+headline "C:/Users/hajimetch/Dropbox/Emacs/org/note.org" "Memo")
      "* TODO %U%?" :empty-lines 0)))
  ;; org-agenda
  (org-agenda-files (list org-directory))          ; 対象ファイル
  (org-agenda-start-with-log-mode '(closed clock)) ; Log mode で開始
  ;; org-clock
  (org-clock-out-remove-zero-time-clocks t) ; 1分未満を記録しない
  (org-clock-clocked-in-display 'frame-title) ; タスク名をタイトルバーに表示
  (org-archive-location "C:/Users/hajimetch/Dropbox/Emacs/org/archive.org::datetree/")
                                        ; Archive ファイルを datetree で管理

  :config
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


;;; howm
(use-package howm :ensure :demand
  :init
  (setq howm-view-title-header "*")
  (setq howm-prefix (kbd "C-x ,"))
  :bind
  (:map howm-mode-map
        ("C-c C-c"  . my/howm-save-buffer-and-kill)
        ("C-c C-k"  . my/howm-kill-buffer))
  :custom
  (howm-directory "C:/Users/hajimetch/Dropbox/Emacs/howm/") ; ファイルパス
  (howm-keyword-file (concat howm-directory ".howm-keys"))
  (howm-history-file (concat howm-directory ".howm-history"))
  (howm-menu-file (concat howm-directory "menu.txt"))
  (howm-menu-lang 'ja)                  ; home-menu の言語
  :config
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
(use-package open-junk-file :ensure
  :bind ("C-c j"    . open-junk-file)
  :custom (open-junk-file-format "C:/Users/hajimetch/Dropbox/Emacs/junk/%Y-%m-%d-%H%M%S."))
