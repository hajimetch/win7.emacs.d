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
(use-package open-junk-file :ensure
  :bind ("C-c j"    . open-junk-file)
  :config (setq open-junk-file-format "C:/Users/hajimetch/Dropbox/Emacs/junk/%Y-%m-%d-%H%M%S."))
