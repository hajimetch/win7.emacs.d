;;; Helm
(use-package helm :ensure
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
(use-package helm-elscreen :ensure
  :after (helm elscreen)
  :bind ("C-x C-l"  . helm-elscreen))


;;; helm-ag(ripgrep)
(use-package helm-ag :ensure
  :after helm
  :bind ("C-c g"    . helm-do-ag)
  :config (set-variable 'helm-ag-base-command "rg --vimgrep --no-heading --smart-case"))


;;; helm-swoop
(use-package helm-swoop :ensure
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
(use-package helm-descbinds :ensure
  :after helm
  :bind ("C-c k"    . helm-descbinds))


;;; Yasnippet
(use-package yasnippet
  :config
  (set-variable 'yas-snippet-dirs
                '("C:/Users/hajimetch/Dropbox/Emacs/snippets/mysnippets" ; 自作スニペット
                  "C:/Users/hajimetch/Mac/Dropbox/Emacs/snippets/yasnippets" ; デフォルトスニペット
                  )))

(use-package helm-c-yasnippet :ensure
  :after (helm yasnippet)
  :bind ("C-c y"    . helm-yas-complete)
  :config
  (set-variable 'helm-yas-space-match-any-greedy t)
  (add-to-list 'auto-mode-alist '("emacs.+/snippets/" . snippet-mode))
  (yas-global-mode t))


;;; Projectile
(use-package projectile :ensure
  :diminish projectile-mode "Prj"
  :config
  (set-variable 'projectile-completion-system 'helm)
  (projectile-mode t))

(use-package helm-projectile :ensure
  :after (helm projectile)
  :bind ("C-x C-p"  . helm-projectile)
  :bind-keymap* ("C-c C-p" . projectile-command-map)
  :config (helm-projectile-on)

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
