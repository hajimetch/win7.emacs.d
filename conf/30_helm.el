;;; Helm
(require 'helm)

;; helm-mini
(setq helm-mini-default-sources
      (quote
       (helm-source-buffers-list
        helm-source-recentf
        helm-source-files-in-current-dir
        )))

;; 表示する最大候補数
(setq helm-candidate-number-limit 100)

;; Helm バッファのサイズ
(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 40)
(helm-autoresize-mode t)

;; Helm バッファが常にウィンドウの下側に来るように設定
(setq helm-default-display-buffer-functions '(display-buffer-in-side-window))

;; Helm その他の設定
(setq helm-scroll-amount 8)
(setq helm-split-window-inside-p t)
(setq helm-ff-search-library-in-sexp t)
(setq helm-ff-file-name-history-use-recentf t)

(helm-mode t)


;;; helm-c-yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("c:/Users/hajimetch/Dropbox/Emacs/snippets/mysnippets" ; 自作スニペット
        "c:/Users/hajimetch/Dropbox/Emacs/snippets/yasnippets" ; デフォルトスニペット
        ))
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)
(yas-global-mode t)


;;; helm-migemo-mode
(helm-migemo-mode t)


;;; helm-swoop
(require 'helm-swoop)

;; リストを循環しない
(setq helm-swoop-move-to-line-cycle nil)


;;; helm-ag を ripgrep で利用
(setq helm-ag-base-command "rg --vimgrep --no-heading --smart-case")


;;; Projectile
(projectile-mode t)
(setq projectile-completion-system 'helm)

;; helm-projectile
(require 'helm-projectile)
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
    (error "helm-ag not available")))


;;; helm-find-file から browse-project を呼び出す
(defun my/helm-ff-run-browse-project ()
  "Call helm-ff-run-browse-project with C-u."
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'helm-ff-run-browse-project))


;;; helm-gtags が UNC path 環境下で動作しない問題を回避
(advice-add 'select-window
            :around (lambda (orig-fun &rest args)
                      (when (nth 0 args)(apply orig-fun args))))


;;; helm-man-woman
;; 既存のソースを読み込む
(require 'helm-elisp)
(require 'helm-man)

;; 基本となるソースを定義
(setq helm-for-document-sources
      '(helm-source-info-elisp
        helm-source-info-cl
        helm-source-info-eieio
        helm-source-man-pages))

;; man, info, apropos を串刺し検索する
(defun my/helm-for-document ()
  "Preconfigured `helm' for helm-for-document."
  (interactive)
  (let ((default (thing-at-point 'symbol)))
    (helm :sources
          (nconc
           (mapcar (lambda (func)
                     (funcall func default))
                   helm-apropos-function-list)
           helm-for-document-sources)
          :buffer "*helm for document*")))
