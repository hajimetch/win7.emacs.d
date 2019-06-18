;; py-yapf
(use-package py-yapf :ensure
  :after python
  :bind
  (:map python-mode-map
        ("C-c C-f"  . py-yapf-buffer))
  :hook (python-mode . py-yapf-enable-on-save))

;; jedi
(use-package company-jedi :ensure
  :after (python company)
  :init (use-package jedi-core)
  :hook (python-mode . jedi:setup)
  :config
  (set-variable 'jedi:complete-on-dot t)
  (set-variable 'jedi:use-shortcuts t)
  (add-to-list 'company-backends 'company-jedi)
  (unbind-key "C-c ." jedi-mode-map))


;;; flycheck
(use-package flycheck :ensure
  :init (use-package flycheck-pos-tip :ensure)
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
(use-package web-mode :ensure
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
(use-package js2-mode :ensure
  :mode "\\.js\\'")


;;; markdown-mode
(use-package markdown-mode :ensure
  :mode "\\.md'"
  :config
  (set-variable 'markdown-command "C:/Tools/Pandoc/pandoc -s --self-contained -t html5 -c C:/Tools/Pandoc/github.css --quiet")
  (skk-wrap-newline-command markdown-enter-key))


;;; php-mode
(use-package php-mode :ensure
  :config (setq php-manual-url 'ja))


;;; ini-mode
(use-package ini-mode :ensure
  :mode "\\.ini\\'")


;;; json-mode
(use-package json-mode :ensure
  :mode "\\.json\\'")


;;; yaml-mode
(use-package yaml-mode :ensure
  :mode "\\.ya?ml$")


;;; gtags
(use-package helm-gtags :ensure
  :diminish helm-gtags-mode "HGtags"
  :after helm
  :bind
  (:map helm-gtags-mode-map
        ("C-c . ."  . helm-gtags-find-tag-from-here)
        ("C-c . ,"  . helm-gtags-pop-stack)
        ("C-c . t"  . helm-gtags-find-tag)
        ("C-c . r"  . helm-gtags-find-rtag)
        ("C-c . s"  . helm-gtags-find-symbol)
        ("C-c . f"  . helm-gtags-find-files))
  :hook
  ((python-mode     . helm-gtags-mode)
   (emacs-lisp-mode . helm-gtags-mode))
  :config (set-variable 'helm-gtags-auto-update t))


;;; magit
(use-package magit :ensure
  :init (use-package ssh-agency :ensure)
  :bind ("C-x g"    . magit-status)
  :config (setenv "GIT_ASKPASS" "git-gui--askpass"))


;;; git-gutter
(use-package git-gutter :ensure
  :bind
  (("C-x p"         . git-gutter:previous-hunk)
   ("C-x n"         . git-gutter:next-hunk))
  :config (global-git-gutter-mode t))


;;; quickrun
(use-package quickrun :ensure
  :bind ("C-c q"    . quickrun))
