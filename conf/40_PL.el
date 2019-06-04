;;; python
(use-package python
  :bind
  (:map python-mode-map
        ("C-c C-f"  . py-yapf-buffer)))


;;; jedi
(use-package jedi-core
  :after (python company)
  :hook (python-mode . jedi:setup)
  :custom
  (jedi:complete-on-dot t)
  (jedi:use-shortcuts t)
  :config
  (add-to-list 'company-backends 'company-jedi)
  (unbind-key "C-c ." jedi-mode-map))


;;; flycheck
(use-package flycheck
  :bind
  (:map flycheck-mode-map
        ("C-c C-d"  . flycheck-list-errors))
  :hook (after-init . global-flycheck-mode)
  :config
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-pos-tip-mode t))


;;; semantic-mode
(semantic-mode t)


;;; web-mode
(use-package web-mode
  :mode
  (("\\.html\\'"    . web-mode)
   ("\\.css\\'"     . web-mode)
   ("\\.jsx\\'"     . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.ctp\\'"     . web-mode)
   ("\\.jsp\\'"     . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'"     . web-mode)))


;;; js2-mode
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))


;;; mark-down-mode
(use-package markdown-mode
  :mode ("\\.md'"   . markdown-mode)
  :custom (markdown-command "C:/Tools/Pandoc/pandoc -s --self-contained -t html5 -c C:/Tools/Pandoc/github.css --quiet")
  :config (skk-wrap-newline-command markdown-enter-key))


;;; php-mode
(use-package php-mode
  :custom (php-manual-url 'ja))


;;; gtags
(use-package helm-gtags
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
  :custom
  (helm-gtags-auto-update t))


;;; magit
(use-package magit
  :bind ("C-x g"    . magit-status)
  :config (setenv "GIT_ASKPASS" "git-gui--askpass"))


;;; git-gutter
(use-package git-gutter
  :bind
  (("C-x p"         . git-gutter:previous-hunk)
   ("C-x n"         . git-gutter:next-hunk))
  :config (global-git-gutter-mode t))


;;; quickrun
(use-package quickrun
  :bind
  ("C-c q"          . quickrun))
