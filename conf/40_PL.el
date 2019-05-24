;;; python
(require 'python)


;;; jedi
(require 'jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-to-list 'company-backends 'company-jedi)


;;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(flycheck-add-next-checker 'python-flake8 'python-pylint)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; flycheck-pos-tip
(flycheck-pos-tip-mode t)


;;; semantic-mode
(semantic-mode t)


;;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))


;;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


;;; mark-down-mode
(add-to-list 'auto-mode-alist '("\\.md'" . mark-down-mode))
(setq markdown-command "C:/Tools/Pandoc/pandoc -s --self-contained -t html5 -c C:/Tools/Pandoc/github.css --quiet")


;;; php-mode
(setq php-manual-url 'ja)


;;; gtags
(setq helm-gtags-auto-update t)
(add-hook 'python-mode-hook 'helm-gtags-mode)
(add-hook 'emacs-lisp-mode-hook 'helm-gtags-mode)


;;; git-gutter
(global-git-gutter-mode t)
