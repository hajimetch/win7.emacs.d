;;; package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)


;;; server start for emacsclient
(require 'server)
(setq server-auth-dir "~/.emacs.server")

(unless (eq (server-running-p) 't)
  (server-start)

  (defun my/iconify-emacs-when-server-is-done ()
    (unless server-clients (iconify-frame)))
  (add-hook 'after-init-hook 'my/iconify-emacs-when-server-is-done) ; minimize when start

  (global-set-key (kbd "C-x C-c") 'server-edit) ; do not exit when C-x C-c
  )

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


;;; custom-file
(setq custom-file (locate-user-emacs-file "custom.el"))

;; if not exists, create custom-file
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; load custom-file
(load custom-file)


;;; init-loader
(require 'init-loader)

;; display errors processing init files
(defun init-loader-re-load (re dir &optional sort)
  (let ((load-path (cons dir load-path)))
    (dolist (el (init-loader--re-load-files re dir sort))
      (condition-case e
          (let ((time (car (benchmark-run (load (file-name-sans-extension el))))))
            (init-loader-log (format "loaded %s. %s" (locate-library el) time)))
        (error
         (init-loader-error-log
          (format "%s. %s" (locate-library el)
                  (error-message-string e))))))))

;; load ~/.emacs.d/conf/*
(init-loader-load "~/.emacs.d/conf")
