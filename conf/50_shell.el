;;; Eshell
;; eshell alias
(setq eshell-command-aliases-list
      (append
       (list
        (list "ll" "ls -lh")
        (list "la" "ls -a")
        (list "emacs" "find-file $1")
        (list "m" "find-file $1")
        (list "mc" "find-file $1")
        (list "d" "dired .")
        (list "l" "my/eshell-less $1 $2"))))

;; view files in eshell
(defun my/eshell-less (&rest args)
  "Invoke `view-file' on the file.
\"less +42 foo\" also goes to line 42 in the buffer."
  (interactive)
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (view-file file)
          (goto-line line))
      (view-file (pop args)))))

;; open eshell with current directory
(defun my/eshell-pop (&optional arg)
  "Popup eshell and cd to current directory."
  (interactive "P")
  (cond ((equal (buffer-name) "*eshell*")
         (kill-buffer-and-window))
        (t
         (let ((dir default-directory))
           (eshell arg)
           (cd dir))
         (goto-char (point-max)))))
(bind-key "C-c t" 'my/eshell-pop)
