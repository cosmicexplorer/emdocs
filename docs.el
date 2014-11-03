(defconst tmp-diff-file-suffix ".tmp")
(defconst executable-directory "/usr/share/")

(defun send-buffer-to-tmp (file-name)
  (start-process "cat-buffer-to-file" "*emacs-docs*"
                 (concat executable-directory "echo-string-to-file") ; command
                 (concat file-name tmp-diff-file-suffix) ; outfile
                 (progn                 ; get text of buffer
                   (set-buffer file-name)
                   (substring-no-properties (buffer-string)))))
