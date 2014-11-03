(defconst tmp-diff-file-suffix ".tmp")

(defun ex-stuff (file-name)
  (start-process "cat-buffer" "*emacs-docs*" "echo"
                 (progn
                   (set-buffer file-name)
                   (substring-no-properties (buffer-string)))))
