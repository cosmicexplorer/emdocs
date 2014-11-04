(defconst tmp-diff-file-suffix ".tmp")
(defconst executable-directory "/home/cosmicexplorer/projects/CollabEmacs/")

(defun send-buffer-to-tmp (file-name)
  (start-process "cat-buffer-to-file" "*emacs-docs*"
                 (concat executable-directory "echo-string-to-file.sh")
                 (concat file-name tmp-diff-file-suffix) ; outfile
                 (progn                 ; get text of buffer
                   (set-buffer file-name)
                   (substring-no-properties (buffer-string)))))
