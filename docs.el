(defconst executable-directory "/home/cosmicexplorer/projects/CollabEmacs/")

(defun send-buffer-to-file (file-name tmp-suffix)
  (start-process "send-buffer-to-file" "*emacs-docs*"
                 (concat executable-directory "echo-string-to-file.sh")
                 (concat file-name tmp-suffix) ; output file
                 (progn                 ; get text of buffer
                   (set-buffer file-name)
                   (substring-no-properties (buffer-string)))))
