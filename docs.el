(defconst executable-directory "/home/cosmicexplorer/projects/CollabEmacs/")

(defun send-buffer-to-file (file-name suffix)
  (with-current-buffer file-name
    (when (= (point-max) 1)
      (newline))
    (shell-command-on-region
     (point-min) (point-max)
     (concat executable-directory "write_stdin_to_file.sh" " " ; space args
             "\"" file-name suffix "\"")                       ; output file
     "*emacs-docs-out*" nil
     "*emacs-docs-err*" t)))

(defun read-buffer-from-file (file-name)
  (with-current-buffer file-name
    (let ((cur-point (point)))          ; save-excursion doesn't work with
                                        ; buffer replacement like this
      (shell-command-on-region
       (point-min) (point-max)
       (concat "cat " file-name)
       t t                              ; output and replace current buffer
       "*emacs-docs-err*" t)
      (goto-char cur-point))))
