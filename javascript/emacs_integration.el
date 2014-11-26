;;; gives access to emdocs-shell-command-minus-message function
(load-file "./emdocs-shell-command-minus-message.el")

;;; read/write buffers to file
(defun emdocs-send-buffer-to-file (name-of-buffer file-path)
  "Sends contents of buffer name-of-buffer to file identified by file-path."
  (with-current-buffer name-of-buffer
    (emdocs-shell-command-minus-message
     (point-min) (point-max)
     (concat "./write_stdin_to_file.sh" " " ; space args
             "\"" file-path "\"")           ; output file
     "*emdocs-out*" nil
     "*emdocs-err*" t)))

(defun emdocs-read-buffer-from-file (name-of-buffer file-path)
  "Replaces contents of name-of-buffer with file identified by file-name."
  (with-current-buffer name-of-buffer
    (let ((cur-point (point)))          ; save-excursion doesn't work with
                                        ; buffer replacement like this
      (emdocs-shell-command-minus-message
       (point-min) (point-max)
       (concat "cat " file-path)
       t t                              ; output and replace current buffer
       "*emdocs-err*" t)
      (goto-char cur-point))))
