;;; gives access to emdocs-shell-command-minus-message function
(load-file "./emdocs-shell-command-minus-message.el")

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

(require 'json)
(defconst emdocs-emit-filename ".emission")
(defconst emdocs-insert-edit "insert")
(defconst emdocs-delete-edit "delete")
(defconst emdocs-indel-edit "indel")
(defun emdocs-emit-keypress (type point content)
  "Sends a keypress to the node server also running so that it can be emitted to
other users on the network."
  ;; currently just sends info to file where node will monitor with inotify
  (with-temp-file emdocs-emit-filename
    (insert (json-encode '(:type type :point point :content content)))))

(defun emdocs-notify-others-of-change (beg end prev-length)
  "Retrieves keypress as one of the after-change-functions, parses content sent
by after-change-functions, and dispatches the appropriate request to the node
server."
  (cond ((= prev-length 0)              ; if insertion
         (emdocs-emit-keypress
          emdocs-insert-edit beg (buffer-substring beg end)))
        ((= beg end)                    ; if deletion
         (emdocs-emit-keypress
          emdocs-delete-edit beg prev-length))
        (t                              ; insertion and deletion, as in a region
         (emdocs-emit-keypress
          emdocs-indel-edit beg
          (cons prev-length (buffer-substring beg end))))))

(defun emdocs-set-after-change-functions (name-of-buffer)
  "Adds appropriate after-change-functions to the given name-of-buffer."
  (with-current-buffer name-of-buffer
    (setq-local after-change-functions
                (cons
                 #'emdocs-notify-others-of-change
                 after-change-functions))))
