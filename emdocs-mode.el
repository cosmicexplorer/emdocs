;;; keypress notifications
;;; server functions
(load-file "./emdocs-server-functions.el")
(defconst emdocs-insert-edit "insert")
(defconst emdocs-delete-edit "delete")
(defconst emdocs-indel-edit "indel")
(defun emdocs-set-after-change-functions (name-of-buffer)
  "Adds appropriate after-change-functions to the given name-of-buffer."
  (with-current-buffer name-of-buffer
    (setq-local after-change-functions
                (cons
                 #'emdocs-notify-others-of-change
                 after-change-functions))))

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
         (throw 'unused-branch t))))

(require 'json)
(defun emdocs-emit-keypress (type point content)
  "Sends a keypress to the node server also running so that it can be emitted to
other users on the network."
  ;; currently just sends info to file where node will monitor with inotify
  (emdocs-server-broadcast-message
   (concat (json-encode `(,:type ,type ,:point ,point ,:content ,content))
           "\n")))

;;; client functions
;;; TODO: figure out why connection to server keeps dropping for no reason
(load-file "./emdocs-client-functions.el")
(defun emdocs-receive-changes-on-buffer (name-of-buffer)
  (setq *emdocs-client-modifying-buffer* name-of-buffer))

(defun emdocs-receive-keypress (message name-of-buffer)
  (let ((json-object-type 'plist))
    (emdocs-edit-from-json (json-read-from-string message) name-of-buffer)))

(defun emdocs-edit-from-json (json-message name-of-buffer)
  (let ((type (plist-get json-message :type))
        (point (plist-get json-message :point))
        (content (plist-get json-message :content)))
    (with-current-buffer name-of-buffer
      (cond
       ((string-equal type emdocs-insert-edit)
        (goto-char point)
        (insert content))
       ((string-equal type emdocs-delete-edit)
        (goto-char point)
        (delete-forward-char content))
       (t
        (throw 'unrecognized-op t))))))

(provide 'emdocs)
