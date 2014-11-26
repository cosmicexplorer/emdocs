;;; server socket connection
(load-file "./emdocs-utilities.el")
(defconst +emdocs-internal-http-port+ 8081)
(defvar *emdocs-server-conn-table* nil)
(defvar *emdocs-server-process* nil)
(defconst +emdocs-server-process-name+ "emdocs-server")
(defconst +emdocs-server-buffer-name+ "*emdocs-server*")

(defun emdocs-server-start ()
  (unless (process-status +emdocs-server-process-name+)
    (setq *emdocs-server-process*
          (make-network-process
           :name +emdocs-server-process-name+
           :buffer +emdocs-server-buffer-name+
           :family 'ipv4
           :host (emdocs-get-ip-address)
           :service +emdocs-internal-http-port+
           :sentinel #'emdocs-server-sentinel
           :filter #'emdocs-server-filter
           :server t
           :noquery t))
    (setq *emdocs-server-conn-table* (make-hash-table :test 'eq :weakness nil))
    (emdocs-server-log-message "server started")))

(defun emdocs-server-stop ()
  (maphash '(lambda (client-socket cur-message) (delete-process client-socket))
           *emdocs-server-conn-table*)
  (clrhash *emdocs-server-conn-table*)
  (delete-process *emdocs-server-process*)
  (setq *emdocs-server-process* nil))

(defun emdocs-server-filter (client-socket message)
  (process-send-string client-socket message)
  (emdocs-server-log-message message client-socket))

(defun emdocs-server-sentinel (client-socket message)
  (cond ((string-match "connection broken" message)
         (remhash client-socket *emdocs-server-conn-table*))
        ((string-match "open from" message)
         (puthash client-socket message *emdocs-server-conn-table*)))
  (emdocs-server-log-message message client-socket))

(defun emdocs-server-log-message (string &optional client-socket)
  (with-current-buffer (get-buffer-create +emdocs-server-buffer-name+)
    (goto-char (point-max))
    (insert
     (current-time-string)
     (if client-socket (format ": %s: " client-socket) ": ")
     string)
    (newline)))

(defun emdocs-server-broadcast-message (message)
  (maphash '(lambda (client-socket cur-message)
              (process-send-string client-socket message))
           *emdocs-server-conn-table*))

(defun emdocs-list-clients ()
  (maphash '(lambda (client-socket cur-message)
              (emdocs-server-log-message "SOCKET LOGGED" client-socket))
           *emdocs-server-conn-table*))

(defun emdocs-notify-others-of-change (beg end prev-length)
  "Retrieves keypress as one of the after-change-functions, parses content sent
by after-change-functions, and dispatches the appropriate request to the node
server."
  (cond ((= prev-length 0)              ; if insertion
         (emdocs-emit-keypress-json
          emdocs-insert-edit beg (buffer-substring beg end)))
        ((= beg end)                    ; if deletion
         (emdocs-emit-keypress-json
          emdocs-delete-edit beg prev-length))
        (t                              ; insertion and deletion, as in a region
         (throw 'unused-branch t))))

(require 'json)
(defun emdocs-emit-keypress-json (type point content)
  "Sends a keypress to the server also running so that it can be emitted to
other users on the network."
  (emdocs-server-broadcast-message
   (concat (json-encode `(,:type ,type ,:point ,point ,:content ,content))
           "\n")))

(defun emdocs-set-after-change-functions (name-of-buffer)
  "Adds appropriate after-change-functions to the given name-of-buffer."
  (with-current-buffer name-of-buffer
    (setq-local after-change-functions
                (cons
                 #'emdocs-notify-others-of-change
                 after-change-functions))))

;; (emdocs-server-start)
;; (emdocs-list-clients)
;; (hash-table-weakness *emdocs-server-conn-table*)
;; (emdocs-server-broadcast-message "hello world!\n")
;; (emdocs-server-stop)
