;;; server socket connection
(defconst +emdocs-internal-http-port+ 8081)
(defvar *emdocs-client-table* (make-hash-table
                                 :test 'eq
                                 :weakness 'key-and-value))
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
           :host 'local
           :service +emdocs-internal-http-port+
           :sentinel #'emdocs-server-sentinel
           :filter #'emdocs-server-filter
           :server t
           :noquery t))
    (emdocs-server-log-message "server started")))

(defun emdocs-server-stop ()
  (maphash '(lambda (client-socket cur-message)
             (delete-process client-socket))
           *emdocs-client-table*)
  (clrhash *emdocs-client-table*)
  (delete-process +emdocs-server-process-name+)
  (setq *emdocs-server-process* nil))

(defun emdocs-server-filter (client-socket message)
  (process-send-string client-socket message)
  (emdocs-server-log-message message client-socket))

(defun emdocs-server-sentinel (client-socket message)
  (cond ((string-match "connection broken" message)
         (remhash client-socket *emdocs-client-table*))
        ((string-match "open from" message)
         (puthash client-socket message *emdocs-client-table*)))
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
           *emdocs-client-table*))

(defun emdocs-list-clients ()
  (maphash '(lambda (client-socket cur-message)
              (emdocs-server-log-message "SOCKET LOGGED" client-socket))
           *emdocs-client-table*))

(emdocs-server-start)
(emdocs-list-clients)
(emdocs-server-broadcast-message "hello world!\n")
(emdocs-server-stop)
