;;; client socket connection
(load-file "./emdocs-server-functions.el")
(defconst +emdocs-client-process-name+ "emdocs-client")
(defconst +emdocs-client-buffer-name+ "*emdocs-client*")
(defvar *emdocs-client-socket-process* nil)
(defvar *emdocs-client-modifying-buffer* nil)

(defun emdocs-client-start ()
  (unless (process-status emdocs-client-process-name)
    (setq *emdocs-client-socket-process*
          (make-network-process
           :name +emdocs-client-process-name+
           :buffer +emdocs-client-buffer-name+
           :family 'ipv4
           :host 'local
           :service +emdocs-internal-http-port+
           :sentinel #'emdocs-client-sentinel
           :filter #'emdocs-client-filter
           :server nil                  ; make a client
           :noquery t))
    (emdocs-client-log-message "client started")))

(defun emdocs-client-stop ()
  (delete-process +emdocs-client-process-name+)
  (setq *emdocs-client-socket-process* nil))

(defun emdocs-client-filter (server-socket message)
  (when *emdocs-client-modifying-buffer*
    (emdocs-receive-keypress message *emdocs-client-modifying-buffer*))
  (emdocs-client-log-message message server-socket))

(defun emdocs-client-sentinel (server-socket message)
  (emdocs-client-log-message message server-socket))

(defun emdocs-client-log-message (string &optional server-socket)
  (with-current-buffer (get-buffer-create +emdocs-client-buffer-name+)
    (goto-char (point-max))
    (insert
     (current-time-string)
     (if server-socket (format ": %s: " server-socket) ": ")
     string)
    (newline)))

(defun emdocs-client-send-message (message)
  (process-send-string *emdocs-client-socket-process* message))

(emdocs-client-start)
(emdocs-client-send-message "hello world!\n")
(emdocs-client-stop)
