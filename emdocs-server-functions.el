;; -*- lexical-binding: t; -*-

;;; see https://stackoverflow.com/questions/27166957/
;;; emacs-not-accepting-lambda-in-after-change-functions

;;; server socket connection
(load-file "./emdocs-utilities.el")
(load-file "./emdocs-network-classes.el")
(defconst +emdocs-server-process-name+ "emdocs-server")
(defconst +emdocs-server-buffer-name+ "*emdocs-server*")

(defmethod emdocs-server-start ((server emdocs-server) buffer-to-broadcast)
  (unless (process-status (emdocs-get-process-name server))
    (setf (emdocs-get-process server)
          (make-network-process
           :name (emdocs-get-process-name server)
           :buffer (emdocs-get-log-buffer server)
           :family 'ipv4
           ;; :host (emdocs-get-ip-address)
           :host 'local
           :service +emdocs-internal-http-port+
           :sentinel #'(lambda (sock msg)
                         (emdocs-server-sentinel server sock msg))
           :filter #'(lambda (sock msg)
                       (emdocs-server-filter server sock msg))
           :sentinel #'emdocs-server-sentinel-DEBUG
           :filter #'emdocs-server-filter-DEBUG
           :server t
           :noquery t))
    (emdocs-server-log-message server "server started")
    (emdocs-set-after-change-functions server buffer-to-broadcast))
  (emdocs-get-process-name server))

(defmethod emdocs-server-stop ((server emdocs-server))
  (when (process-status (emdocs-get-process-name server))
    (maphash '(lambda (client-socket cur-message)
                (delete-process client-socket))
             (emdocs-get-hash-table server))
    (clrhash (emdocs-get-hash-table server))
    (delete-process (emdocs-get-process server))
    (setf (emdocs-get-process server) nil)))

(defmethod emdocs-server-filter ((server emdocs-server) client-socket message)
  (emdocs-server-log-message server message client-socket))

(defmethod emdocs-server-sentinel ((server emdocs-server) client-socket message)
  (cond ((string-match "connection broken" message)
         (remhash client-socket (emdocs-get-hash-table server)))
        ((string-match "open from" message)
         (puthash client-socket message (emdocs-get-hash-table server))))
  (emdocs-server-log-message server message client-socket))

(defmethod emdocs-server-log-message ((server emdocs-server)
                                      string &optional client-socket)
  (with-current-buffer (get-buffer (emdocs-get-log-buffer server))
    (goto-char (point-max))
    (insert
     (current-time-string)
     (if client-socket (format ": %s: " client-socket) ": ")
     string)
    (newline)))

(defmethod emdocs-server-broadcast-message ((server emdocs-server) message)
  (maphash #'(lambda (client-socket cur-message)
               (process-send-string client-socket message))
           (emdocs-get-hash-table server))
  (emdocs-server-log-message server (concat "broadcasted: " message)))

(defmethod emdocs-list-clients ((server emdocs-server))
  (maphash #'(lambda (client-socket cur-message)
              (emdocs-server-log-message "SOCKET LOGGED" client-socket))
           (emdocs-get-hash-table server)))

(defmethod emdocs-notify-others-of-change ((server emdocs-server)
                                           beg end prev-length)
  "Retrieves keypress as one of the after-change-functions, parses content sent
by after-change-functions, and dispatches the appropriate request to the node
server."
  (cond ((= prev-length 0)              ; if insertion
         (emdocs-emit-keypress-json
          server
          emdocs-insert-edit beg (buffer-substring beg end)))
        ((= beg end)                    ; if deletion
         (emdocs-emit-keypress-json
          server
          emdocs-delete-edit beg prev-length))
        (t
         (throw 'unused-branch t))))

(require 'json)
(defmethod emdocs-emit-keypress-json ((server emdocs-server) type point content)
  "Sends a keypress to the server also running so that it can be emitted to
other users on the network."
  (emdocs-server-broadcast-message
   server
   (concat (json-encode `(,:type ,type ,:point ,point ,:content ,content))
           "\n")))

(defmethod emdocs-set-after-change-functions ((server emdocs-server)
                                              name-of-buffer)
  "Adds appropriate after-change-functions to the given name-of-buffer."
  (setf (emdocs-get-attached-buffer server) name-of-buffer)
  (with-current-buffer name-of-buffer
    (setq-local after-change-functions
                (cons
                 #'(lambda (beg end prev-length)
                     (emdocs-notify-others-of-change
                      server beg end prev-length))
                 after-change-functions))))
