;; -*- lexical-binding: t; -*-
;;; see https://stackoverflow.com/questions/27166957/
;;; emacs-not-accepting-lambda-in-after-change-functions

;;; client socket connection
(load-file "./emdocs-utilities.el")
(load-file "./emdocs-network-classes.el")

(defmethod emdocs-client-start ((client emdocs-client) buffer-to-change)
  (unless (process-status (emdocs-get-process-name client))
    (setf (emdocs-get-process client)
          (make-network-process
           :name (emdocs-get-process-name client)
           :buffer (emdocs-get-log-buffer client)
           :family 'ipv4
           :host 'local
           :service +emdocs-internal-http-port+
           :sentinel #'(lambda (sock msg)
                         (emdocs-client-sentinel client sock msg))
           :filter #'(lambda (sock msg)
                       (emdocs-client-filter client sock msg))
           :server nil
           :noquery t))
    (emdocs-receive-changes-on-buffer client buffer-to-change)
    (emdocs-client-log-message client "client started")))

(defmethod emdocs-client-stop ((client emdocs-client))
  (when (process-status (emdocs-get-process-name client))
    (delete-process (emdocs-get-process client))
    (setf (emdocs-get-process client) nil)))

(defmethod emdocs-client-filter ((client emdocs-client) server-socket message)
  (when (emdocs-get-attached-buffer client)
    (emdocs-receive-keypress message (emdocs-get-attached-buffer client)))
  (emdocs-client-log-message client message server-socket))

(defmethod emdocs-client-sentinel ((client emdocs-client) server-socket message)
  (emdocs-client-log-message client message server-socket))

(defmethod emdocs-client-log-message ((client emdocs-client)
                                      string &optional server-socket)
  (with-current-buffer (get-buffer (emdocs-get-log-buffer client))
    (goto-char (point-max))
    (insert
     (current-time-string)
     (if server-socket (format ": %s: " server-socket) ": ")
     string)
    (newline)))

(defmethod emdocs-client-send-message ((client emdocs-client) message)
  (process-send-string (emdocs-get-process client) message))

(defmethod emdocs-receive-changes-on-buffer ((client emdocs-client)
                                             name-of-buffer)
  (setf (emdocs-get-attached-buffer client) name-of-buffer))

(defun emdocs-receive-keypress (message name-of-buffer)
  (let ((json-object-type 'plist))
    (emdocs-edit-from-json (json-read-from-string message) name-of-buffer)))

(require 'json)
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
        (delete-char content))
       (t
        (throw 'unrecognized-op t))))))
