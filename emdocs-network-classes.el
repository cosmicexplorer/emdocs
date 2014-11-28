;; -*- lexical-binding: t; -*-
;;; see https://stackoverflow.com/questions/27166957/
;;; emacs-not-accepting-lambda-in-after-change-functions

;;; inheritance tree:
;;;
;;;                  (emdocs-attached-buffer-base) (see comments below)
;;;                                         \     \
;;;                                          \     emdocs-server
;;;                           emdocs-server-base <
;;;                         /                   \  emdocs-self-server
;;; emdocs-connection-base <                     \
;;;                          emdocs-client-base - emdocs-client
;;;

(load-file "./emdocs-utilities.el")

(defclass emdocs-connection-base ()
  ((process
    :initform nil
    :accessor emdocs-get-process)
   (process-name
    :initarg :process-name
    :initform nil
    :accessor emdocs-get-process-name)
   (log-buffer
    :initarg :log-buffer
    :initform nil
    :accessor emdocs-get-log-buffer)
   (port
    :initarg :port
    :accessor emdocs-get-port)
   (self-socket
    :accessor emdocs-get-self-socket
    :allocation :class)))

(defclass emdocs-server-base (emdocs-connection-base)
  ((client-hash-table
    :initform nil
    :accessor emdocs-get-hash-table)
   (host
    :initform nil
    :initarg :host
    :accessor emdocs-get-host)))

(defclass emdocs-client-base (emdocs-connection-base)
  ((address-connecting-to
    :initform nil
    :initarg :address-connecting-to
    :accessor emdocs-get-address-connecting-to)))

(defclass emdocs-self-server (emdocs-server-base)
  ((host
    :initform 'local))
  (:documentation "Typically, one does not use the inherited initarg, and uses
  the default of 'local."))

;;; TODO: change attached-buffer to multiply inherit from a base class;
;;; would normally inherit from attached-buffer-base, but can't figure out
;;; multiple inheritance with EIEIO right now
(defclass emdocs-server (emdocs-server-base)
  ((after-change-function
    :initform nil
    :accessor emdocs-get-after-change-function)
   (attached-buffer
    :initarg :attached-buffer
    :initform nil
    :accessor emdocs-get-attached-buffer)))

(defclass emdocs-client (emdocs-client-base)
  ((attached-buffer
     :initarg :attached-buffer
     :initform nil
     :accessor emdocs-get-attached-buffer)))

;;; base functions
(defgeneric emdocs-filter (socket-obj socket message))
(defgeneric emdocs-sentinel (socket-obj socket message))
(defgeneric emdocs-log-message (socket-obj socket message))

(defmethod emdocs-filter ((socket-obj emdocs-connection-base)
                          socket
                          message)
  (emdocs-log-message socket-obj message socket))

(defmethod emdocs-sentinel ((socket-obj emdocs-connection-base)
                            socket
                            message)
  (emdocs-log-message socket-obj message socket))

(defmethod emdocs-log-message ((conn emdocs-connection-base)
                               string
                               &optional socket)
  (with-current-buffer (get-buffer (emdocs-get-log-buffer conn))
    (goto-char (point-max))
    (insert
     (current-time-string)
     (if socket (format ": %s: " socket) ": ")
     string)
    (unless (bolp) (newline))))

;;; WARNING: these deal with class variables!
(defmethod emdocs-start ((socket-obj emdocs-connection-base) self-socket)
  (unless self-socket
    (setf (emdocs-get-self-socket socket-obj) self-socket)))
(defmethod emdocs-stop ((socket-obj emdocs-connection-base)
                        &optional cleanup-socket)
  (when cleanup-socket
    (setf (emdocs-get-self-socket socket-obj) nil)))

;;; server functions
(defmethod emdocs-start :after ((server emdocs-server-base) self-socket)
  (unless (process-status (emdocs-get-process-name server))
    (setf (emdocs-get-hash-table server)
          (make-hash-table :test 'eq :weakness nil))
    (setf (emdocs-get-process server)
          (make-network-process
           :name (emdocs-get-process-name server)
           :buffer (emdocs-get-log-buffer server)
           :family 'ipv4
           :host (emdocs-get-host server)
           :service (emdocs-get-port server)
           :sentinel #'(lambda (sock msg)
                         (emdocs-sentinel server sock msg))
           :filter #'(lambda (sock msg)
                       (emdocs-filter server sock msg))
           :sentinel #'emdocs-server-sentinel-DEBUG
           :filter #'emdocs-server-filter-DEBUG
           :server t
           :noquery t))
    (emdocs-log-message server "server started"))
  server)

(defmethod emdocs-stop :before ((server emdocs-server-base)
                               &optional cleanup-socket)
  (when (emdocs-get-hash-table server)
      (maphash #'(lambda (client-socket cur-message)
                  (delete-process client-socket))
               (emdocs-get-hash-table server))
      (clrhash (emdocs-get-hash-table server))
      (setf (emdocs-get-hash-table server) nil))
  (when (process-status (emdocs-get-process-name server))
    (delete-process (emdocs-get-process server))
    (setf (emdocs-get-process server) nil)
    ;; TODO: add better way to kill things
    ;; (kill-buffer (emdocs-get-log-buffer server))
    ))

(defmethod emdocs-sentinel :after ((server emdocs-server-base)
                                   client-socket
                                   message)
  (cond ((string-match +emdocs-conn-broken-msg-regex+ message)
         (remhash client-socket (emdocs-get-hash-table server)))
        ((string-match +emdocs-conn-added-msg-regex+ message)
         (puthash client-socket message (emdocs-get-hash-table server)))))

(defmethod emdocs-broadcast-message ((server emdocs-server-base) message)
  (maphash #'(lambda (client-socket cur-message)
               (process-send-string client-socket message))
           (emdocs-get-hash-table server))
  (emdocs-log-message server (concat "broadcasted: " message)))

(defmethod emdocs-list-clients ((server emdocs-server-base))
  (maphash #'(lambda (client-socket cur-message)
               (emdocs-log-message "SOCKET LOGGED" client-socket))
           (emdocs-get-hash-table server)))

;;; client functions
(defmethod emdocs-start :after ((client emdocs-client-base)
                                self-socket)
  (unless (process-status (emdocs-get-process-name client))
    (setf (emdocs-get-process client)
          (make-network-process
           :name (emdocs-get-process-name client)
           :buffer (emdocs-get-log-buffer client)
           :family 'ipv4
           :host (emdocs-get-address-connecting-to client)
           :service (emdocs-get-port client)
           :sentinel #'(lambda (sock msg)
                         (emdocs-sentinel client sock msg))
           :filter #'(lambda (sock msg)
                       (emdocs-filter client sock msg))
           :server nil
           :noquery t))
    (emdocs-log-message client "client started"))
  client)

(defmethod emdocs-stop :before ((client emdocs-client-base)
                               &optional cleanup-socket)
  (when (process-status (emdocs-get-process-name client))
    (delete-process (emdocs-get-process client))
    (setf (emdocs-get-process client) nil)
    (setf (emdocs-get-attached-buffer client) nil)
    ;; TODO: add better way to kill things
    ;; (kill-buffer (emdocs-get-log-buffer client))
    ))

(load-file "./emdocs-server-functions.el")
(load-file "./emdocs-client-functions.el")
