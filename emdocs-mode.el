;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl))

;;; globals

(defconst +emdocs-http-port+ 8080
  "docstring")

;;; classes

;;; functions

(defun emdocs-get-internal-ip-address (&optional device-name)
  "Returns ip address of active interface, ignoring loopback. Returns nil if
none active."
  (if device-name
      (format-network-address (car (network-interface-info device-name)) t)
    (let ((network-interfaces (network-interface-list)))
      ;; relies on loopback always being listed last in list
      (if (string-equal (caar network-interfaces) "lo")
          nil
        (format-network-address (cdar (network-interface-list)) t)))))

(defun emdocs-server-sentinel (buffer sock msg)
  "docstring")

(defun emdocs-server-filter (buffer sock msg)
  "docstring")

(defun emdocs-setup-server (buffer my-ip)
  "docstring"
  (make-network-process
   :name (concat "*emdocs-server:" (buffer-name buffer) "*")
   ;; TODO: remove the log buffer
   :buffer (concat "*emdocs-server-log:" (buffer-name buffer) "*")
   :family 'ipv4
   :host my-ip
   :service +emdocs-http-port+
   :sentinel (lambda (sock msg)
               (emdocs-server-sentinel buffer sock msg))
   :filter (lambda (sock msg)
             (emdocs-server-filter buffer sock msg))
   :server t
   :noquery t))

(defun emdocs-client-sentinel (buffer sock msg)
  "docstring")

(defun emdocs-client-filter (buffer sock msg)
  "docstring")

(defun emdocs-connect-client (buffer ip)
  "docstring"
  (make-network-process
   :name (concat "*emdocs-client:"
                 (buffer-name buffer) ":" ip "*")
   ;; TODO: remove the log buffer
   :buffer (concat "*emdocs-client-log:" (buffer-name buffer) "*")
   :family 'ipv4
   :host ip
   :service +emdocs-http-port+
   :sentinel (lambda (sock msg)
               (emdocs-client-sentinel buffer sock msg))
   :filter (lambda (sock msg)
             (emdocs-client-filter buffer sock msg))
   :server nil
   :noquery t))

(defun emdocs-attach-sockets-to-buffer (buffer ip)
  "docstring"
  (let ((my-ip (emdocs-get-internal-ip-address)))
    (if my-ip
        (progn
          (make-local-variable 'emdocs-server)
          (setq emdocs-server nil)
          (setq emdocs-server           ; the setq is required, not sure why
                (condition-case emdocs-server
                    (emdocs-setup-server buffer my-ip)
                  (file-error 'no-conn)))
          (if (eq emdocs-server 'no-conn)
              (progn
                (message "Server could not connect: exiting.")
                (setq emdocs-mode nil)
                (emdocs-disconnect))
            (make-local-variable 'emdocs-client-list)
            (setq emdocs-client-list nil)
            (unless (or (string-equal ip "localhost")
                        (string-equal ip ""))
              (setq emdocs-client-list
                    (condition-case emdocs-client-list
                        (cons (emdocs-connect-client buffer ip) nil)
                      (file-error 'no-conn))))
            (when (eq emdocs-client-list 'no-conn)
              (message "Client could not connect: exiting.")
              (setq emdocs-mode nil)
              (emdocs-disconnect))))
      (message "Not connected to internet: exiting.")
      (setq emdocs-mode nil)
      (emdocs-disconnect))))

(defun emdocs-connect ()
  "docstring"
  (make-local-variable 'emdocs-ip-addr)
  (setq emdocs-ip-addr (read-from-minibuffer "ip: "))
  (emdocs-attach-sockets-to-buffer (current-buffer) emdocs-ip-addr))

(defun emdocs-disconnect ()
  "docstring"
  (when (and emdocs-server
             (not (eq emdocs-server 'no-conn)))
    (delete-process emdocs-server))
  (makunbound 'emdocs-server)
  (loop for client in emdocs-client-list
        do (when (and client
                      (not (eq client 'no-conn)))
             (delete-process client)))
  (makunbound 'emdocs-client-list))

;;; define mode

(define-minor-mode emdocs-mode
  "docstring"
  :lighter " MDox"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'emdocs-mode)
            map)
  (if emdocs-mode
      (emdocs-connect)
    (emdocs-disconnect)))

(provide 'emdocs-mode)
