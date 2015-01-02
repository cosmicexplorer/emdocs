;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl)
  (require 'json))

;;; globals

(defconst +emdocs-http-port+ 8080
  "docstring")

(defvar *emdocs-server* nil
  "docstring")

;;; functions

;;; utility
(defun emdocs-get-internal-ip-address (&optional device-name)
  "Returns ip address of active interface, ignoring loopback. Returns nil if
none active. Returns an arbitrary interface if more than one is connected."
  (if device-name
      (format-network-address (car (network-interface-info device-name)) t)
    (let ((network-interfaces (network-interface-list)))
      ;; relies on loopback always being listed last in list
      (if (string-equal (caar network-interfaces) "lo")
          nil
        (format-network-address (cdar (network-interface-list)) t)))))

(defun emdocs-get-server-process-buffer ()
  "docstring"
  (concat "*emdocs-server-log*"))

(defun emdocs-get-client-process-buffer (buffer)
  "docstring"
  (concat "*emdocs-client-log:" (buffer-name buffer) "*"))

(defun emdocs-get-server-process-name ()
  "docstring"
  (concat "*emdocs-server*"))

(defun emdocs-get-client-process-name (buffer ip)
  "docstring"
  (concat "*emdocs-client:" (buffer-name buffer) ":" ip "*"))

(defun emdocs-server-sentinel (sock msg)
  "docstring"
  (with-current-buffer (emdocs-get-server-process-buffer)
    (goto-char (point-min))
    (insert "sentinel:" msg)
    (newline))
  (cond ((string-match "^open from [0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\n$" msg)
         (process-send-string sock "give your ip and buffer\n"))))

(defun emdocs-server-filter (sock msg)
  "docstring"
  (with-current-buffer (emdocs-get-server-process-buffer)
    (goto-char (point-min))
    (insert "filter:" msg)
    (newline)))

(defun emdocs-setup-server (my-ip)
  "docstring"
  (make-network-process
   :name (emdocs-get-server-process-name)
   ;; TODO: remove the log buffer
   :buffer (emdocs-get-server-process-buffer)
   :family 'ipv4
   :host my-ip
   :service +emdocs-http-port+
   :sentinel #'emdocs-server-sentinel
   :filter #'emdocs-server-filter
   :server t
   :noquery t))

(defun emdocs-client-sentinel (buffer sock msg)
  "docstring"
  (with-current-buffer (emdocs-get-client-process-buffer buffer)
    (goto-char (point-min))
    (insert "sentinel:" msg)
    (newline)))

(defun emdocs-client-filter (buffer sock msg)
  "docstring"
  (with-current-buffer (emdocs-get-client-process-buffer buffer)
    (goto-char (point-min))
    (insert "filter:" msg)
    (newline))
  (cond ((string-match "^give your ip and buffer\n$" msg)
         (process-send-string sock
          (json-encode `(:buffer ,(buffer-name buffer)
                         :ip ,(emdocs-get-internal-ip-address)))))))

(defun emdocs-connect-client (buffer ip)
  "docstring"
  (make-network-process
   :name (emdocs-get-client-process-name buffer ip)
   ;; TODO: remove the log buffer
   :buffer (emdocs-get-client-process-buffer buffer)
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
        (unless *emdocs-server*
          (setq *emdocs-server*           ; the setq is required, not sure why
                (condition-case *emdocs-server*
                    (emdocs-setup-server my-ip)
                  (file-error 'no-conn)))
          (if (eq *emdocs-server* 'no-conn)
              (progn
                (message "Server could not connect: exiting.")
                (setq *emdocs-server* nil)
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
            (if (eq emdocs-client-list 'no-conn)
                (progn (message "Client could not connect: exiting.")
                       (setq emdocs-mode nil)
                       (emdocs-disconnect))
              (insert "DO AFTER-CHANGE FUNCTIONS"))))
      (message "Not connected to internet: exiting.")
      (setq emdocs-mode nil)
      (emdocs-disconnect))))

(defun emdocs-connect ()
  "docstring"
  (make-local-variable 'emdocs-ip-addr)
  (setq emdocs-ip-addr (read-from-minibuffer "ip: "))
  ;; check for private ips
  (if (or (string-match "^192\\.168\\.[0-9]+\\.[0-9]+$" emdocs-ip-addr)
          (string-match "^10\\.[0-9]+\\.[0-9]+\\.[0-9]+$" emdocs-ip-addr)
          (string-match "^172\\.1[6789]\\.[0-9]+\\.[0-9]+$" emdocs-ip-addr)
          (string-match "^172\\.2[0-9]\\.[0-9]+\\.[0-9]+$" emdocs-ip-addr)
          (string-match "^172\\.3[01]\\.[0-9]+\\.[0-9]+$" emdocs-ip-addr)
          (string-equal "" emdocs-ip-addr)
          (string-equal "localhost" emdocs-ip-addr))
      (emdocs-attach-sockets-to-buffer (current-buffer) emdocs-ip-addr)
    (message
"Invalid ip address. Keep in mind emdocs only works for private ip addresses.")
    (setq emdocs-mode nil)))

(defun emdocs-disconnect ()
  "docstring"
  (loop for client in emdocs-client-list
        do (when (and client
                      (not (eq client 'no-conn)))
             (delete-process client)))
  (makunbound 'emdocs-client-list))

(defun emdocs-kill-server ()
  "docstring"
  (interactive)
  (if (and *emdocs-server*
           (not (eq (process-status *emdocs-server*) 'closed)))
      (delete-process *emdocs-server*))
  (setq *emdocs-server* nil))

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
