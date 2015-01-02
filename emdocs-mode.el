;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl)
  (require 'json))

;;; globals

(defconst +emdocs-http-port+ 8080
  "docstring")

(defvar *emdocs-server* nil
  "docstring")

(defvar *emdocs-clients* nil
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
  "*emdocs-server-log*")

(defun emdocs-get-client-process-buffer (buffer)
  "docstring"
  (concat "*emdocs-client-log:" (buffer-name buffer) "*"))

(defun emdocs-get-server-process-name ()
  "docstring"
  "*emdocs-server*")

(defun emdocs-get-client-process-name (buffer ip)
  "docstring"
  (concat "*emdocs-client:" (buffer-name buffer) ":" ip "*"))

(defun emdocs-server-sentinel (sock msg)
  "docstring"
  (with-current-buffer (emdocs-get-server-process-buffer)
    (goto-char (point-min))
    (insert "sentinel:" msg)
    (unless (bolp) (newline)))
  (cond ((string-match "^open from [0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\n$" msg)
         (process-send-string sock "give me buffer and ip")
         (add-to-list '*emdocs-clients* sock))
        ((string-match "^connection broken by remote peer\n$" msg)
         (setq *emdocs-clients* (delete sock *emdocs-clients*)))))

(defun emdocs-server-filter (sock msg)
  "docstring"
  ;; assumes will only receive json from emdocs-client-sentinel
  (with-current-buffer (emdocs-get-server-process-buffer)
    (goto-char (point-min))
    (insert "filter:" msg)
    (unless (bolp) (newline)))
  (let* ((json-object-type 'plist)
         (json-msg (json-read-from-string msg)))
    (emdocs-connect-client (plist-get json-msg :buffer)
                           (plist-get json-msg :ip))))

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
    (unless (bolp) (newline)))
  (cond ((string-match "^give me buffer and ip\n$" msg)
         (process-send-string
          sock
          (json-encode
           `(:buffer ,(buffer-name buffer)
             :ip ,(emdocs-get-internal-ip-address)))))))

(defun emdocs-client-filter (buffer sock msg)
  "docstring"
  ;; assumes will only receive json from emdocs-after-change-function
  (with-current-buffer (emdocs-get-client-process-buffer buffer)
    (goto-char (point-min))
    (insert "filter:" msg)
    (unless (bolp) (newline)))
  (let* ((json-object-type 'plist)
         (json-msg (json-read-from-string msg)))
    (with-current-buffer (plist-get json-msg :buffer)
      (save-excursion
        (setq emdocs-is-network-insert t)
        (unwind-protect
            (cond
             ((string-equal "insert" (plist-get json-msg :edit_type))
              (goto-char (plist-get json-msg :point))
              (insert (plist-get json-msg :content)))
             ((string-equal "delete" (plist-get json-msg :edit_type))
              (goto-char (plist-get json-msg :point))
              (delete-char (plist-get json-msg :content))))
          (setq emdocs-is-network-insert nil))))))

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

(defun emdocs-broadcast-message (msg)
  (loop for client in *emdocs-clients*
        do (process-send-string client msg)))

(defun emdocs-emit-keypress-json (type point content)
  (emdocs-broadcast-message
   (json-encode
    `(:buffer ,(buffer-name)
      :edit_type ,type
      :point ,point
      :content ,content))))

(defun emdocs-after-change-function (beg end prev-length)
  (unless emdocs-is-network-insert
    (cond ((= prev-length 0)
           (emdocs-emit-keypress-json
            "insert" beg (buffer-substring beg end)))
          ((= beg end)
           (emdocs-emit-keypress-json
            "delete" beg prev-length)))))

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
              (with-current-buffer buffer
                (make-local-variable 'emdocs-is-network-insert)
                (setq emdocs-is-network-insert nil)
                (add-hook 'after-change-functions
                          #'emdocs-after-change-function)))))
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
  (makunbound 'emdocs-client-list)
  (remove-hook 'after-change-functions #'emdocs-after-change-function))

;;; interactives
(defun emdocs-kill-server ()
  "docstring"
  (interactive)
  (if (and *emdocs-server*
           (not (eq (process-status *emdocs-server*) 'closed)))
      (delete-process *emdocs-server*))
  (setq *emdocs-server* nil))

(defun emdocs-get-ip-address ()
  (interactive)
  (message (emdocs-get-internal-ip-address)))

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
