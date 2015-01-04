;; -*- lexical-binding: t; -*-

;;; requires

(eval-when-compile
  (require 'cl)
  (require 'json)
  (require 'eieio))

;;; globals

(defconst +emdocs-http-port+ 8080
  "docstring")

(defvar *emdocs-server* nil
  "docstring")

(defvar *emdocs-outgoing-clients* nil
  "docstring")

(defvar *emdocs-incoming-clients* nil
  "docstring")

;;; locals

(defvar-local emdocs-initial-client nil
  "docstring")

(defvar-local emdocs-ip-addr nil
  "docstring")

(defvar-local emdocs-is-network-insert nil
  "docstring")

(defvar-local emdocs-after-change-lambda nil
  "docstring")

;;; classes

(defclass emdocs-client ()
  ((process
    :initarg :process
    :accessor emdocs-get-process
    :documentation "docstring")
   (attached-buffer
    :initarg :attached-buffer
    :accessor emdocs-get-attached-buffer
    :documentation "docstring")
   (ip
    :initarg :ip
    :accessor emdocs-get-ip
    :documentation "docstring"))
  "docstring")

;;; functions

(defun emdocs-get-internal-ip-address (&optional device-name)
  "Returns ip address of active network interface, ignoring loopback. Returns
nil if none active. Returns an arbitrary interface if more than one is
connected."
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
  (if (bufferp buffer)
      (concat "*emdocs-client-log:" (buffer-name buffer) "*")
    (concat "*emdocs-client-log:" buffer "*")))

(defun emdocs-get-server-process-name ()
  "docstring"
  "*emdocs-server*")

(defun emdocs-get-client-process-name (buffer ip)
  "docstring"
  (if (bufferp buffer)
      (concat "*emdocs-client:" (buffer-name buffer) ":" ip "*")
    (concat "*emdocs-client:" buffer "*")))

(defun emdocs-is-ip-from-client (test-ip client)
  (string-equal test-ip
                (emdocs-get-ip client)))

(defun emdocs-server-sentinel (sock msg)
  "docstring"
  (with-current-buffer (emdocs-get-server-process-buffer)
    (goto-char (point-min))
    (insert "sentinel:" msg)
    (unless (bolp) (newline)))
  (cond ((string-match "^open from [0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\n$" msg)
         (let ((ip (progn
                     (string-match "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+" msg)
                     (match-string-no-properties 0 msg))))
           (unless (find ip *emdocs-incoming-clients*
                         :test #'emdocs-is-ip-from-client)
             (process-send-string sock "give me buffer and ip\n"))))
        ((string-match "^connection broken by remote peer\n$" msg)
         (setq *emdocs-incoming-clients*
               (remove-if
                (lambda (client)
                  (equal (emdocs-get-process client) sock))
                *emdocs-incoming-clients*)))))

(defun emdocs-server-filter (sock msg)
  "docstring"
  ;; assumes will only receive json from emdocs-client-filter
  (with-current-buffer (emdocs-get-server-process-buffer)
    (goto-char (point-min))
    (insert "filter:" msg)
    (unless (bolp) (newline)))
  (let* ((json-object-type 'plist)
         (json-msg (json-read-from-string msg))
         (buffer (plist-get json-msg :buffer))
         (ip (plist-get json-msg :ip))
         (get-buffer-contents (plist-get json-msg :get_buffer_contents)))
    (cond (ip                           ; if given buffer and ip to connect to
           (unless (find ip *emdocs-incoming-clients*
                         :test #'emdocs-is-ip-from-client)
             (add-to-list '*emdocs-incoming-clients*
                          (make-instance 'emdocs-client
                                         :process sock
                                         :attached-buffer buffer
                                         :ip ip))
             (unless (find ip *emdocs-outgoing-clients*
                           :test #'emdocs-is-ip-from-client)
               (emdocs-connect-client buffer ip))
             (emdocs-broadcast-message msg)))
          (get-buffer-contents          ; if asking for contents of buffer
           (with-current-buffer buffer
             (process-send-string
              sock
              (json-encode `(:buffer ,buffer
                             :buffer_contents ,(buffer-string)))))))))

(defun emdocs-setup-server (my-ip)
  "docstring"
  (make-network-process
   :name (emdocs-get-server-process-name)
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
  (when (string-match "^connection broken by remote peer\n$" msg)
    (setq *emdocs-outgoing-clients*
          (remove-if
           (lambda (client)
             (equal (emdocs-get-process client) sock))
           *emdocs-outgoing-clients*))))

(defun emdocs-client-filter (buffer sock msg)
  "docstring"
  ;; assumes will only receive json from emdocs-after-change-function
  (with-current-buffer (emdocs-get-client-process-buffer buffer)
    (goto-char (point-min))
    (insert "filter:" msg)
    (unless (bolp) (newline)))
  (if (string-match "^give me buffer and ip\n$" msg)
      (process-send-string
       sock
       (json-encode
        `(:buffer ,(if (bufferp buffer)
                       (buffer-name buffer)
                     buffer)
          :ip ,(emdocs-get-internal-ip-address))))
    (let* ((json-object-type 'plist)
           (json-msg (json-read-from-string msg))
           (buffer (plist-get json-msg :buffer))
           (ip (plist-get json-msg :ip))
           (type (plist-get json-msg :edit_type))
           (cur-point (plist-get json-msg :point))
           (content (plist-get json-msg :content))
           (buffer-contents (plist-get json-msg :buffer_contents)))
      (cond (ip                       ; if being told to connect
             (unless (find ip *emdocs-outgoing-clients*
                           :test #'emdocs-is-ip-from-client)
               (emdocs-connect-client buffer ip)))
            (cur-point
             (with-current-buffer buffer
               (save-excursion
                 (setq emdocs-is-network-insert t)
                 (unwind-protect
                     (cond
                      ((string-equal "insert" type)
                       (goto-char cur-point)
                       (insert content))
                      ((string-equal "delete" type)
                       (goto-char cur-point)
                       (delete-char content)))
                   (setq emdocs-is-network-insert nil)))))
            (buffer-contents
             (with-current-buffer buffer
               (let ((prev-point (point)))
                 (erase-buffer)
                 (insert buffer-contents)
                 (goto-char prev-point))))))))

(defun emdocs-ask-for-buffer-contents (buffer sock)
  "docstring"
  (when (process-live-p sock)
    (process-send-string
     sock
     (json-encode `(:get_buffer_contents ,t
                    :buffer ,(if (bufferp buffer)
                                 (buffer-name buffer)
                               buffer))))
    (run-at-time "1 min" nil #'emdocs-ask-for-buffer-contents buffer sock)))

(defun emdocs-connect-client (buffer ip)
  "docstring"
  (unless (string-equal ip (emdocs-get-internal-ip-address))
    (let ((process (make-network-process
                    :name (emdocs-get-client-process-name buffer ip)
                    :buffer (emdocs-get-client-process-buffer buffer)
                    :family 'ipv4
                    :host ip
                    :service +emdocs-http-port+
                    :sentinel (lambda (sock msg)
                                (emdocs-client-sentinel buffer sock msg))
                    :filter (lambda (sock msg)
                              (emdocs-client-filter buffer sock msg))
                    :server nil
                    :noquery t)))
      (add-to-list '*emdocs-outgoing-clients*
                   (make-instance 'emdocs-client
                                  :process process
                                  :attached-buffer (if (bufferp buffer)
                                                       (buffer-name buffer)
                                                     buffer)
                                  :ip ip))
      (run-at-time
       "2 sec" nil
       (lambda () (emdocs-ask-for-buffer-contents buffer process))))))

(defun emdocs-broadcast-message (msg)
  "docstring"
  (loop for client in *emdocs-incoming-clients*
        do (process-send-string (emdocs-get-process client) msg)))

(defun emdocs-emit-keypress-json (buffer type point content)
  "docstring"
  (emdocs-broadcast-message
   (json-encode
    `(:buffer ,(if (bufferp buffer)
                   (buffer-name buffer)
                 buffer)
      :edit_type ,type
      :point ,point
      :content ,content))))

(defun emdocs-after-change-function (buffer beg end prev-length)
  "docstring"
  (with-current-buffer (buffer-name buffer)
    (if (boundp 'emdocs-is-network-insert)
        (unless emdocs-is-network-insert
          (cond ((= prev-length 0)
                 (emdocs-emit-keypress-json
                  buffer "insert" beg (buffer-substring beg end)))
                ((= beg end)
                 (emdocs-emit-keypress-json
                  buffer "delete" beg prev-length)))))))

(defun emdocs-attach-sockets-to-buffer (buffer ip)
  "docstring"
  (let ((my-ip (emdocs-get-internal-ip-address)))
    (if my-ip
        (progn
          (unless *emdocs-server*
            (setq *emdocs-server*           ; the setq is required, not sure why
                  (condition-case *emdocs-server*
                      (emdocs-setup-server my-ip)
                    (file-error 'no-conn))))
          (if (eq *emdocs-server* 'no-conn)
              (progn
                (message "Server could not be started: exiting.")
                (setq *emdocs-server* nil)
                (setq emdocs-mode nil)
                (emdocs-disconnect))
            (setq emdocs-initial-client nil)
            (unless (or (string-equal ip "localhost")
                        (string-equal ip ""))
              (setq emdocs-initial-client
                    (condition-case emdocs-initial-client
                        (emdocs-connect-client buffer ip)
                      (file-error 'no-conn))))
            (if (eq emdocs-initial-client 'no-conn)
                (progn (message "Client could not connect: exiting.")
                       (setq emdocs-mode nil)
                       (emdocs-disconnect))
              (with-current-buffer buffer
                (setq-local emdocs-is-network-insert nil)
                (setq-local emdocs-after-change-lambda
                            (lambda (beg end prev-length)
                              (with-current-buffer buffer
                                (emdocs-after-change-function
                                 buffer beg end prev-length))))
                (setq-local after-change-functions
                            (cons emdocs-after-change-lambda
                                  after-change-functions))
                (add-hook 'kill-buffer-hook #'emdocs-disconnect)
                (add-hook 'kill-emacs-hook #'emdocs-kill-server)))))
      (message "Not connected to internet: exiting.")
      (setq emdocs-mode nil)
      (emdocs-disconnect))))

(defun emdocs-connect ()
  "docstring"
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
     (concat
      "Invalid ip address. "
      "Keep in mind emdocs only works for private ip addresses."))
    (setq emdocs-mode nil)))

(defun emdocs-test-if-client-attached-to-buffer (client)
  (string-equal
   (emdocs-get-attached-buffer client)
   (buffer-name)))

(defun emdocs-disconnect ()
  "docstring"
  (loop for client in *emdocs-outgoing-clients*
        do (when (emdocs-test-if-client-attached-to-buffer client)
             (delete-process (emdocs-get-process client))))
  (setq *emdocs-outgoing-clients*
        (remove-if #'emdocs-test-if-client-attached-to-buffer
                   *emdocs-outgoing-clients*))
  (loop for client in *emdocs-incoming-clients*
        do (when (emdocs-test-if-client-attached-to-buffer client)
             (delete-process (emdocs-get-process client))))
  (setq *emdocs-incoming-clients*
        (remove-if #'emdocs-test-if-client-attached-to-buffer
                   *emdocs-incoming-clients*))
  (setq-local after-change-functions
              (remove emdocs-after-change-lambda after-change-functions))
  (setq-local emdocs-after-change-lambda nil))

;;; interactives
(defun emdocs-kill-server ()
  "docstring"
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
