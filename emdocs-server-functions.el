;; -*- lexical-binding: t; -*-
;;; see https://stackoverflow.com/questions/27166957/
;;; emacs-not-accepting-lambda-in-after-change-functions

;;; server socket connection
;;; server factory method
(defun emdocs-make-server (global-ip buf-name)
  (make-instance
   'emdocs-server
   :process-name (concat "emdocs-server:"
                         global-ip ":"
                         (number-to-string +emdocs-external-http-port+) ":"
                         buf-name)
   :log-buffer (concat "emdocs-server:" buf-name)
   :port +emdocs-external-http-port+
   :host (emdocs-get-internal-ip-address)
   :global-ip global-ip
   :attached-buffer buf-name))

(defmethod emdocs-start :after ((server emdocs-server))
  (emdocs-attach-and-set-change-functions server))

(defmethod emdocs-stop :before ((server emdocs-server))
  (when (emdocs-get-after-change-function server)
    (with-current-buffer (emdocs-get-attached-buffer server)
      (setq-local after-change-functions
                  (remove-if
                   #'(lambda (func)
                       (eq func
                           (emdocs-get-after-change-function server)))
                   after-change-functions)))
    (setf (emdocs-get-after-change-function server) nil)
    (setf (emdocs-get-attached-buffer server) nil)))

(defmethod emdocs-sentinel :after ((server emdocs-server) client-socket message)
  ;; TODO: make this only occur the first time a client enters the network
  (cond ((string-match +emdocs-conn-added-msg-regex+ message)
         (with-current-buffer (emdocs-get-attached-buffer server)
           (process-send-string client-socket
                                (concat +emdocs-send-file-header+
                                        (buffer-string))))
         (let ((external-ip-of-socket
                (car (process-contact client-socket))))
           (unless (or (string-equal (emdocs-get-global-ip server)
                                     external-ip-of-socket)
                       (string-equal (emdocs-get-host server)
                                     external-ip-of-socket))
             (emdocs-attach-and-tableify
              (emdocs-make-client (emdocs-get-global-ip server)
                                  external-ip-of-socket
                                  (emdocs-get-attached-buffer server))
              (emdocs-get-singleton-client-table server)))))))

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
          emdocs-delete-edit beg prev-length))))

(defmethod emdocs-emit-keypress-json ((server emdocs-server) type point content)
  "Sends a keypress to the server also running so that it can be emitted to
other users on the network."
  (emdocs-broadcast-message
   server
   (concat
    +emdocs-edit-msg-header+
    (json-encode `(,:type ,type ,:point ,point ,:content ,content))
    "\n")))

(defmethod emdocs-attach-and-set-change-functions ((server emdocs-server))
  "Adds appropriate after-change-functions to the given name-of-buffer."
  (with-current-buffer (emdocs-get-attached-buffer server)
    (unless (emdocs-get-after-change-function server)
      (setq-local emdocs-is-network-insert nil)
      (setf (emdocs-get-after-change-function server)
            #'(lambda (beg end prev-length)
                (with-current-buffer (emdocs-get-attached-buffer server)
                  (unless emdocs-is-network-insert
                    (emdocs-notify-others-of-change
                     server beg end prev-length)))))
      (setq-local after-change-functions
                  (cons
                   (emdocs-get-after-change-function server)
                   after-change-functions)))))
