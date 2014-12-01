;; -*- lexical-binding: t; -*-
;;; see https://stackoverflow.com/questions/27166957/
;;; emacs-not-accepting-lambda-in-after-change-functions

;;; client socket connection
;;; client factory method
(defun emdocs-make-client (global-ip host buf-name)
  (make-instance
   'emdocs-client
   :process-name (concat "emdocs-client:"
                         global-ip ":"
                         (number-to-string +emdocs-external-http-port+) ":"
                         buf-name)
   :log-buffer (concat "emdocs-client:" buf-name)
   :port +emdocs-external-http-port+
   :host host
   :global-ip global-ip
   :attached-buffer buf-name))

(defmethod emdocs-stop :before ((client emdocs-client))
  (setf (emdocs-get-attached-buffer client) nil))

(defmethod emdocs-filter :after ((client emdocs-client)
                                 server-socket
                                 message)
  ;; mux based on header
  (cond ((string-match (concat "^" +emdocs-edit-msg-header+) message)
         (emdocs-receive-keypress
          client
          ;; get message minus header
          (substring message (length +emdocs-edit-msg-header+))))
        ((string-match (concat "^" +emdocs-send-file-header+) message)
         (with-current-buffer (emdocs-get-attached-buffer client)
           (let ((prev-point (point)))
             (erase-buffer)
             (insert
              (substring message (length +emdocs-send-file-header+)))
             (goto-char prev-point))))))

(defmethod emdocs-client-send-message ((client emdocs-client) message)
  (process-send-string (emdocs-get-process client) message))

(defmethod emdocs-receive-keypress ((client emdocs-client) message)
  (let ((json-object-type 'plist))
    (emdocs-edit-from-json client (json-read-from-string message))))

(require 'json)
(defmethod emdocs-edit-from-json ((client emdocs-client) json-message)
  (let ((type (plist-get json-message :type))
        (point (plist-get json-message :point))
        (content (plist-get json-message :content)))
    (with-current-buffer (emdocs-get-attached-buffer client)
      (save-excursion
        (setq-local emdocs-is-network-insert t)
        (unwind-protect
            (cond
             ((string-equal type emdocs-insert-edit)
              (goto-char point)
              (insert content))
             ((string-equal type emdocs-delete-edit)
              (goto-char point)
              (delete-char content)))
          (setq-local emdocs-is-network-insert nil))))))
