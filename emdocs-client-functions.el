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
  (let* ((json-object-type 'plist)
         (json-message (json-read-from-string message)))
    (cond ((string-equal (plist-get json-message :message_type)
                         +emdocs-edit-msg-header+)
           (emdocs-edit-from-json
            client
            json-message))
          ((string-equal (plist-get json-message :message_type)
                         +emdocs-send-file-header+)
           (with-current-buffer (emdocs-get-attached-buffer client)
             (let ((prev-point (point)))
               (erase-buffer)
               (insert (plist-get json-message :content))
               (goto-char prev-point)))))))

(defmethod emdocs-client-send-message ((client emdocs-client) message)
  (process-send-string (emdocs-get-process client) message))

(require 'json)
(defmethod emdocs-edit-from-json ((client emdocs-client) json-message)
  (let ((type (plist-get json-message :edit_type))
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
