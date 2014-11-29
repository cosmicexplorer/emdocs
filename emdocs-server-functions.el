;; -*- lexical-binding: t; -*-
;;; see https://stackoverflow.com/questions/27166957/
;;; emacs-not-accepting-lambda-in-after-change-functions

;;; server socket connection
(defmethod emdocs-server-start-on-buffer ((server emdocs-server)
                                          buffer-to-broadcast)
  (emdocs-start server)
  (setf (emdocs-get-attached-buffer server) buffer-to-broadcast)
  (emdocs-attach-and-set-change-functions server buffer-to-broadcast))

(defmethod emdocs-stop :before ((server emdocs-server)
                               &optional cleanup-socket)
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

(defmethod emdocs-filter :after ((server emdocs-server) client-socket message)
  (cond ((string-match (concat "^" +emdocs-receive-address-header+) message)
         (message (substring message
                             (length +emdocs-receive-address-header+))))))

(defmethod emdocs-sentinel :after ((server emdocs-server) client-socket message)
  ;; TODO: add p2p support
  ;; (cond ((string-match +emdocs-conn-added-msg-regex+ message)
  ;;        (emdocs-broadcast-message
  ;;         server
  ;;         (concat +emdocs-client-add-header+
  ;;                 ;; this is the remote ip address of the socket
  ;;                 (car (process-contact client-socket))))))
  )

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
         (throw 'unknown-edit-type t))))

(require 'json)
(defmethod emdocs-emit-keypress-json ((server emdocs-server) type point content)
  "Sends a keypress to the server also running so that it can be emitted to
other users on the network."
  (emdocs-broadcast-message
   server
   (concat
    +emdocs-edit-msg-header+
    (json-encode `(,:type ,type ,:point ,point ,:content ,content))
    "\n")))

(defmethod emdocs-attach-and-set-change-functions ((server emdocs-server)
                                                   name-of-buffer)
  "Adds appropriate after-change-functions to the given name-of-buffer."
  (unless (emdocs-get-after-change-function server)
    (setf (emdocs-get-after-change-function server)
          #'(lambda (beg end prev-length)
              (emdocs-notify-others-of-change
               server beg end prev-length)))
    (with-current-buffer name-of-buffer
      (setq-local after-change-functions
                  (cons
                   (emdocs-get-after-change-function server)
                   after-change-functions)))))
