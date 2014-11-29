;; -*- lexical-binding: t; -*-
(defconst +emdocs-external-http-port+ 8080)
(defconst +emdocs-internal-http-port+ 8081)

(defconst +emdocs-conn-broken-msg-regex+ "^connection broken by remote peer")
(defconst +emdocs-conn-added-msg-regex+ "^open from")

(defconst +emdocs-edit-msg-header+ "EDIT_MSG")
(defconst +emdocs-ask-for-address-header+ "ASK_ADDR")
(defconst +emdocs-receive-address-header+ "REC_ADDR")
(defconst +emdocs-client-add-header+ "CLIENT_ADD")

(defconst emdocs-insert-edit "insert")
(defconst emdocs-delete-edit "delete")
(defconst emdocs-indel-edit "indel")

(defun emdocs-get-internal-ip-address (&optional device-name)
  "Returns ip address of active interface, ignoring loopback. Returns nil if
none active."
  (if device-name
      (format-network-address (car (network-interface-info device-name)) t)
    (let ((network-interfaces (network-interface-list)))
      ;; relies on loopback being listed last in list
      (if (string-equal (caar network-interfaces) "lo")
          nil
        (format-network-address (cdar (network-interface-list)) t)))))

(defun emdocs-get-global-ip-address ()
  (shell-command-to-string "curl http://ipecho.net/plain"))
