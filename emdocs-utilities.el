(defconst emdocs-insert-edit "insert")
(defconst emdocs-delete-edit "delete")
(defconst emdocs-indel-edit "indel")

(defun emdocs-get-global-ip-address (&optional device-name)
  "Returns ip address of active interface, ignoring loopback. Returns nil if
none active."
  (if device-name
      (format-network-address (car (network-interface-info device-name)) t)
    (let ((network-interfaces (network-interface-list)))
      ;; relies on loopback being listed last in list
      (if (string-equal (caar network-interfaces) "lo")
          nil
        (format-network-address (cdar (network-interface-list)) t)))))
