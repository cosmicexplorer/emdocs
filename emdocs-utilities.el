(defun emdocs-get-network-interfaces ()
  "return network interfaces from ifconfig as a list of strings"
  (split-string
   (substring
    (shell-command-to-string "ifconfig | grep -Po \"^[[:alnum:]]+(?=:)\"")
    0 -1)))

(defun emdocs-get-ip-address (&optional device-name)
  (if device-name
      (format-network-address (car (network-interface-info device-name)) t)
    (let ((net-device nil))
      (loop for dev in (get-network-interfaces)
            do (when (and (network-interface-info dev)
                          (not (string-equal dev "lo")))
                 (setq net-device
                       (format-network-address
                        (car (network-interface-info dev))
                        t))))
      net-device)))
