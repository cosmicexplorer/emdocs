;; -*- lexical-binding: t; -*-
;;; keypress notifications
(load-file "./emdocs-network-classes.el")

;;; singletons
;;; TODO: assuming no concurrent access
(let ((emdocs-self-socket nil)
      (emdocs-server-table nil)
      (emdocs-client-table nil))
  (defconst +emdocs-self-socket-process-name+
    (concat
     "emdocs-self-socket"
     ":"
     (number-to-string +emdocs-internal-http-port+)))
  (defconst +emdocs-self-socket-log-buffer+ +emdocs-self-socket-process-name+)
  (defun emdocs-get-global-self-socket ()
    (if emdocs-self-socket
        emdocs-self-socket
      (setq emdocs-self-socket
            (make-instance
             'emdocs-server
             :process-name +emdocs-self-socket-process-name+
             :log-buffer +emdocs-self-socket-log-buffer+
             :port +emdocs-internal-http-port+))
      (emdocs-start emdocs-self-socket emdocs-self-socket)))
  (defun emdocs-get-global-server-table ()
    (if emdocs-server-table
        emdocs-server-table
      (setq emdocs-server-table
            (make-hash-table :test 'eq :weakness nil))))
  (defun emdocs-get-global-client-table ()
    (if emdocs-client-table
        emdocs-client-table
      (setq emdocs-client-table
            (make-hash-table :test 'eq :weakness nil))))
  (defun emdocs-kill-globals ()
    (when emdocs-server-table
      (maphash #'(lambda (serv-obj buf-name)
                  (emdocs-stop serv-obj))
               emdocs-server-table)
      (clrhash emdocs-server-table)
      (setq emdocs-server-table nil))
    (when emdocs-client-table
      (maphash #'(lambda (client-obj buf-name)
                  (emdocs-stop client-obj))
               emdocs-client-table)
      (clrhash emdocs-client-table)
      (setq emdocs-client-table nil))
    (when emdocs-self-socket
      (emdocs-stop emdocs-self-socket)
      (setq emdocs-self-socket nil))))

;;; integration functions
(defun emdocs-connect (input-ip-address)
  (interactive "Mip address (RET for none): ")
  ;; input-ip-address is "" if not given
  (let* ((base-proc-name (concat input-ip-address
                                 ":"
                                 (number-to-string +emdocs-external-http-port+)
                                 ":"
                                 (buffer-name)))
         (base-buf-name (concat (buffer-name)))
         (server-to-add
          (make-instance
           'emdocs-server
           :process-name (concat "emdocs-server:" base-proc-name)
           :log-buffer (concat "emdocs-server:" base-buf-name)
           :host (emdocs-get-global-ip-address)
           :port +emdocs-external-http-port+))
         (client-to-add
          (if (string-equal input-ip-address "")
              nil
            (make-instance
             'emdocs-client
             :process-name (concat "emdocs-client:" base-proc-name)
             :log-buffer (concat "emdocs-client:" base-buf-name)
             :port +emdocs-external-http-port+
             :address-connecting-to input-ip-address))))
    (puthash
     server-to-add
     (buffer-name)
     (emdocs-get-global-server-table))
    (emdocs-server-start-on-buffer server-to-add (buffer-name)
                                   (emdocs-get-global-self-socket))
    (unless (string-equal input-ip-address "")
      (puthash
       client-to-add
       (buffer-name)
       (emdocs-get-global-client-table))
      (emdocs-client-start-on-buffer client-to-add (buffer-name)
                                     (emdocs-get-global-self-socket)))))

;;; TODO: remove after debugging!!!!
;;; debugging functions for single-client single-server model
(defun get-server ()
  (if (= (hash-table-count (emdocs-get-global-server-table)) 1)
      (let ((single-server nil))
        (maphash #'(lambda (serv buf)
                    (setq single-server serv))
                 (emdocs-get-global-server-table))
        single-server)
    (throw 'not-helpful t)))
(defun get-client ()
  (if (= (hash-table-count (emdocs-get-global-client-table)) 1)
      (let ((single-client nil))
        (maphash #'(lambda (cli buf)
                    (setq single-client cli))
                 (emdocs-get-global-client-table))
        single-client)
    (throw 'not-helpful t)))

(defun emdocs-disconnect ()
  (interactive)
  (let ((servers-to-disconnect nil))
    (maphash #'(lambda (serv-obj buf-name)
                (when (string-equal (buffer-name) buf-name)
                  (emdocs-stop serv-obj)
                  (setq servers-to-disconnect ; add-to-list not working fsr
                        (cons serv-obj servers-to-disconnect))))
             (emdocs-get-global-server-table))
    (loop for serv-obj in servers-to-disconnect
          do (remhash serv-obj (emdocs-get-global-server-table))))
  (let ((clients-to-disconnect nil))
    (maphash #'(lambda (client-obj buf-name)
                (when (string-equal (buffer-name) buf-name)
                  (emdocs-stop client-obj)
                  (setq clients-to-disconnect
                        (cons client-obj clients-to-disconnect))))
             (emdocs-get-global-client-table))
    (loop for client-obj in clients-to-disconnect
          do (remhash client-obj (emdocs-get-global-client-table)))))

(defun emdocs-disconnect-all ()
  (interactive)
  (emdocs-kill-globals))

(provide 'emdocs)
