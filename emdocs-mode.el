;;; keypress notifications
(load-file "./emdocs-server-functions.el")
(load-file "./emdocs-client-functions.el")

(defvar *emdocs-server-table* (make-hash-table :test 'eq :weakness nil))
(defvar *emdocs-client-table* (make-hash-table :test 'eq :weakness nil))

;;; integration functions
(defun emdocs-connect (input-ip-address)
  (interactive "Mip address (blank for none): ")
  (let* ((base-proc-name (concat input-ip-address
                           ":"
                           (number-to-string +emdocs-internal-http-port+)
                           ":"
                           (buffer-name)))
        (base-buf-name (concat (buffer-name)))
        (server-to-add
         (make-instance
          'emdocs-server
          :process-name (concat "emdocs-server:" base-proc-name)
          :log-buffer (concat "emdocs-server:" base-buf-name)))
        (client-to-add
         (make-instance
          'emdocs-client
          :process-name (concat "emdocs-client:" base-proc-name)
          :log-buffer (concat "emdocs-client:" base-buf-name))))
    (puthash
     server-to-add
     (buffer-name)
     *emdocs-server-table*)
    (puthash
     client-to-add
     (buffer-name)
     *emdocs-client-table*)
    (emdocs-server-start server-to-add (buffer-name))
    (emdocs-client-start client-to-add (buffer-name))))

(defun get-server ()
  (if (= (hash-table-count *emdocs-server-table*) 1)
      (let ((single-server nil))
        (maphash '(lambda (serv buf)
                    (setq single-server serv))
                 *emdocs-server-table*)
        single-server)
    (throw 'not-helpful t)))

(defun get-client ()
  (if (= (hash-table-count *emdocs-client-table*) 1)
      (let ((single-client nil))
        (maphash '(lambda (cli buf)
                    (setq single-client cli))
                 *emdocs-client-table*)
        single-client)
    (throw 'not-helpful t)))

(defun emdocs-disconnect ()
  (interactive)
  (let ((servers-to-disconnect nil))
    (maphash '(lambda (serv-obj buf-name)
                (when (string-equal (buffer-name) buf-name)
                  (emdocs-server-stop serv-obj)
                  (setq servers-to-disconnect ; add-to-list not working fsr
                        (cons serv-obj servers-to-disconnect))))
             *emdocs-server-table*)
    (loop for serv-obj in servers-to-disconnect
          do (remhash serv-obj *emdocs-server-table*)))
  (let ((clients-to-disconnect nil))
    (maphash '(lambda (client-obj buf-name)
                (when (string-equal (buffer-name) buf-name)
                  (emdocs-client-stop client-obj)
                  (setq clients-to-disconnect
                        (cons client-obj clients-to-disconnect))))
             *emdocs-client-table*)
    (loop for client-obj in clients-to-disconnect
          do (remhash client-obj *emdocs-client-table*))))

(defun emdocs-disconnect-all ()
  (interactive)
  (maphash '(lambda (serv-obj buf-name)
              (emdocs-server-stop serv-obj))
           *emdocs-server-table*)
  (clrhash *emdocs-server-table*)
  (maphash '(lambda (client-obj buf-name)
              (emdocs-client-stop client-obj))
           *emdocs-client-table*)
  (clrhash *emdocs-client-table*)
  nil)

(provide 'emdocs)


;; (hash-table-count *emdocs-server-table*)
;; (hash-table-count *emdocs-client-table*)
