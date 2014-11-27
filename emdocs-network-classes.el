(defconst +emdocs-internal-http-port+ 8081)

(defclass emdocs-server ()
  ((process
    :initform nil
    :accessor emdocs-get-process)
   (process-name
    :initarg :process-name
    :initform nil
    :accessor emdocs-get-process-name)
   (log-buffer
    :initarg :log-buffer
    :initform nil
    :accessor emdocs-get-log-buffer)
   (attached-buffer
    :initarg :attached-buffer
    :initform nil
    :accessor emdocs-get-attached-buffer)
   (client-hash-table
    :initform (make-hash-table :test 'eq :weakness nil)
    :accessor emdocs-get-hash-table)
   (after-change-function
    :initform nil
    :accessor emdocs-get-after-change-function)))

(defclass emdocs-client ()
  ((process
    :initform nil
    :accessor emdocs-get-process)
   (process-name
    :initarg :process-name
    :initform nil
    :accessor emdocs-get-process-name)
   (log-buffer
    :initarg :log-buffer
    :initform nil
    :accessor emdocs-get-log-buffer)
   (attached-buffer
    :initarg :attached-buffer
    :initform nil
    :accessor emdocs-get-attached-buffer)
   (address-connecting-to
    :initform nil
    :initarg :address-connecting-to
    :access emdocs-get-address-connecting-to)))
