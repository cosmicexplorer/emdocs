;;; emdocs.el --- collaborative sharing of buffers -*- lexical-binding: t -*-

;; Author: Danny McClanahan
;; Version: 0.1
;; URL: https://github.com/cosmicexplorer/emdocs
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (dash "2.13.0") (uuid "0.0.3"))
;; Keywords: find, file, files, helm, fast, rg, ripgrep, grep, search, match

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; ???


;; License:

;; This file is licensed under the GPL 3.0 or any later version, not the AGPL as is the
;; 'emdocs' binary.

;; End Commentary


;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 'uuid)


;; Constants
(defconst emdocs--process-name "*emdocs--emdocs*")
(defconst emdocs--process-buffer-name "*emdocs--emdocs-output*")

(defconst emdocs--error-buffer-name "*emdocs-errors*")


;; Locals
(defvar-local emdocs-buffer-id nil
  "???")


;; Globals
(defvar emdocs-buffer-mapping nil
  "???")


;; Logic
(defun emdocs--interaction-filter (proc string)
  (when (process-live-p proc)
    ;; TODO: would love to use the native json lib, but that's not available in most emacs (and
    ;; likely never will be? consider making a native rust module?).
    (let ((msg (json-read-from-string string)))
      (unless (and (stringp msg) (string-equal msg "ok"))
        (let* ((op (alist-get 'op msg))
               (source (alist-get 'source op))
               (transform (alist-get 'transform op)))
          (--> (alist-get 'uuid source)
               (format ":%s" it)
               (message "%s" it)
               (intern it)
               (plist-get emdocs-buffer-mapping it)
               (message "source: %s" it))
          (message "transform: %S" transform))))))

(defun emdocs--create-buffer-id (source)
  `(:uuid ,source))

(defun emdocs--create-point (point)
  `(:code_point_index ,point))

(defun emdocs--create-insert (contents)
  `(:insert (:contents ,contents)))

(defun emdocs--create-delete (distance)
  `(:delete (:distance ,distance)))

(defun emdocs--create-edit (point payload)
  `(:edit (:point ,point :payload ,payload)))

(defun emdocs--create-transform (type)
  `(:type ,type))

(defun emdocs--create-operation (source transform)
  `(:source ,source :transform ,transform))

(defun emdocs--create-ide-message (op)
  `(:op ,op))

(defun emdocs--create-region (start end)
  `(:start ,start :end ,end))

(defun emdocs--create-sync-checksum (checksum)
  `(:checksum (:checksum ,checksum)))

(defun emdocs--create-sync-contents (contents)
  `(:contents (:contents ,contents)))

(defun emdocs--create-sync-region (region type)
  `(:sync (:region ,region :type ,type)))

(defun emdocs--sync-whole-buffer ()
  (cl-assert (= 1 (point-min)) t)
  (let* ((source (emdocs--create-buffer-id emdocs-buffer-id))
         (beg (emdocs--create-point (point-min)))
         (end (emdocs--create-point (point-max)))
         (region (emdocs--create-region beg end)))
    (->> (buffer-string)
         (emdocs--create-sync-contents)
         (emdocs--create-sync-region region)
         (emdocs--create-transform)
         (emdocs--create-operation source)
         (emdocs--create-ide-message)
         (emdocs--write-stdin))))

(defun emdocs--write-stdin (x)
  ;; TODO: consider batching these changes and then writing them when emacs is idle? Only necessary
  ;; if latency is currently insufficient.
  ;; TODO: also consider ways to avoid overhead of copying strings from a buffer with
  ;; (buffer-substring), should the overhead turn out to be significant.
  (when (emdocs--process-live-p)
    (let ((line (format "%s\n" (json-encode x))))
      (process-send-string emdocs--process-name line))))

(defun emdocs--make-process ()
  (message "%s" "initiating emdocs process...")
  (make-process
   :name emdocs--process-name
   :buffer emdocs--process-buffer-name
   :command '("/home/cosmicexplorer/projects/active/emdocs/target/debug/emdocs" "interact")
   :filter #'emdocs--interaction-filter
   :noquery t
   :stderr (get-buffer-create emdocs--error-buffer-name)))

(defun emdocs--process-live-p ()
  (process-live-p (get-process emdocs--process-name)))

(defun emdocs--after-change-function (beg end prev-length)
  (when emdocs-mode
    (let ((source (emdocs--create-buffer-id emdocs-buffer-id))
          (point (emdocs--create-point beg)))
      (let ((delete-edit (unless (zerop prev-length)
                         (emdocs--create-delete prev-length)))
            (insert-edit (unless (= beg end)
                         (->> (buffer-substring beg end)
                              (emdocs--create-insert)))))
        (cl-assert (or delete-edit insert-edit) t "every change must be an insert, delete, or both")
        (cl-loop for edit in `(,delete-edit ,insert-edit)
                 when edit
                 do (->> edit
                         (emdocs--create-edit point)
                         (emdocs--create-transform)
                         (emdocs--create-operation source)
                         (emdocs--create-ide-message)
                         (emdocs--write-stdin)))))))

(defun emdocs--before-change-function (beg end)
  ;; TODO: set up undo list, etc
  )

(defun emdocs--setup-buffer-idempotent ()
  (unless emdocs-buffer-id
    (setq-local emdocs-buffer-id (uuid-string)))
  (setq emdocs-buffer-mapping
        (--> (format ":%s" emdocs-buffer-id)
             (intern it)
             (plist-put emdocs-buffer-mapping it (current-buffer))))
  (add-hook 'after-change-functions #'emdocs--after-change-function nil t)
  (add-hook 'before-change-functions #'emdocs--before-change-function nil t))

(defun emdocs--initiate-process-idempotent ()
  (unless (emdocs--process-live-p)
    (emdocs--make-process)))


;; Interactive

(defun emdocs-kill-process ()
  (interactive)
  (when (emdocs--process-live-p)
    (message "%s" "killing emdocs process...")
    (kill-process emdocs--process-name)))

(defun emdocs-restart-process ()
  (interactive)
  (emdocs-kill-process)
  (emdocs--make-process))

;;;###autoload
(define-minor-mode emdocs-mode
  "???"
  :lighter " MDox"
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (if emdocs-mode
      (progn
        (emdocs--setup-buffer-idempotent)
        (emdocs--initiate-process-idempotent)
        (emdocs--sync-whole-buffer)
        t)
    nil))

(provide 'emdocs)
;;; emdocs.el ends here
