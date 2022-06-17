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
          (message "source: %s" (alist-get 'uuid source))
          (message "transform: %S" transform))))))

(defun emdocs--create-buffer-id (source)
  `(:uuid ,source))

(defun emdocs--create-point (point)
  `(:code_point_index ,point))

(defun emdocs--create-insert (point contents)
  `(:point ,point :payload (:insert (:contents ,contents))))

(defun emdocs--create-delete (point distance)
  `(:point ,point :payload (:delete (:distance ,distance))))

(defun emdocs--create-edit (source transform)
  `(:op (:source ,source :transform (:type (:edit ,transform)))))

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
      (let ((delete-op (unless (zerop prev-length)
                         (emdocs--create-delete point prev-length)))
            (insert-op (unless (= beg end)
                         (->> (buffer-substring beg end)
                              (emdocs--create-insert point)))))
        (cl-assert (or delete-op insert-op) t "every change must be an insert, delete, or both")
        (cl-loop for op in `(,delete-op ,insert-op)
                 when op
                 do (->> op
                         (emdocs--create-edit source)
                         (emdocs--write-stdin)))))))

(defun emdocs--before-change-function (beg end)
  ;; TODO: set up undo list, etc
  )

(defun emdocs--setup-buffer-idempotent ()
  (unless emdocs-buffer-id
    (setq-local emdocs-buffer-id (uuid-string)))
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
        t)
    nil))

(provide 'emdocs)
;;; emdocs.el ends here
