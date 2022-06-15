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
  (let ((line (format "%s\n" (json-encode x))))
    (process-send-string emdocs--process-name line)))

(defun emdocs--make-process ()
  (make-process
   :name emdocs--process-name
   :buffer emdocs--process-buffer-name
   :command '("/home/cosmicexplorer/projects/active/emdocs/target/debug/emdocs" "interact")
   :filter #'emdocs--interaction-filter
   :stderr (get-buffer-create emdocs--error-buffer-name)))

(defun emdocs--after-change-function (beg end prev-length)
  (let ((source (emdocs--create-buffer-id emdocs-buffer-id))
        (point (emdocs--create-point beg)))
    (->> (cond ((zerop prev-length)
                (->> (buffer-substring-no-properties beg end)
                     (emdocs--create-insert point)))
               ((= beg end)
                (emdocs--create-delete point prev-length))
               (t (error "should never get here")))
         (emdocs--create-edit source)
         (emdocs--write-stdin))))

(defun emdocs--before-change-function (beg end)
  ;; TODO: set up undo list, etc
  )

(defun emdocs--setup-buffer-idempotent ()
  (unless emdocs-buffer-id
    (setq-local emdocs-buffer-id (uuid-string)))
  (add-hook 'after-change-functions #'emdocs--after-change-function)
  (add-hook 'before-change-functions #'emdocs--before-change-function))


;; Interactive

;;;###autoload
(define-minor-mode emdocs-mode
  "???"
  :lighter " MDox"
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (if emdocs-mode
      (progn
        (emdocs--setup-buffer-idempotent)
        (unless (process-live-p (get-process emdocs--process-name))
          (emdocs--make-process))
        t)
    nil))

(provide 'emdocs)
;;; emdocs.el ends here
