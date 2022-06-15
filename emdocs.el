;;; emdocs.el --- collaborative sharing of buffers -*- lexical-binding: t -*-

;; Author: Danny McClanahan
;; Version: 0.1
;; URL: https://github.com/cosmicexplorer/emdocs
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (dash "2.13.0") (helm "2.8.8"))
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


;;; Notes:

;;; This file is licensed under the GPL, not the AGPL as is the 'emdocs' binary.

(define-minor-mode emdocs-mode
  "???"
  :lighter " MDox"
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (if emdocs-mode
      t
    nil))

(provide 'emdocs)
;;; emdocs.el ends here
