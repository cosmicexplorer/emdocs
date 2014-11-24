;; asdfasdfasdf
;; asdfasdfasdf
;; asdfasdfasdf
;; RACECARS

;; (perform-patch-from-file "emacs_integration.el" "test_node_output")

;;; gives access to shell-command-minus-message function
(load-file "./shell-command-minus-message.el")

(defconst executable-directory "~/projects/emdocs/")

(defun send-buffer-to-file (buffer-name file-path)
  "Sends contents of buffer buffer-name to file identified by file-path."
  (with-current-buffer buffer-name
    (shell-command-minus-message
     (point-min) (point-max)
     (concat executable-directory "write_stdin_to_file.sh" " " ; space args
             "\"" file-path "\"")                              ; output file
     "*emdocs-out*" nil
     "*emdocs-err*" t)))

(defun read-buffer-from-file (file-name)
  "Replaces contents of buffer with file identified by file-name."
  (with-current-buffer file-name
    (let ((cur-point (point)))          ; save-excursion doesn't work with
                                        ; buffer replacement like this
      (shell-command-minus-message
       (point-min) (point-max)
       (concat "cat " file-name)
       t t                             ; output and replace current buffer
       "*emdocs-err*" t)
      (goto-char cur-point))))

(defun read-file-to-string (file-name)
  "Return stringified file."
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-string)))

(require 'json)

(defun perform-patch-from-file (buffer-to-patch file-path)
  "Parse output of google's diff_match_patch from file-path and apply it to
buffer buffer-to-patch. Assumes the patch is 'correct' in that it is able to be
applied without any errors."
  ;; TODO: make interactive
  (with-current-buffer buffer-to-patch
    (let ((json-object-type 'plist)
          (json-diff-struct nil)
          (cur-diff-set nil)
          (diff-vector nil))
      (setq json-diff-struct
            (json-read-from-string
             (read-file-to-string file-path)))
      (save-excursion
        (loop
         for diff-index from (1- (length json-diff-struct)) downto 0
         do (progn
              (setq cur-diff-set (aref json-diff-struct diff-index))
              (goto-char (1+ (plist-get cur-diff-set :start1)))
              (setq diff-vector (plist-get cur-diff-set :diffs))
              ;; simple iteration over the vector isn't working, so aref + index
              (loop for diff-pair-index from 0 upto (1- (length diff-vector))
                    do (perform-diff-operation
                        (aref diff-vector diff-pair-index)))))))))

;;; TODO: make more robust by allowing for non-matches with some best-guesses
(defun perform-diff-operation (diff-pair-vector)
  "Helper function which performs the diffing operation specified by a
two-element vector in a part of the JSON created by google's diff_match_patch."
  (let ((diff-op (aref diff-pair-vector 0))
        (diff-string (aref diff-pair-vector 1)))
    (cond
     ((equal diff-op 0)
      (forward-char (length diff-string)))
     ((equal diff-op 1)
      (insert diff-string))
     ((equal diff-op -1)
      (delete-forward-char (length diff-string)))
     (t
      (throw 'unrecognized-diff-op
             "diff_match_patch.patch_make operation not recognized")))))
