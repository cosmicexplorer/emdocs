(defconst executable-directory "/home/cosmicexplorer/projects/CollabEmacs/")

(defun send-buffer-to-file (file-name suffix)
  (with-current-buffer file-name
    (when (= (point-max) 1)
      (newline))
    (shell-command-on-region
     (point-min) (point-max)
     (concat executable-directory "write_stdin_to_file.sh" " " ; space args
             "\"" file-name suffix "\"")                       ; output file
     "*emacs-docs-out*" nil
     "*emacs-docs-err*" t)))

(defun read-buffer-from-file (file-name)
  (with-current-buffer file-name
    (let ((cur-point (point)))          ; save-excursion doesn't work with
                                        ; buffer replacement like this
      (shell-command-on-region
       (point-min) (point-max)
       (concat "cat " file-name)
       t t                              ; output and replace current buffer
       "*emacs-docs-err*" t)
      (goto-char cur-point))))

(defun read-file-to-string (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-string)))

(require 'json)

(defun perform-patch-from-file (patch-file)
  "Parse output of google's diff_match_patch from a file and apply it to the
current buffer. Assumes the patch is 'correct' in that it is able to be applied
without any errors."
  (interactive)
  (let ((json-object-type 'plist)
        (json-diff-struct nil)
        (cur-diff-set nil)
        (diff-vector nil))
    (setq json-diff-struct
          (json-read-from-string (read-file-to-string patch-file)))
    (save-excursion
      (loop
       for diff-index from (1- (length json-diff-struct)) downto 0
       do (progn
            (setq cur-diff-set (aref json-diff-struct diff-index))
            (goto-char (1+ (plist-get cur-diff-set :start1)))
            (setq diff-vector (plist-get cur-diff-set :diffs))
            ;; simple iteration over the vector isn't working, so aref and index
            (loop for diff-pair-index from 0 upto (1- (length diff-vector))
                  do (perform-diff-operation
                      (aref diff-vector diff-pair-index))))))))

(defun perform-diff-operation (diff-pair-vector)
  "Helper function which performs the diffing operation specified by a
two-element vector created by a part of the JSON created by google's
diff_match_patch."
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
      (throw 'unrecognized-diff-op t)))))
