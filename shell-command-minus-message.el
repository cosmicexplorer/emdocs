(defun shell-command-minus-message (start end command
                                          &optional output-buffer replace
                                          error-buffer display-error-buffer)
  (interactive (let (string)
		 (unless (mark)
		   (error "The mark is not set now, so there is no region"))
		 ;; Do this before calling region-beginning
		 ;; and region-end, in case subprocess output
		 ;; relocates them while we are in the minibuffer.
		 (setq string (read-shell-command "Shell command on region: "))
		 ;; call-interactively recognizes region-beginning and
		 ;; region-end specially, leaving them in the history.
		 (list (region-beginning) (region-end)
		       string
		       current-prefix-arg
		       current-prefix-arg
		       shell-command-default-error-buffer
		       t)))
  (let ((error-file
	 (if error-buffer
	     (make-temp-file
	      (expand-file-name "scor"
				(or small-temporary-file-directory
				    temporary-file-directory)))
	   nil))
	exit-status)
    (if (or replace
	    (and output-buffer
		 (not (or (bufferp output-buffer) (stringp output-buffer)))))
	;; Replace specified region with output from command.
	(let ((swap (and replace (< start end))))
	  ;; Don't muck with mark unless REPLACE says we should.
	  (goto-char start)
	  (and replace (push-mark (point) 'nomsg))
	  (setq exit-status
		(call-process-region start end shell-file-name replace
				     (if error-file
					 (list t error-file)
				       t)
				     nil shell-command-switch command))
	  ;; It is rude to delete a buffer which the command is not using.
	  ;; (let ((shell-buffer (get-buffer "*Shell Command Output*")))
	  ;;   (and shell-buffer (not (eq shell-buffer (current-buffer)))
	  ;; 	 (kill-buffer shell-buffer)))
	  ;; Don't muck with mark unless REPLACE says we should.
	  (and replace swap (exchange-point-and-mark)))
      ;; No prefix argument: put the output in a temp buffer,
      ;; replacing its entire contents.
      (let ((buffer (get-buffer-create
		     (or output-buffer "*Shell Command Output*"))))
	(unwind-protect
	    (if (eq buffer (current-buffer))
		;; If the input is the same buffer as the output,
		;; delete everything but the specified region,
		;; then replace that region with the output.
		(progn (setq buffer-read-only nil)
		       (delete-region (max start end) (point-max))
		       (delete-region (point-min) (min start end))
		       (setq exit-status
			     (call-process-region (point-min) (point-max)
						  shell-file-name t
						  (if error-file
						      (list t error-file)
						    t)
						  nil shell-command-switch
						  command)))
	      ;; Clear the output buffer, then run the command with
	      ;; output there.
	      (let ((directory default-directory))
		(with-current-buffer buffer
		  (setq buffer-read-only nil)
		  (if (not output-buffer)
		      (setq default-directory directory))
		  (erase-buffer)))
	      (setq exit-status
		    (call-process-region start end shell-file-name nil
					 (if error-file
					     (list buffer error-file)
					   buffer)
					 nil shell-command-switch command)))
	  ;; Report the output.
	  (with-current-buffer buffer
	    (setq mode-line-process
		  (cond ((null exit-status)
			 " - Error")
			((stringp exit-status)
			 (format " - Signal [%s]" exit-status))
			((not (equal 0 exit-status))
			 (format " - Exit [%d]" exit-status)))))
	  (if (with-current-buffer buffer (> (point-max) (point-min)))
	      ;; There's some output, display it
	      (display-message-or-buffer buffer)
	    ;; No output; error?
	    (let ((output
		   (if (and error-file
			    (< 0 (nth 7 (file-attributes error-file))))
		       (format "some error output%s"
			       (if shell-command-default-error-buffer
				   (format " to the \"%s\" buffer"
					   shell-command-default-error-buffer)
				 ""))
		     "no output"))))
	    ;; Don't kill: there might be useful info in the undo-log.
	    ;; (kill-buffer buffer)
	    ))))

    (when (and error-file (file-exists-p error-file))
      (if (< 0 (nth 7 (file-attributes error-file)))
	  (with-current-buffer (get-buffer-create error-buffer)
	    (let ((pos-from-end (- (point-max) (point))))
	      (or (bobp)
		  (insert "\f\n"))
	      ;; Do no formatting while reading error file,
	      ;; because that can run a shell command, and we
	      ;; don't want that to cause an infinite recursion.
	      (format-insert-file error-file nil)
	      ;; Put point after the inserted errors.
	      (goto-char (- (point-max) pos-from-end)))
	    (and display-error-buffer
		 (display-buffer (current-buffer)))))
      (delete-file error-file))
    exit-status))
