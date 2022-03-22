(defun error-cycle (&optional reverse)
  "Move to next error if it's exists.
If it wasn't exists then move to previous error.
Move to previous error if REVERSE is non-nil."
  (interactive)
  (error-cycle-1
   (error-tip-collect-current-file-errors flycheck-current-errors) reverse))

(defun error-cycle-reverse ()
    "Do `error-cycle by reverse order."
    (interactive)
    (error-cycle t))

(defun error-cycle-1 (errors &optional reverse)
  ;; (error-tip-delete-popup)
  (setq error-tip-state nil)
  (when errors
    (let*
        ((next     (assoc-default :next         errors))
         (previous (assoc-default :previous     errors))
         (cur-line (assoc-default :current-line errors))
         (jump (lambda (errs)
                 ;; Set errors forcefully at EOB
                 (when (and (eobp) (eq (point) (point-at-bol)))
                   (setq errs previous))
                 (goto-char (point-min))
                 (unless (line-move (1- (error-tip-get (car errs) 'line)) t)
                   (push (cons 'eob (line-number-at-pos)) error-tip-state))
                 (setq error-tip-current-errors errs)
                 ;; (if (null error-tip-timer-delay)
                 ;;     (error-tip-popup-error-message (error-tip-get-errors))
                 ;;   (error-tip-cancel-timer)
                 ;;   (error-tip-register-timer))))
         (target (if (not reverse)
                     (or next previous cur-line)
                   (reverse (or previous next cur-line)))))
      (funcall jump target))))

(defun error-tip-get (err element)
  (when (fboundp 'flycheck-tip--get)
    (flycheck-tip--get element err)))

(defun error-tip-collect-current-file-errors (errors)
  "Collect errors from ERRORS."
  (cl-loop with c-line = (line-number-at-pos (point))
           for err in errors
           for err-line = (error-tip-get err 'line)
           if (and buffer-file-truename ; whether file or buffer
                   (not (equal (expand-file-name buffer-file-truename)
                               (error-tip-get err 'file))))
           do '() ; skip
           else if (< c-line err-line)
           collect err into next
           else if (> c-line err-line)
           collect err into previous
           else if (= c-line err-line)
           collect err into current-line
           finally return (when (or next previous current-line)
                            (list (cons :next         next)
                                  (cons :previous     previous)
                                  (cons :current-line current-line)))))
