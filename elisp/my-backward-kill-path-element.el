;;; my-backward-kill-path-element.el
;;;
;;; for vertico / find-file, find-alternate-file, etc...
;;;

(setq my-backward-kill-path-element--bak "")

(defun my-backward-kill-path-element--clear-bak ()
  (setq my-backward-kill-path-element--bak ""))

(defun my-backward-kill-path-element--store-bak ()
  (setq my-backward-kill-path-element--bak
        (buffer-substring (minibuffer-prompt-end) (point-max))))

(defun my-backward-kill-path-element--region ()
  (if (>= (minibuffer-prompt-end) (point-max))
      nil    ;; do nothing
    (when (eq (char-before) ?/)
      (backward-char))
    (while (and (< (minibuffer-prompt-end) (point)) (not (eq (char-before) ?/)))
      (backward-char))
    (list (point) (point-max))))

(defun my-backward-kill-path-element ()
  (interactive)
  (let ((reg (my-backward-kill-path-element--region)))
    (when reg
      (let ((beg (first reg))
            (end (second reg)))
        (when (< beg end)
          (delete-region beg end))))))

(defun my-backward-kill-path-element--revert ()
  (interactive)
  (delete-region (minibuffer-prompt-end) (point-max))
  (insert my-backward-kill-path-element--bak))

(add-hook 'minibuffer-setup-hook #'my-backward-kill-path-element--store-bak)

;; (define-key vertico-map (kbd "M-h") 'my-backward-kill-path-element)
;; (define-key vertico-map (kbd "M-l") 'my-backward-kill-path-element--revert)

(provide 'my-backward-kill-path-element)
;; my-backward-kill-path-element.el ends here
