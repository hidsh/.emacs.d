;; macrostep-c-mode.el
;; https://chatgpt.com/c/68b61029-f294-832b-b81f-a1bd63213344

(require 'cl-lib)

(defvar-local macrostep-c--overlays nil
  "List of overlays currently active in `macrostep-c-mode`.")

(defun macrostep-c--call-cpp (text)
  "Run cpp on TEXT and return expanded string."
  (with-temp-buffer
    (insert text)
    (let ((temp-file (make-temp-file "macrostep-c" nil ".c")))
      (write-region (point-min) (point-max) temp-file nil 'silent)
      (with-temp-buffer
        (unless (zerop (call-process "cpp" nil t nil "-P" temp-file))
          (error "cpp failed"))
        (buffer-string)))))

(defun macrostep-c-expand ()
  "Expand the C macro at point and show it inline."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (line (thing-at-point 'line t))
         (expanded (macrostep-c--call-cpp line)))
    (when bounds
      (let ((ov (make-overlay (car bounds) (cdr bounds))))
        (overlay-put ov 'macrostep-c t)
        (overlay-put ov 'after-string
                     (concat " ⇨ " (string-trim expanded)))
        (push ov macrostep-c--overlays)))))


(defun macrostep-c-collapse-all ()
  "Remove all macrostep overlays."
  (interactive)
  (mapc #'delete-overlay macrostep-c--overlays)
  (setq macrostep-c--overlays nil))

(defun macrostep-c-quit ()
  "Quit `macrostep-c-mode` and remove all overlays."
  (interactive)
  (macrostep-c-collapse-all)
  (macrostep-c-mode -1))

(defvar macrostep-c-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") #'macrostep-c-expand)
    (define-key map (kbd "C-c C-c") #'macrostep-c-collapse-all)
    (define-key map (kbd "C-c C-q") #'macrostep-c-quit)
    map)
  "Keymap for `macrostep-c-mode`.")

;;;###autoload
(define-minor-mode macrostep-c-mode
  "Minor mode for interactively expanding C macros inline, similar to `macrostep-mode`."
  :lighter " µC"
  :keymap macrostep-c-mode-map
  (if macrostep-c-mode
      (message "macrostep-c-mode enabled: press e to expand, q to quit")
    (macrostep-c-collapse-all)))
