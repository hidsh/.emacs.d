;; https://chatgpt.com/c/68b61029-f294-832b-b81f-a1bd63213344
(require 'cl-lib)

(defvar c-macrostep-overlays nil
  "Overlays currently used for C macro expansions.")

(defun c-macrostep--call-cpp (text)
  "Run cpp on TEXT and return expanded string."
  (with-temp-buffer
    (insert text)
    (let ((temp-file (make-temp-file "c-macrostep" nil ".c")))
      (write-region (point-min) (point-max) temp-file nil 'silent)
      (with-temp-buffer
        (unless (zerop (call-process "cpp" nil t nil "-P" temp-file))
          (error "cpp failed"))
        (buffer-string)))))

(defun c-macrostep-expand-at-point ()
  "Expand the C macro at point using cpp and show it inline."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (line (thing-at-point 'line t))
         (expanded (c-macrostep--call-cpp line)))
    (let ((ov (make-overlay (car bounds) (cdr bounds))))
      (overlay-put ov 'c-macrostep t)
      (overlay-put ov 'after-string
                   (concat " ⇨ " (string-trim expanded)))
      (push ov c-macrostep-overlays))))

(defun c-macrostep-collapse-all ()
  "Remove all macrostep overlays."
  (interactive)
  (mapc #'delete-overlay c-macrostep-overlays)
  (setq c-macrostep-overlays nil))

