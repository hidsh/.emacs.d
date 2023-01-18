;;; flash-mode-line.el --- Flash modeline with specified color

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program is fully based on topic at https://www.emacswiki.org/emacs/AlarmBell
;; I'd added customization and flicker-less features to it.

;;; Customization

(defgroup flash-mode-line nil
  "Flash modeline with specified color."
  :group 'convinience)

(defcustom flash-mode-line-time 0.25
  "Flashing time in second."
  :type 'number
  :group 'flash-mode-line)

(defcustom flash-mode-line-color "yellow"
  "Color to flash modeline.
It can also be used hexadecimal like \"#ff0000\""
  :type 'color
  :group 'flash-mode-line)

;;; Code:

(defun flash-mode-line-func ()
  "Flash modeline with specified color."
  (let ((orig-bg (face-background 'mode-line)))
    (set-face-background 'mode-line flash-mode-line-color)
    (run-with-timer 0 nil                                           ;; get rid of flickering
                    (lambda () (setq ring-bell-function nil)))
    (run-with-timer flash-mode-line-time nil                        ;; restore colors
                    (lambda (bg) (set-face-background 'mode-line bg))
                    orig-bg)
    (run-with-timer (+ flash-mode-line-time 0.5) nil                ;; enable myself again
                    (lambda () (setq ring-bell-function #'flash-mode-line-func)))))

(setq ring-bell-function #'flash-mode-line-func)

(provide 'flash-mode-line)

;;; flash-mode-line.el ends here
