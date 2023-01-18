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
;; I'd added color customization and flicker-less features to it.

;;; Customization

(defgroup flash-mode-line nil
  "Flash modeline with specified color."
  :group 'convinience)

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
    (run-with-timer 0 nil (lambda () (setq ring-bell-function nil)))                ;; rid of flickering
    (run-with-timer 0.25 nil (lambda (bg) (set-face-background 'mode-line bg))      ;; restore colors
                    orig-bg)
    (run-with-timer 0.5 nil (lambda () (setq ring-bell-function #'flash-mode-line-func))))) ;; enable myself again

(setq ring-bell-function #'flash-mode-line-func)

(provide 'flash-mode-line)

;;; flash-mode-line.el ends here
