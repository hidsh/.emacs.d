;;; evil-smartparens-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-smartparens" "evil-smartparens.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from evil-smartparens.el

(autoload 'evil-smartparens-mode "evil-smartparens" "\
Toggle evil-smartparens.

This is a minor mode.  If called interactively, toggle the
`Evil-Smartparens mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `evil-smartparens-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "evil-smartparens" '("evil-s"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-smartparens-autoloads.el ends here
