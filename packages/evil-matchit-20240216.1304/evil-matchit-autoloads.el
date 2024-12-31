;;; evil-matchit-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from evil-matchit.el

(autoload 'evilmi-jump-items-internal "evil-matchit" "\
Jump between items NUM times and apply function FUNC.

(fn NUM &optional FUNC)")
(autoload 'evilmi-jump-items-native "evil-matchit" "\
Jump between items NUM times.

(fn &optional NUM)" t)
(autoload 'evilmi-load-plugin-rules "evil-matchit" "\
Load MODES's plugin RULES.

(fn MODES RULES)")
(autoload 'evilmi-init-plugins "evil-matchit" "\
Load plugins." t)
(autoload 'evilmi-select-items "evil-matchit" "\
Select NUM items/tags and the region between them.

(fn &optional NUM)" t)
(autoload 'evilmi-delete-items "evil-matchit" "\
Delete NUM items/tags and the region between them.

(fn &optional NUM)" t)
(autoload 'evilmi-version "evil-matchit" "\
Print version." t)
(register-definition-prefixes "evil-matchit" '("evilmi-"))


;;; Generated autoloads from evil-matchit-c.el

(autoload 'evilmi-c-get-tag "evil-matchit-c" "\
Get tag at point.")
(autoload 'evilmi-c-jump "evil-matchit-c" "\
Use INFO to jump NUM times.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-c" '("evilmi-c-"))


;;; Generated autoloads from evil-matchit-cmake.el

(autoload 'evilmi-cmake-get-tag "evil-matchit-cmake")
(autoload 'evilmi-cmake-jump "evil-matchit-cmake" "\


(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-cmake" '("evilmi-cmake-"))


;;; Generated autoloads from evil-matchit-diff.el

(autoload 'evilmi-diff-get-tag "evil-matchit-diff" "\
Get tag at point.")
(autoload 'evilmi-diff-jump "evil-matchit-diff" "\
Jump to the matching tag using INFO and NUM.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-diff" '("evilmi-diff-"))


;;; Generated autoloads from evil-matchit-elixir.el

(autoload 'evilmi-elixir-get-tag "evil-matchit-elixir")
(autoload 'evilmi-elixir-jump "evil-matchit-elixir" "\


(fn RLT NUM)")
(register-definition-prefixes "evil-matchit-elixir" '("evilmi-elixir-"))


;;; Generated autoloads from evil-matchit-evil-setup.el

(autoload 'evilmi-jump-to-percentage "evil-matchit-evil-setup" "\
Like Vim %, NUM is the percentage of location.

(fn NUM)" t)
 (autoload 'evilmi-jump-items "evil-matchit" nil t)
(autoload 'evil-matchit-mode "evil-matchit-evil-setup" "\
Buffer-local minor mode to emulate matchit.vim.

This is a minor mode.  If called interactively, toggle the
`Evil-Matchit mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `evil-matchit-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{evil-matchit-mode-map}

(fn &optional ARG)" t)
(autoload 'turn-on-evil-matchit-mode "evil-matchit-evil-setup" "\
Enable the minor mode in the current buffer.")
(autoload 'turn-off-evil-matchit-mode "evil-matchit-evil-setup" "\
Disable the minor mode in the current buffer.")
(put 'global-evil-matchit-mode 'globalized-minor-mode t)
(defvar global-evil-matchit-mode nil "\
Non-nil if Global Evil-Matchit mode is enabled.
See the `global-evil-matchit-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-matchit-mode'.")
(custom-autoload 'global-evil-matchit-mode "evil-matchit-evil-setup" nil)
(autoload 'global-evil-matchit-mode "evil-matchit-evil-setup" "\
Toggle Evil-Matchit mode in all buffers.
With prefix ARG, enable Global Evil-Matchit mode if ARG is positive; otherwise,
disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Evil-Matchit mode is enabled in all buffers where `turn-on-evil-matchit-mode'
would do it.

See `evil-matchit-mode' for more information on Evil-Matchit mode.

(fn &optional ARG)" t)
(register-definition-prefixes "evil-matchit-evil-setup" '("evil"))


;;; Generated autoloads from evil-matchit-fortran.el

(autoload 'evilmi-fortran-get-tag "evil-matchit-fortran")
(autoload 'evilmi-fortran-jump "evil-matchit-fortran" "\


(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-fortran" '("evilmi-fortran-"))


;;; Generated autoloads from evil-matchit-html.el

(autoload 'evilmi-html-get-tag "evil-matchit-html" "\
Get current tag.")
(autoload 'evilmi-html-jump "evil-matchit-html" "\
Use INFO from current tag to jump NUM times.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-html" '("evilmi-html--"))


;;; Generated autoloads from evil-matchit-indent.el

(autoload 'evilmi-indent-get-tag "evil-matchit-indent" "\
Return a list containing start-position, tag-type, keyword.")
(autoload 'evilmi-indent-extract-keyword "evil-matchit-indent" "\
Extract keyword from LINE.

(fn LINE)")
(autoload 'evilmi-indent-jump "evil-matchit-indent" "\
Use INFO from `evilmi-indent-get-tag' to jump to matched tag.

(fn INFO)")
(register-definition-prefixes "evil-matchit-indent" '("evilmi-"))


;;; Generated autoloads from evil-matchit-javascript.el

(autoload 'evilmi-javascript-get-tag "evil-matchit-javascript" "\
Get tag at point.")
(autoload 'evilmi-javascript-jump "evil-matchit-javascript" "\
Jump to the matching tag using INFO and NUM.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-javascript" '("evilmi-"))


;;; Generated autoloads from evil-matchit-latex.el

(autoload 'evilmi-latex-get-tag "evil-matchit-latex")
(autoload 'evilmi-latex-jump "evil-matchit-latex" "\


(fn RLT NUM)")
(register-definition-prefixes "evil-matchit-latex" '("evilmi-latex-"))


;;; Generated autoloads from evil-matchit-markdown.el

(autoload 'evilmi-markdown-get-tag "evil-matchit-markdown" "\
Get current tag.  Return (list start-position tag).")
(autoload 'evilmi-markdown-jump "evil-matchit-markdown" "\
Jump to the next tag using INFO and NUM.

(fn INFO NUM)")


;;; Generated autoloads from evil-matchit-ocaml.el

(autoload 'evilmi-ocaml-get-tag "evil-matchit-ocaml" "\
Return information of current tag: (list position-of-word word).")
(autoload 'evilmi-ocaml-jump "evil-matchit-ocaml" "\


(fn RLT NUM)")
(register-definition-prefixes "evil-matchit-ocaml" '("evilmi-ocaml-"))


;;; Generated autoloads from evil-matchit-octave.el

(autoload 'evilmi-octave-get-tag "evil-matchit-octave" "\
Get current tag info.")
(autoload 'evilmi-octave-jump "evil-matchit-octave" "\
Use INFO returned by `evilmi-octave-get-tag' and NUM to jump to matched tag.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-octave" '("evilmi-octave-"))


;;; Generated autoloads from evil-matchit-org.el

(autoload 'evilmi-org-get-tag "evil-matchit-org" "\
Get current tag in org file.")
(autoload 'evilmi-org-jump "evil-matchit-org" "\
Jump to the matching tag using INFO and NUM.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-org" '("evilmi-"))


;;; Generated autoloads from evil-matchit-prog.el

(autoload 'evilmi-prog-get-tag "evil-matchit-prog" "\
Get tag at point.")
(autoload 'evilmi-prog-jump "evil-matchit-prog" "\
Use INFO to jump NUM times.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-prog" '("evilmi-prog-"))


;;; Generated autoloads from evil-matchit-python.el

(autoload 'evilmi-python-get-tag "evil-matchit-python" "\
Return a list containing start-position, tag-type, keyword.")
(autoload 'evilmi-python-jump "evil-matchit-python" "\
Use INFO from `evilmi-python-get-tag' to jump to matched tag.
NUM is ignored.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-python" '("evilmi-python-"))


;;; Generated autoloads from evil-matchit-ruby.el

(autoload 'evilmi-ruby-get-tag "evil-matchit-ruby" "\
Get tag at point.")
(autoload 'evilmi-ruby-jump "evil-matchit-ruby" "\
Use INFO to jump NUM times.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-ruby" '("evilmi-ruby-"))


;;; Generated autoloads from evil-matchit-script.el

(autoload 'evilmi-script-get-tag "evil-matchit-script" "\
Get tag at point.")
(autoload 'evilmi-script-jump "evil-matchit-script" "\
Use INFO returned by `evilmi-script-get-tag' and NUM to jump to matched tag.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-script" '("evilmi-script-"))


;;; Generated autoloads from evil-matchit-sdk.el

(autoload 'evilmi-sdk-curline "evil-matchit-sdk" "\
Get current line text.")
(autoload 'evilmi-sdk-member "evil-matchit-sdk" "\
Check if KEYWORD exist in KEYWORD-LIST.

(fn KEYWORD KEYWORD-LIST)")
(autoload 'evilmi-sdk-get-tag-info "evil-matchit-sdk" "\
Return (row column is-function-exit-point keyword).
The row and column mark the position in `evilmi-mylang-match-tags'
is-function-exit-point could be unknown status

(fn KEYWORD MATCH-TAGS)")
(autoload 'evilmi-sdk-get-tag "evil-matchit-sdk" "\
Use MATCH-TAGS and HOWTOS to return information for jump.

(fn MATCH-TAGS HOWTOS)")
(autoload 'evilmi-sdk-jump "evil-matchit-sdk" "\
Use RLT, NUM, MATCH-TAGS and HOWTOS to jump.
Return nil if no matching tag found.  Please note (point) is changed
after calling this function.

(fn RLT NUM MATCH-TAGS HOWTOS)")
(autoload 'evilmi-sdk-font-p "evil-matchit-sdk" "\
If current font at POS is among FONTS.

(fn POS FONTS)")
(autoload 'evilmi-sdk-semantic-flex "evil-matchit-sdk" "\
Using the syntax table, do something roughly equivalent to flex.
Semantically check between START and END.  Optional argument DEPTH
indicates at what level to scan over entire lists.
The return value is a token stream.  Each element is a list, such of
the form (symbol start-expression .  end-expression) where SYMBOL
denotes the token type.
END does not mark the end of the text scanned, only the end of the
beginning of text scanned.  Thus, if a string extends past END, the
end of the return token will be larger than END.  To truly restrict
scanning, use `narrow-to-region'.
The last argument, LENGTH specifies that only LENGTH tokens are returned.

(fn START END &optional DEPTH LENGTH)")
(autoload 'evilmi-sdk-tokens "evil-matchit-sdk" "\
Get semantic tokens of current N lines.

(fn N)")
(register-definition-prefixes "evil-matchit-sdk" '("evilmi-"))


;;; Generated autoloads from evil-matchit-sh.el

(autoload 'evilmi-sh-get-tag "evil-matchit-sh")
(autoload 'evilmi-sh-jump "evil-matchit-sh" "\


(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-sh" '("evilmi-sh-"))


;;; Generated autoloads from evil-matchit-simple.el

(autoload 'evilmi-simple-get-tag "evil-matchit-simple" "\
Get current tag in simple language.")
(autoload 'evilmi-simple-jump "evil-matchit-simple" "\
Use INFO of current tag to jump to matching tag.  NUM is ignored.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-simple" '("evilmi-"))


;;; Generated autoloads from evil-matchit-sql.el

(autoload 'evilmi-sql-get-tag "evil-matchit-sql" "\
Get tag at point.")
(autoload 'evilmi-sql-jump "evil-matchit-sql" "\
Use INFO returned by `evilmi-sql-get-tag' and NUM to jump to matched tag.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-sql" '("evilmi-sql-"))


;;; Generated autoloads from evil-matchit-template.el

(autoload 'evilmi-template-get-tag "evil-matchit-template" "\
Get tag at point.")
(autoload 'evilmi-template-jump "evil-matchit-template" "\
Jump to the matching tag using INFO and NUM.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-template" '("evilmi-template-"))


;;; Generated autoloads from evil-matchit-terminal.el

(autoload 'evilmi-prompt-line-p "evil-matchit-terminal" "\
If line at POSITION has prompt at the beginning.

(fn &optional POSITION)")
(autoload 'evilmi-terminal-get-tag "evil-matchit-terminal" "\
Get tag at point.")
(autoload 'evilmi-terminal-jump "evil-matchit-terminal" "\
Use INFO to jump NUM times.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-terminal" '("evilmi-terminal-p"))


;;; Generated autoloads from evil-matchit-verilog.el

(autoload 'evilmi-verilog-get-tag "evil-matchit-verilog" "\
Get tag at point.")
(autoload 'evilmi-verilog-jump "evil-matchit-verilog" "\
Use INFO returned by `evilmi-verilog-get-tag' and NUM to jump to matched tag.

(fn INFO NUM)")
(register-definition-prefixes "evil-matchit-verilog" '("evilmi-verilog-"))


;;; Generated autoloads from evil-matchit-yaml.el

(autoload 'evilmi-yaml-get-tag "evil-matchit-yaml" "\
Return a list containing start-position, tag-type, keyword.")
(autoload 'evilmi-yaml-jump "evil-matchit-yaml" "\
Use INFO returned by `evilmi-yaml-get-tag' and NUM to jump to matched tag.

(fn INFO NUM)")

;;; End of scraped data

(provide 'evil-matchit-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; evil-matchit-autoloads.el ends here