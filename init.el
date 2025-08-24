;;; init.el --- my init  -*- coding:utf-8-unix; mode:emacs-lisp -*-

;;; Commentary:

;; init.el a.k.a my BONSAI thing XD

;; (setq debug-on-error t)

;; suppress to popup *warnings* buffer for emacs28's native-comp
(setq native-comp-async-report-warnings-errors nil)

(add-to-list 'load-path (locate-user-emacs-file "elisp"))
(setq custom-theme-directory (locate-user-emacs-file "themes"))

;; to hide message "ad-handle-definition: `vc-revert' got redefined"
(setq ad-redefinition-action 'accept)

;; ignore `(require 'cl)'
(setq byte-compile-warnings '(not cl-functions obsolete))


;; ----------------------------------------------------------------------
;; my-elisp
(require 'discrete)

;; check-emacs-setting
;; (require 'check-emacs-setting)
;; (setq check-emacs-setting-files '("~/.emacs.d/init.el"
;;                                   "~/.emacs.d/elisp/discrete.el"
;;                                   "~/.emacs.d/elisp/_mac.el"
;;                                   "~/.emacs.d/elisp/_ubuntu.el"
;;                                   "~/.emacs.d/elisp/_windows.el"))

;; ----------------------------------------------------------------------
;; save sessions
;; (setq desktop-auto-save-timeout nil)
;; (with-no-message
;;   (desktop-save-mode 1))

;; ----------------------------------------------------------------------
(defun mycolor (name)
  (let ((colors '((white       . "#f9f9f9")
                  (light-gray  . "#a4a2a2")
                  (gray        . "#7c7a7a")
                  (dark-gray   . "#555555")
                  (dark-gray2  . "#3e3e3e")
                  (black       . "#000000")
                  (red         . "#ff6b7f")
                  (blue        . "#61afef")
                  (dark-blue2  . "#1684DF")
                  (dark-blue   . "#126EBA")
                  (dark-blue3  . "#0F5895")
                  (green       . "#98be65")
                  (pink        . "#eb7bc0")
                  (purple      . "#c678dd")
                  (orange      . "#e3b23c")
                  ;; (charcoal . "#3d363e"))))
                  ;; (charcoal . "#362f37")))
                  (charcoal    . "#2b262c"))))
    (cdr (assoc name colors))))

;; e.g. (mycolor 'red) => "#ff6b7f"

(defun myfont (type)
  (let* ((fonts '((default  . "Source Han Code JP N")
                  (default2 . "Consolas")
                  (default3 . "Cica")
                  (nerdfont . "ShureTechMono Nerd Font Mono")
                  (ui       . "x14y24pxHeadUpDaisy")
                  (ui2      . "Krungthep")
                  (ui3      . "Squarea")
                  (posframe . "UbuntuMono Nerd Font Mono")
                  ))
         (name (cdr (assoc type fonts))))
    (if window-system
        (if (x-list-fonts name)
            name
          (progn
            (message (format "ERROR: Font not found: %s" name))
            nil))
      (unless noninteractive
        ;; NOT occurs error in batch mode (= while checking)
        (message "ERROR: Specifying font can only work under any window-system."))
      nil)))

;; e.g. (myfont 'default) => "Source Han Code JP N"

;; ----------------------------------------------------------------------
; host independent
(require
 (pcase system-type
   ('windows-nt '_windows)
   ('gnu/linux  '_linux)
   ('darwin     '_mac)
   (_ (user-error "Unknown system-type: %s" system-type))))

;; (my-load-frame)

;; ----------------------------------------------------------------------
;; defaults
(setq-default
 inhibit-startup-screen t                         ; Disable start-up screen
 auto-window-vscroll nil                          ; Lighten vertical scroll
 ;; confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 delete-by-moving-to-trash t                      ; Delete files to trash
 display-time-default-load-average nil            ; Don't display load average
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
 indent-tabs-mode nil                             ; Stop using tabs to indent
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 left-margin-width 1 right-margin-width 1         ; Add left and right margins
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally

 bidi-display-reordering nil                      ; 右から左に読む言語に対応させないことで描画を高速化
 vc-follow-symlinks t
 ring-bell-function 'ignore
 parens-require-spaces nil
 transient-mark-mode nil
 tab-width 4
 tab-stop-list nil
 comment-column 60

 ;; display-line-numbers-grow-only t
 ;; display-line-numbers-width-start 10
 ;; line-number-display-width 10
 ;; display-line-numbers-width 4

 ;; 1行スクロール
 ;; (setq scroll-conservatively most-positive-fixnum)
 scroll-margin 3
 next-screen-context-lines 3
 scroll-preserve-screen-position t

 next-line-add-newlines nil                  ; バッファ末尾に余計な改行コードを防ぐための設定
 idle-update-delay 0.3

 indicate-unused-lines t                     ; 左フランジにEOFがわかるように
 electric-pair-mode nil

 ;;
 ;; backup files
 ;; https://masutaka.net/chalow/2014-05-11-1.html
 ;; http://yohshiy.blog.fc2.com/blog-entry-319.html
 ;;
 ;; backup to `hoge.txt~'
 backup-directory-alist '((".*" . "~/.Trash"))      ;; todo windows
 version-control     t  ;; 番号付けによる複数保存 存実行の有無
 kept-new-versions   5  ;;                   最新の保持数
 kept-old-versions   1  ;;                   最古の保持数
 delete-old-versions t  ;;                   範囲外を削除

 ;; backup to `#hoge.txt#'
 auto-save-file-name-transforms '(("~/\\([^/]*/\\)*\\([^/]*\\)$" "~/.Trash/\\2" t))         ;; todo windows
                                        ;             '((".*" "~/.Trash" t))

 auto-save-default nil                  ; disabled

 ;; backup to `~/.emacs.d/auto-save-list/.saves-xxxx'
 auto-save-list-file-prefix nil         ; disabled

 ;; lock file to `.#hoge'
 create-lockfiles nil                   ; disabled

 ;; isearch-wrap-pause nil                 ; never wrap like evil-seach-forward/backward

 ) ;; setq-default

;; ----------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(tool-bar-mode -1)
;; (scroll-bar-mode -1)
(menu-bar-mode 0)                                 ; Disable the menu bar
(add-hook 'focus-out-hook #'garbage-collect)
(electric-indent-mode)
(which-function-mode -1)

(setq cursor-type 'box)
(blink-cursor-mode 0)

;; margin
(setq-default left-margin-width 0 right-margin-width 0) ; Define new widths.
(set-window-buffer nil (current-buffer))                ; Use them now.

;; save-place
(setq save-place-file "~/.emacs.d/.emacs-places")
(save-place-mode 1)                                     ; Enable save-place

;; ミニバッファの履歴を保存する
(savehist-mode 1)
(setq history-length 1000)
(setq history-delete-duplicates t)

(global-auto-revert-mode -1)                            ; disable auto-revert-mode
(setq indent-line-function 'indent-relative-maybe)

;; mode-line
(column-number-mode t)
;; ;; モードラインの割合表示を総行数表示に
;; (setq mode-line-position '(:eval (format "%%3c:%%l/%d"
;;                                          (count-lines (point-max) (point-min)))))

;; タイトルバーにファイルのフルパス表示
(defmacro replace-home-directory-string (file-name)
  `(if ,file-name
       (let ((regexp (cond ((eq system-type 'windows-nt) "^C:\\Users\\[^\\]\\+")
                           ((eq system-type 'gnu/linux)  "^/home/[^/]+/")
                           (t                            "^/Users/[^/]+/"))))
         (replace-regexp-in-string regexp "~/" ,file-name))
     nil))

;; (defun emacs-version-briefly ()
;;   (let ((lst (split-string (emacs-version))))
;;     (concat (nth 1 lst) (nth 2 lst))))

(setq frame-title-format '(format "%s"
                                  (:eval (or (replace-home-directory-string (buffer-file-name))
                                             (buffer-name)))))

(set-face-attribute 'help-key-binding nil :inherit 'default :height 0.9 :box nil :inverse-video t
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

(set-face-background 'region "#0F5895")

;; =====================================================================
;; key unbinding / binding
(keyboard-translate ?\C-h ?\C-?)                        ; c-h

(global-unset-key (kbd "M-,"))                          ; xref
;; (global-unset-key (kbd "M-."))                          ; xref
(global-unset-key (kbd "C-z"))                          ; suspend-frame
(global-unset-key (kbd "C-x C-x"))                      ; exchange-point-and-mark
(global-unset-key (kbd "C-x C-z"))                      ; suspend-frame
(global-unset-key (kbd "C-x o"))                        ; other-window
(global-unset-key (kbd "C-x m"))                        ; compose-mail
(global-unset-key (kbd "M-t"))                          ; transpose-word
(global-unset-key (kbd "M-'"))                          ; abbrev-prefix-mark
(global-unset-key (kbd "M-c"))                          ; capitalize-word     why also assigned to M-RET ??
(global-unset-key (kbd "M-i"))                          ; tab-to-tab-stop
(global-unset-key [f11])                                ; toggle-frame-fullscreen
(global-unset-key [f12])                                ; "M-c"
(global-unset-key (kbd "C-M-e"))                        ; end-of-defun

;; (global-set-key "(" 'my-insert-paren)                   ; ()
;; (global-set-key "{" 'my-insert-brace)                   ; {}
;; (global-set-key "[" 'my-insert-bracket)                 ; []
;; (global-set-key "<" 'my-insert-angle)                   ; <>
;; (global-set-key "'" 'my-insert-squote)                  ; ''
;; (global-set-key "\"" 'my-insert-dquote)                 ; ""

(global-set-key (kbd "C-m") 'newline-and-indent)             ; Returnキーで改行＋オートインデント
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'my-split-window-below)
(global-set-key (kbd "C-3") 'my-split-window-right)
(global-set-key (kbd "C-o") 'other-window)

(global-set-key (kbd "M-9") 'insert-parentheses)
(global-set-key (kbd "M-[") 'my-insert-brace2)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-P") 'beginning-of-buffer)
(global-set-key (kbd "M-N") 'end-of-buffer)

(global-set-key (kbd "C-x t") 'revert-buffer)
(global-set-key (kbd "C-x C-t") 'toggle-truncate-lines)
(global-set-key (kbd "C-x n f") 'narrow-to-defun)

(define-key isearch-mode-map (kbd "C-b") 'isearch-delete-char)

(keymap-set minibuffer-local-map "C-p" #'previous-history-element)
(keymap-set minibuffer-local-map "C-n" #'next-history-element)

;; =====================================================================

(setq truncate-partial-width-windows nil)
(setq-default truncate-lines t)

;; kill-ringに同じ内容を重複して入れない
(defadvice kill-new (before ys:no-kill-new-duplication activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

;; prevent annoying message "Text is read only" at mimibuffer
(plist-put minibuffer-prompt-properties
           'point-entered 'minibuffer-avoid-prompt)

;; enable completion in `eval-expression' (M-:)
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

(defun indent-or-insert-tab ()
  (interactive)
  (let ((pos (point)))
    (funcall indent-line-function)
    (when (= pos (point))
      (insert "\t"))))

(global-set-key "\C-i" 'indent-or-insert-tab)

(defun my-func ()
  "called \'my-func\'")

(global-set-key [f2] #'(lambda () (interactive) (message "%S" (funcall 'my-func))))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom. If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance. If there is more than one, they won't work right.
;;  '(hl-line ((t (:background "#141619")))))

;; ;; (global-hl-line-mode 1)

;; ----------------------------------------------------------------------
;; command aliases
(defalias 'a 'my-consult-apropos-symbol-at-point)
(defalias 'reb 're-builder)

(defalias 'dm 'describe-mode)
(defalias 'dv 'describe-variable)
(defalias 'dfun 'describe-function)
(defalias 'face 'describe-face)
(defalias 'dk 'describe-key)

(defalias 'l 'display-line-numbers-mode)
(defalias 'hl 'hl-line-mode)
(defalias 'calc 'quick-calc)
(defalias 'package-uninstall 'package-delete)
(defalias 'package-list-update 'package-refresh-contents)

;; ----------------------------------------------------------------------
;; (defvar my-face-adj-line-number-height 1.0)     ;; set by _mac.el or _windows.el or ...

(defun my-adv-load-theme--font-change (&rest _)
 (let ((font (myfont 'ui)))
   (when font
     (set-face-attribute 'mode-line          nil :family font :height my-face-adj-mode-line-height)     ;; defined at _mac.el or _windows.el
     (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)
     (set-face-attribute 'minibuffer-prompt  nil :family font)

     (set-face-attribute 'line-number              nil :family font :height my-face-adj-line-number-height)
     (set-face-attribute 'line-number-current-line nil :family font :height my-face-adj-line-number-height))))

(advice-add 'load-theme :after #'my-adv-load-theme--font-change)

;; ----------------------------------------------------------------------
;; appearance
;; ----------------------------------------------------------------------
;; (set-face-foreground 'vertical-border "red") ;; by theme
(setq window-divider-default-right-width 1)
(window-divider-mode 1)

;; ----------------------------------------------------------------------
;; (use-package messages-buffer-mode
;;   :config

  ;; Can not use `use-package' "Error (use-package): Cannot load messages-buffer-mode"

  ;; font lock
  (defvar message-buffer-font-lock-defaults
    `((( "\"\\.\\*\\?"                                  . font-lock-string-face)
       ( "^Quit"   . font-lock-constant-face))))      ;; case insensitive
  (defun my-hook--message-buffer-font-lock ()
    (message "my-hook--message-buffer-font-lock")
    (setq font-lock-defaults message-buffer-font-lock-defaults))

  (add-hook 'messages-buffer-mode-hook #'my-hook--message-buffer-font-lock)

  ;; auto scroll for *message* buffer
  ;; https://stackoverflow.com/questions/4682033/in-emacs-can-i-set-up-the-messages-buffer-so-that-it-tails
  (defun modi/messages-auto-tail (&rest _)
    "Make *Messages* buffer auto-scroll to the end after each message."
    (let* ((buf-name "*Messages*")
           ;; Create *Messages* buffer if it does not exist
           (buf (get-buffer-create buf-name)))
      ;; Activate this advice only if the point is _not_ in the *Messages* buffer
      ;; to begin with. This condition is required; otherwise you will not be
      ;; able to use `isearch' and other stuff within the *Messages* buffer as
      ;; the point will keep moving to the end of buffer :P
      (when (not (string= buf-name (buffer-name)))
        ;; Go to the end of buffer in all *Messages* buffer windows that are
        ;; *live* (`get-buffer-window-list' returns a list of only live windows).
        (dolist (win (get-buffer-window-list buf-name nil :all-frames))
          (with-selected-window win
            (goto-char (point-max))))
        ;; Go to the end of the *Messages* buffer even if it is not in one of
        ;; the live windows.
        (with-current-buffer buf
          (goto-char (point-max))))))

  (advice-add 'message :after #'modi/messages-auto-tail)
;; )


;; ----------------------------------------------------------------------
;; package setting
;; ----------------------------------------------------------------------
(eval-and-compile
  (setq package-user-dir "~/.emacs.d/packages")
  (customize-set-variable
   'package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (require 'diminish)                ;; if you use :diminish
  (require 'bind-key)                ;; if you use any :bind variant
  )

;; ----------------------------------------------------------------------
;; ここから use-package
;; ----------------------------------------------------------------------
(use-package cl-lib
  :init
  (setq byte-compile-warnings '(not cl-functions obsolete))     ;; temporary

  :config
  (eval-when-compile (require 'cl)) ;; for `lexical-let' macro
  (defun compose-cl (f g)
    (lexical-let ((f f)
                  (g g))
      (lambda (x)
        (funcall f (funcall g x)))))
)

;; ----------------------------------------------------------------------
(use-package my-doom-material-theme
  :load-path "~/.emacs.d/themes"
  :if window-system
  :config
  (defface my-evil-normal-tag-face `((t (:inherit default) :weight bold)) "")
  (copy-face 'my-evil-normal-tag-face 'my-evil-emacs-tag-face)
  (copy-face 'my-evil-normal-tag-face 'my-evil-insert-tag-face)
  (copy-face 'my-evil-normal-tag-face 'my-evil-motion-tag-face)
  (copy-face 'my-evil-normal-tag-face 'my-evil-visual-tag-face)
  (copy-face 'my-evil-normal-tag-face 'my-evil-operator-tag-face)
  ;; (defface my-evil-emacs-tag-face `((t (:inherit my-evil-normal-tag-face))) "")
  ;; (defface my-evil-insert-tag-face `((t (:inherit my-evil-normal-tag-face))) "")
  ;; (defface my-evil-motion-tag-face `((t (:inherit my-evil-normal-tag-face))) "")
  ;; (defface my-evil-visual-tag-face `((t (:inherit my-evil-normal-tag-face))) "")
  ;; (defface my-evil-operator-tag-face `((t (:inherit my-evil-normal-tag-face))) "")

  (load-theme 'my-doom-material t)
  )

;; ----------------------------------------------------------------------
(use-package mood-line
  :after my-doom-material-theme
  :load-path "elisp/mood-line"      ;; clone from gitlab
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code)
  ;; (mood-line-glyph-alist mood-line-glyphs-unicode)

  :config
  (mood-line-mode)

  (defface my-mode-line-accent-face  `((t (:foreground ,(face-background 'my-evil-normal-tag-face)))) "")

  (defvar mode-line-selected-window nil)

  (defun mode-line-record-selected-window ()
    (unless (string-match-p " \*Minibuf" (buffer-name (window-buffer (car (window-list)))))
      (setq mode-line-selected-window (selected-window))))

  (add-hook 'window-configuration-change-hook #'mode-line-record-selected-window)
  [ ---------------------- comment begin -------------------------------------------------------

  (defun mode-line-update-all ()
    (force-mode-line-update t))

  (add-hook 'buffer-list-update-hook #'mode-line-update-all)
  (add-hook 'focus-in-hook #'mode-line-update-all)

  (defun mode-line-choose-face (prior-face)
    (if (eq mode-line-selected-window (selected-window))
        prior-face
      'mode-line-inactive))

  (set 'eol-mnemonic-dos (propertize "\u24d3" 'face `(:family ,(myfont 'default3))))  ;; d
  (set 'eol-mnemonic-unix (propertize "\u24e4" 'face `(:family ,(myfont 'default3)))) ;; u
  (set 'eol-mnemonic-mac (propertize "\u24dc" 'face `(:family ,(myfont 'default3))))  ;; m
  (set 'eol-mnemonic-undecided (propertize "?" 'face `(:family ,(myfont 'default3)))) ;; ?
  (defun my-coding-system-name-mnemonic ()
    (let* ((base (coding-system-base buffer-file-coding-system))
           (name (symbol-name base)))
      (cond ((string-prefix-p "utf-8" name) "U8")
            ((string-prefix-p "utf-16" name) "U16")
            ((string-prefix-p "utf-7" name) "U7")
            ((string-prefix-p "japanese-shift-jis" name) "SJIS")
            ((string-match "cp\\([0-9]+\\)" name) (match-string 1 name))
            ((string-match "japanese-iso-8bit" name) "EUC")
            (t "???")
            )))

  (defun my-mode-line-evil-tag ()
    (if (eq mode-line-selected-window (selected-window))
        evil-mode-line-tag
      (let ((color (face-foreground 'mode-line-inactive)))
        (propertize evil-mode-line-tag 'face `(:background ,color :inverse-video t)))))

  (defun split-vc-mode-string (s)
    "Returns cons (\"Git\" . \"master\") from `s' for example \" Git-master\", otherwise (\"\" . \"\")."
    (let ((pos (string-match "[-:]" s)))
      (if pos
          (cons (string-trim-left (substring s 0 pos)) (substring s (1+ pos)))
        '("" . ""))))

  (defun my-mode-line-vc-string ()
    (if vc-mode
        (let* ((l (split-vc-mode-string vc-mode))
               (type (car l))
               (branch (cdr l)))
          (concat (pcase type
                    ("Git" (propertize "\ue725" 'face `(:family ,(myfont 'nerdfont))))    ;; branch icon
                    (_ "?"))
                  (propertize " " 'face `(:family ,(myfont 'ui) :height 30))    ;; gap
                  (cond ((string= branch "") "")
                        ((or (string= branch "master") (string= branch "main"))
                         (propertize branch 'face (mode-line-choose-face 'my-mode-line-accent-face)))
                        (t (propertize branch 'face (mode-line-choose-face  'warning))))))
      ""))

  (defun my-modeline-linum ()
    (format "%3s:%-s/%d"
            (format-mode-line "%c")
            ;; (propertize (format-mode-line "%l")
            ;;             'face (mode-line-choose-face 'my-mode-line-accent-face))
            (format-mode-line "%l")
            (propertize (line-number-at-pos (point-max))
                        'face 'mode-line-inactive)))

  ;; (defun moon-flymake-mode-line ()
  ;;   "https://emacs-china.org/t/flymake-mode-line/7878"
  ;;   (let* ((known (hash-table-keys flymake--backend-state))
  ;;          (running (flymake-running-backends))
  ;;          (disabled (flymake-disabled-backends))
  ;;          (reported (flymake-reporting-backends))
  ;;          (diags-by-type (make-hash-table))
  ;;          (all-disabled (and disabled (null running)))
  ;;          (some-waiting (cl-set-difference running reported)))
  ;;     (maphash (lambda (_b state)
  ;;                (mapc (lambda (diag)
  ;;                        (push diag
  ;;                              (gethash (flymake--diag-type diag)
  ;;                                       diags-by-type)))
  ;;                      (flymake--backend-state-diags state)))
  ;;              flymake--backend-state)
  ;;     (apply #'concat
  ;;            (mapcar (lambda (args)
  ;;                      (apply (lambda (num str face)
  ;;                               (propertize
  ;;                                (format str num) 'face face))
  ;;                             ;; (format str num) 'face `(:family `(myfont 'default3))))
  ;;                             args))
  ;;                    ;; `((,(length (gethash :error diags-by-type)) "\uf79f%d " error)    ;; nf-mdi-ghost
  ;;                    ;; (,(length (gethash :warning diags-by-type)) "\uf071%d " warning)    ;; nf-fa-warning
  ;;                    ;; (,(length (gethash :note diags-by-type)) "\uf05a%d" success))))))   ;; nf-fa-info_circle
  ;;                    `((,(length (gethash :error diags-by-type)) "%d " error)
  ;;                      (,(length (gethash :warning diags-by-type)) "%d " warning)
  ;;                      (,(length (gethash :note diags-by-type)) "%d" success))))))

  ;; (defun flymake--transform-mode-line-format (ret)
  ;;   "Change the output of `flymake--mode-line-format'."
  ;;   (setf (seq-elt (car ret) 1) " FM")
  ;;   ret)
  ;; (advice-add #'flymake--mode-line-format
  ;;             :filter-return #'flymake--transform-mode-line-format)

  (defun my-flycheck-mode-line ()
    (let-alist (flycheck-count-errors flycheck-current-errors)
      (let* ((info (or .info 0))
             (warnings (or .warning 0))
             (errors (or .error 0)))
        (when flycheck-last-status-change
          (concat
           (propertize (int-to-string errors)   'face (mode-line-choose-face 'error)) " "
           (propertize (int-to-string warnings) 'face (mode-line-choose-face 'warning)) " "
           (propertize (int-to-string info)     'face (mode-line-choose-face 'success)))))))

  (defun my-mode-line-buffer-name ()
    (propertize (format-mode-line "%b")
                'face (if (buffer-modified-p)
                          (mode-line-choose-face 'error)
                        (mode-line-choose-face 'my-mode-line-accent-face))))

  ;; (defun my-mode-line-read-only ()
  ;;   (if buffer-read-only
  ;;       (propertize  "\uf023" ;; nf-fa-lock
  ;;                    'face (mode-line-choose-face 'error))))
  ---------------------- comment end ------------------------------------------------------- ]

  (setq mood-line-segment-modal-evil-state-alist
  '((normal   . (" N " . my-evil-normal-tag-face))
    (insert   . (" I " . my-evil-insert-tag-face))
    (visual   . (" V " . my-evil-visual-tag-face))
    (replace  . (" R " . my-evil-insert-tag-face))
    (motion   . (" M " . my-evil-motion-tag-face))
    (operator . (" O " . my-evil-operator-tag-face))
    (emacs    . (" E " . my-evil-emacs-tag-face))))

  (defun mood-line-segment-modal--evil-fn ()
    "Return the current `evil-mode' state."
    (when (boundp 'evil-state)
      (let ((mode-cons (alist-get evil-state
                                  mood-line-segment-modal-evil-state-alist)))
        (concat (propertize (car mode-cons)
                            'face (if (mode-line-window-selected-p)
                                      (cdr mode-cons)
                                    `(:inherit mode-line-inactive :inverse-video t)))))))

  ;; (defface counsel-key-binding
  ;;   '((t :inherit font-lock-keyword-face)))

  ;; (defface my-evil-normal-tag-face `((t (:inherit default) :weight bold)) "")

  (defun mood-line-segment-modal--evil-fn ()
  "Return the current `evil-mode' state."
  (when (boundp 'evil-state)
    (let ((mode-cons (alist-get evil-state
                                mood-line-segment-modal-evil-state-alist)))
      (concat (propertize (car mode-cons)
                          'face (if (mode-line-window-selected-p)
                                    (cdr mode-cons)
                                  `(:inherit mode-line-inactive :inverse-video t)))))))

  (defface my-mood-line-active-dark-face `((t (:inherit mode-line-inactive :overline ,(face-foreground 'mode-line-active))))
    "Dark face that is used in my mood-line when active.")

  (defface my-mood-line-inactive-dark-face
    `((t (:inherit mode-line-inactive :overline ,(face-foreground 'mode-line-inactive))))
    "Dark face that is used in my mood-line when inactive.")

  (defun my-mood-line-segment-which-func ()
    "Return string for the result of `which-function'."
    ;; (format "> %s" (which-function)))
    (propertize (format "> %s" (or (which-function) "-"))
                'face (if (mode-line-window-selected-p)
                          'my-mood-line-active-dark-face
                        'my-mood-line-inactive-dark-face)))

  (defun my-mood-line-segment-cursor-position ()
    "Return string for the position of the cursor in the current buffer."
    (format "%17s" (concat (format-mode-line "%3c:%l")
            (propertize (format "/%d" (line-number-at-pos (point-max)))
                        'face (if (mode-line-window-selected-p)
                                  'my-mood-line-active-dark-face
                                'my-mood-line-inactive-dark-face)))))

  ;; override
  (require 'mood-line-segment-vc)
  (defun mood-line-segment-vc--update (&rest _args)
    "Update `mood-line-segment-vc--text' against the current VCS state."
    (setq mood-line-segment-vc--text
          (format "%6s"
                  (if-let* ((vc-active (and vc-mode buffer-file-name))
                            (backend (vc-backend buffer-file-name))
                            (state (vc-state buffer-file-name))
                            (rev (mood-line-segment-vc--rev vc-mode backend)))
                      (format #("%s%s"
                                ;; 0 5 (face mood-line-status-neutral))
                                0 4 (:eval (face (if (mode-line-window-selected-p)
                                                     'my-mood-line-active-dark-face
                                                   'my-mood-line-inactive-dark-face))))
                              ;; (mood-line--get-glyph :vc-good)
                              "\ue725"
                              rev)
                    (make-string 6 ?\ )
            ))))

  (defface my-mood-line-buffer-status-narrowed  `((t (:family "Symbols Nerd Font Mono" :height 1.0 :foreground ,(face-foreground 'warning)))) "Face that is used as my-mood-line-buffer-status-narrowed.")

  ;; override
  (set-face-attribute 'mood-line-major-mode nil :inverse-video t)
  (set-face-foreground 'mood-line-buffer-status-read-only (face-foreground 'error))
  (defun mood-line-segment-major-mode ()
    "Return the name of the major mode of the current buffer."
    (propertize (substring-no-properties (format " %5s " (format-mode-line mode-name)))
                'face 'mood-line-major-mode))

  ;; override
  (set-face-foreground 'mood-line-buffer-status-read-only (face-foreground 'error))
  (defun mood-line-segment-buffer-status ()
    "Return an indicator representing the status of the current buffer."
    (if (buffer-file-name (buffer-base-buffer))
        (cond
         ;; ((and (buffer-narrowed-p)
         ;;       (buffer-modified-p))
         ;;  (propertize (mood-line--get-glyph :buffer-narrowed)
         ;;              'face 'mood-line-buffer-status-modified))
         ((and (buffer-narrowed-p)
               buffer-read-only)
          (propertize (nerd-icons-faicon "nf-fa-lock")
                      'face 'mood-line-buffer-status-read-only))
         ((buffer-narrowed-p)
          ;; (propertize (mood-line--get-glyph :buffer-narrowed)
          ;;             'face 'mood-line-buffer-status-narrowed))
          (propertize (nerd-icons-mdicon "nf-md-arrow_collapse_vertical")
                      'face 'my-mood-line-buffer-status-narrowed))
         ;; ((buffer-modified-p)
         ;;  (propertize (mood-line--get-glyph :buffer-modified)
         ;;              'face 'mood-line-buffer-status-modified))
         (buffer-read-only
          (propertize (nerd-icons-faicon "nf-fa-lock")
                      'face 'mood-line-buffer-status-read-only)))
      (if (buffer-narrowed-p)
          ;; (propertize (mood-line--get-glyph :buffer-narrowed)
          ;;             'face 'mood-line-buffer-status-narrowed)
          (propertize (nerd-icons-mdicon "nf-md-arrow_collapse_vertical")
                      'face 'my-mood-line-buffer-status-narrowed)
        " ")))

  (defconst my-mood-line-format
    (mood-line-defformat
     :padding ""
     :left
     (((mood-line-segment-modal)              . " ")
      (mood-line-segment-buffer-status)
      ((mood-line-segment-buffer-name)        . " ")
      ((my-mood-line-segment-which-func)      . " ")
      ;; ((mood-line-segment-anzu)            . "  ")
      ;; ((mood-line-segment-multiple-cursors). "  ")
      ;; ((mood-line-segment-cursor-position) . " ")
      ;; (mood-line-segment-scroll)
      )
     :right
     (((my-mood-line-segment-cursor-position) . " ")
      ((mood-line-segment-vc)                 . " ")
      ((mood-line-segment-major-mode)         . " ")
      ;; ((mood-line-segment-misc-info)       . "  ")
      ;; ((mood-line-segment-checker)         . "  ")
      ;; ((mood-line-segment-process)         . "  ")
      "    "
      ))
    "My modeline format for mood-line.")


  (setq mood-line-format my-mood-line-format)
  )

;; ----------------------------------------------------------------------
(use-package saveplace
  :disabled
  :defer
  :config
  (setq save-place-file "~/.emacs.d/.emacs-places")
  (save-place-mode t)                                     ; Enable save-place
  )


;; ----------------------------------------------------------------------
;; im-ctl
;; (defun im-ctl (on) (do-depends-on-each-os))

(when window-system
  (defun im-on ()
    (interactive)
    (if (fboundp 'im-ctl)
        (im-ctl t)
      (message "Error: void function \"im-ctl\"! System dependent")))

  (defun im-off ()
    (interactive)
    (if (fboundp 'im-ctl)
        (im-ctl nil)
      (message "Error: void function \"im-ctl\"! System dependent")))

  (add-hook 'minibuffer-setup-hook #'im-off)
  (add-hook 'minibuffer-exit-hook #'im-off)
  ;; (add-hook 'focus-out-hook #'im-off)
  (add-hook 'evil-insert-state-exit-hook #'im-off)

  (im-off)      ;; なぜか起動直後にim-onしているので追加
)

;; ----------------------------------------------------------------------
;; (defvar exclude-face-list '(rainbow-delimiters-base-face
;;                             rainbow-delimiters-depth-1-face
;;                             rainbow-delimiters-depth-2-face
;;                             rainbow-delimiters-depth-3-face
;;                             rainbow-delimiters-depth-4-face
;;                             rainbow-delimiters-depth-5-face
;;                             rainbow-delimiters-depth-6-face
;;                             rainbow-delimiters-depth-7-face
;;                             rainbow-delimiters-depth-8-face
;;                             rainbow-delimiters-depth-9-face
;;                             rainbow-delimiters-mismatched-face
;;                             rainbow-delimiters-unmatched-face
;;                             sp-show-pair-match-face
;;                             mode-line-buffer-id
;;                             mode-line-emphasis
;;                             mode-line-highlight
;;                             mode-line-inactive
;;                             mode-line))
;;
;; (my-font-lighter (remove-if (lambda (x) (member x exclude-face-list)) (face-list)))

;; (zerodark-setup-modeline-format)

;; ----------------------------------------------------------------------
;; ここからモードライン設定
;; https://tsuu32.hatenablog.com/entry/2019/08/04/160316

;; >>>>
[
(defface my-mode-line-accent-face  `((t (:foreground ,(face-background 'my-evil-normal-tag-face)))) "")

(defvar mode-line-selected-window nil)

(defun mode-line-record-selected-window ()
  (unless (string-match-p " \*Minibuf" (buffer-name (window-buffer (car (window-list)))))
    (setq mode-line-selected-window (selected-window))))

(add-hook 'window-configuration-change-hook #'mode-line-record-selected-window)

(defun mode-line-update-all ()
  (force-mode-line-update t))

(add-hook 'buffer-list-update-hook #'mode-line-update-all)
(add-hook 'focus-in-hook #'mode-line-update-all)

(defun mode-line-choose-face (prior-face)
  (if (eq mode-line-selected-window (selected-window))
      prior-face
    'mode-line-inactive))

(set 'eol-mnemonic-dos (propertize "\u24d3" 'face `(:family ,(myfont 'default3))))  ;; d
(set 'eol-mnemonic-unix (propertize "\u24e4" 'face `(:family ,(myfont 'default3)))) ;; u
(set 'eol-mnemonic-mac (propertize "\u24dc" 'face `(:family ,(myfont 'default3))))  ;; m
(set 'eol-mnemonic-undecided (propertize "?" 'face `(:family ,(myfont 'default3)))) ;; ?
(defun my-coding-system-name-mnemonic ()
  (let* ((base (coding-system-base buffer-file-coding-system))
         (name (symbol-name base)))
    (cond ((string-prefix-p "utf-8" name) "U8")
          ((string-prefix-p "utf-16" name) "U16")
          ((string-prefix-p "utf-7" name) "U7")
          ((string-prefix-p "japanese-shift-jis" name) "SJIS")
          ((string-match "cp\\([0-9]+\\)" name) (match-string 1 name))
          ((string-match "japanese-iso-8bit" name) "EUC")
          (t "???")
          )))

(defun my-mode-line-evil-tag ()
  (if (eq mode-line-selected-window (selected-window))
      evil-mode-line-tag
    (let ((color (face-foreground 'mode-line-inactive)))
      (propertize evil-mode-line-tag 'face `(:background ,color :inverse-video t)))))

(defun split-vc-mode-string (s)
  "Returns cons (\"Git\" . \"master\") from `s' for example \" Git-master\", otherwise (\"\" . \"\")."
  (let ((pos (string-match "[-:]" s)))
    (if pos
        (cons (string-trim-left (substring s 0 pos)) (substring s (1+ pos)))
      '("" . ""))))

(defun my-mode-line-vc-string ()
  (if vc-mode
      (let* ((l (split-vc-mode-string vc-mode))
             (type (car l))
             (branch (cdr l)))
        (concat (pcase type
                  ("Git" (propertize "\ue725" 'face `(:family ,(myfont 'nerdfont))))    ;; branch icon
                  (_ "?"))
                (propertize " " 'face `(:family ,(myfont 'ui) :height 30))    ;; gap
                (cond ((string= branch "") "")
                      ((or (string= branch "master") (string= branch "main"))
                       (propertize branch 'face (mode-line-choose-face 'my-mode-line-accent-face)))
                      (t (propertize branch 'face (mode-line-choose-face  'warning))))))
    ""))

(defun my-mode-line-num ()
  (format "%3s:%-s/%d"
          (format-mode-line "%c")
          (propertize (format-mode-line "%l")
                      'face (mode-line-choose-face 'my-mode-line-accent-face))
          (line-number-at-pos (point-max))))

;; (defun moon-flymake-mode-line ()
;;   "https://emacs-china.org/t/flymake-mode-line/7878"
;;   (let* ((known (hash-table-keys flymake--backend-state))
;;          (running (flymake-running-backends))
;;          (disabled (flymake-disabled-backends))
;;          (reported (flymake-reporting-backends))
;;          (diags-by-type (make-hash-table))
;;          (all-disabled (and disabled (null running)))
;;          (some-waiting (cl-set-difference running reported)))
;;     (maphash (lambda (_b state)
;;                (mapc (lambda (diag)
;;                        (push diag
;;                              (gethash (flymake--diag-type diag)
;;                                       diags-by-type)))
;;                      (flymake--backend-state-diags state)))
;;              flymake--backend-state)
;;     (apply #'concat
;;            (mapcar (lambda (args)
;;                      (apply (lambda (num str face)
;;                               (propertize
;;                                (format str num) 'face face))
;;                             ;; (format str num) 'face `(:family `(myfont 'default3))))
;;                             args))
;;                    ;; `((,(length (gethash :error diags-by-type)) "\uf79f%d " error)    ;; nf-mdi-ghost
;;                    ;; (,(length (gethash :warning diags-by-type)) "\uf071%d " warning)    ;; nf-fa-warning
;;                    ;; (,(length (gethash :note diags-by-type)) "\uf05a%d" success))))))   ;; nf-fa-info_circle
;;                    `((,(length (gethash :error diags-by-type)) "%d " error)
;;                      (,(length (gethash :warning diags-by-type)) "%d " warning)
;;                      (,(length (gethash :note diags-by-type)) "%d" success))))))

;; (defun flymake--transform-mode-line-format (ret)
;;   "Change the output of `flymake--mode-line-format'."
;;   (setf (seq-elt (car ret) 1) " FM")
;;   ret)
;; (advice-add #'flymake--mode-line-format
;;             :filter-return #'flymake--transform-mode-line-format)

(defun my-flycheck-mode-line ()
  (let-alist (flycheck-count-errors flycheck-current-errors)
    (let* ((info (or .info 0))
           (warnings (or .warning 0))
           (errors (or .error 0)))
      (when flycheck-last-status-change
        (concat
         (propertize (int-to-string errors)   'face (mode-line-choose-face 'error)) " "
         (propertize (int-to-string warnings) 'face (mode-line-choose-face 'warning)) " "
         (propertize (int-to-string info)     'face (mode-line-choose-face 'success)))))))

(defun my-mode-line-buffer-name ()
  (propertize (format-mode-line "%b")
              'face (if (buffer-modified-p)
                        (mode-line-choose-face 'error)
                      (mode-line-choose-face 'my-mode-line-accent-face))))

(defun my-mode-line-read-only ()
  (if buffer-read-only
      (propertize  "\uf023" ;; nf-fa-lock
                   'face (mode-line-choose-face 'error))
    " "))

;; -----------
;;  (defun simple-mode-line-render (left right)
;;    "Return a string of `window-width' length.
;; Containing LEFT, and RIGHT aligned respectively."
;;    (let ((available-width
;;           (- (window-total-width)
;;              (+ (string-width (format-mode-line left))
;;                 (string-width (format-mode-line right))))))
;;      (append left
;;              (list (format (format "%%%ds" available-width) ""))
;;              right)))

;;  (setq-default
;;   mode-line-format
;;   '((:eval
;;      (simple-mode-line-render
;;       ;; Left.
;;       (quote ("%e "
;;               mode-line-buffer-identification
;;               " %l : %c"
;;               evil-mode-line-tag
;;               "[%*]"))
;;       ;; Right.
;;       (quote ("%p "
;;               mode-line-frame-identification
;;               mode-line-modes
;;               mode-line-misc-info))))))

;;---------
(defun my-mode-line--form ()
  (let* ((left-part (concat
                     (my-mode-line-evil-tag)
                     " "
                     (my-mode-line-buffer-name)
                     " "
                     (my-mode-line-read-only)
                     " "))
         (right-part (concat
                      (my-coding-system-name-mnemonic)
                      (mode-line-eol-desc)
                      " "
                      (my-mode-line-num)
                      " "
                      (my-mode-line-vc-string)
                      " "
                      (cond
                       ((and (boundp 'flycheck-mode) flycheck-mode) (my-flycheck-mode-line))
                       (t "     "))
                      " "
                      mode-name
                      " "
                      ;; mode-line-misc-info
                      ;; " "
                      ;; (vc-mode vc-mode)
                      ;; mode-line-modes))
                      ))
         ;; (margin-env (case system-type
         (margin-env (cl-case system-type   ;; 28.1
                       (darwin 1)
                       (windows-nt 8)
                       (t 0)))
         (margin
          (propertize " "
                      'display `(space :align-to (- (+ scroll-bar scroll-bar) ,(string-width right-part) ,margin-env)))))
    (concat left-part margin right-part)))
(setq-default mode-line-format '(:eval (my-mode-line--form)))

;; other-window でウィンドウを切り替えたときにミニバッファの色がおかしくなるのをなんとかする
;; もっさりしてて釈然としないけどとりあえず
(defun my-adv--other-window (&rest _)
  "Workaround for glitch of modeline color after `otehr-window'.
This makes use of the fact that by `message' a newline, the window configuration changes and the entire screen is redrawn."
  (message "\n")
  (message nil))
(advice-add 'other-window :after #'my-adv--other-window)
;; (advice-remove 'other-window  #'my-adv--other-window)    ;; for testing

;; (kill-local-variable 'mode-line-format)

;; ;; modeline の右端が切れる問題 East Asian Ambiguous Width
;; ;; https://note.com/5mingame2/n/nc8010be0c32e
;; (set-language-environment "English") ;; call this explicity
;; (prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-buffer-file-coding-system 'utf-8-unix)
;; (setq locale-coding-system 'utf-8)
;; (setq file-name-coding-system 'utf-8)
;; (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
;; (require 'my-utf-8-eaw-fullwidth)

] ;; <<<

;; ----------------------------------------------------------------------
;; multi byte file setting
;;
;; https://github.com/uwabami/emacs#cp5022xel
(require 'cp5022x)
(set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201 'katakana-jisx0201 'iso-8859-1 'unicode)
(set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

;; https://handlename.hatenablog.jp/entry/2014/10/17/103603
(prefer-coding-system 'utf-8-unix)

;; https://kokufu.blogspot.com/2016/05/emacs-undecided-unix-cannot-encode.html
(add-hook 'find-file-hook #'(lambda ()
                             (cond ((string-match "undecided-?.*" (format "%s" buffer-file-coding-system))
                                    (let ((coding-system-for-read 'utf-8-unix))
                                      (revert-buffer t t))))))

;; ----------------------------------------------------------------------
(use-package tabbar
  :if window-system
  ;; :disabled
  :hook ((after-save   . tabbar-on-saving-buffer)
         (first-change . tabbar-on-modifying-buffer))
  :config
  (tabbar-mode 1)

  (set-face-attribute 'tabbar-default nil
                      :height my-face-adj-tabbar-height        ;; defined at _mac.el or _windows.el
                      :family (myfont 'ui))

  (global-set-key (kbd "M-u") 'tabbar-backward-tab)
  (global-set-key (kbd "M-i") 'tabbar-forward-tab)

  (tabbar-mwheel-mode nil)                  ;; マウスホイール無効
  (setq tabbar-buffer-groups-function nil)  ;; グループ無効             ;; モードラインがちらつく原因
  (setq tabbar-buffer-groups-function (lambda () (list "")))
  (setq tabbar-use-images nil)              ;; 画像を使わない

  ;;----- 左側のボタンを消す
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))

  (defun my-tabbar-buffer-list ()
    (delq nil
          (mapcar #'(lambda (b)
                      (cond
                       ((eq (current-buffer) b) b)                          ;; Always include the current buffer.
                       ((memq (with-current-buffer b major-mode)            ;; popperで表示するのでtabbarでは表示しない
                              '(
                                vterm-mode                                  ; x vterm
                                reb-mode)) nil)                             ; x reb
                       ((buffer-file-name b) b)                             ; ファイル持ちのバッファは表示する
                       ((char-equal ?\  (aref (buffer-name b) 0)) nil)      ; 空白で始まりのバッファ名は表示しない
                       ((equal "*scratch*" (buffer-name b)) b)              ; *scratch*バッファは表示する
                       ((char-equal ?* (aref (buffer-name b) 0)) nil)       ; それ以外の * で始まるバッファは表示しない
                       ((string-match "^magit" (buffer-name b)) nil)        ; magit が開くバッファは表示しない
                       ((buffer-live-p b) b)
                       ))
                  (buffer-list))))

  (setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

  ;; mod
  (defun tabbar-buffer-tab-label (tab)
    "Return a label for TAB.
That is, a string used to represent it on the tab bar."
    (let ((label  (if tabbar--buffer-show-groups
                      (format " [%s] " (tabbar-tab-tabset tab))
                    (format " %s " (tabbar-tab-value tab)))))
      ;; Unless the tab bar auto scrolls to keep the selected tab
      ;; visible, shorten the tab label to keep as many tabs as possible
      ;; in the visible area of the tab bar.
      (if tabbar-auto-scroll-flag
          label
        (tabbar-shorten
         label (max 1 (/ (window-width)
                         (length (tabbar-view
                                  (tabbar-current-tabset)))))))))

  (defun tabbar-on-saving-buffer ()
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))

  (defun tabbar-on-modifying-buffer ()
    (set-buffer-modified-p (buffer-modified-p))
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))

  (defun tabbar-after-modifying-buffer (begin end length)
    (set-buffer-modified-p (buffer-modified-p))
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))

  )

;; ----------------------------------------------------------------------
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-undo-system 'undo-fu)

  :config
  (evil-mode 1)
  ;; (evil-set-initial-state 'help-mode 'emacs)
  ;; (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'slime-editing-mode 'emacs)

  (setq evil-emacs-state-message nil)
  (setq evil-insert-state-message nil)
  (setq evil-motion-state-message nil)
  (setq evil-normal-state-message nil)
  (setq evil-operator-state-message nil)
  (setq evil-replace-state-message nil)
  (setq evil-visual-state-message nil)
  ;; (setq evil-search-wrap nil)                ;; disable wrap-around for evil-search
  (setq evil-search-wrap-ring-bell t)
  (setq evil-regexp-search nil)

  (evil-ex-define-cmd "q[uit]" #'kill-this-buffer)

  ;; インサートモードではEmacsキーバインド
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] #'evil-normal-state)

  ;; swith-buffer後になぜかevil-visual-stateになっても強制的にnormal-stateに戻す
  (defun my-adv-switch-to-bufffer--disable-evil-visual-state (&rest args)
    (evil-visual-state -1)
    (evil-normal-state 1))
  (advice-add 'switch-to-buffer :after #'my-adv-switch-to-bufffer--disable-evil-visual-state)

  ;; evil-visual-stateで外部コマンドを表示したり posframe を開いたりしたあとでフォーカスが戻ってきたときに
  ;; 強制的にevil-normal-stateに戻す workaround
  (add-function :after after-focus-change-function #'evil-exit-visual-state)

  ;; evil keybindings
  (define-key evil-normal-state-map (kbd "M-c") #'ffap)                       ; M-RET
  ;; (define-key evil-insert-state-map (kbd "M-v") #'nop)                        ; prevent paste in Mac
  (define-key evil-visual-state-map (kbd "x") #'evil-delete)

  ;; for package-mode
  (evil-add-hjkl-bindings package-menu-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous)

  ;; evil modeline indicator
  (delq 'w32-ime-mode-line-state-indicator mode-line-format)
  ;; (add-hook 'emacs-startup-hook (lambda ()
     (setq evil-mode-line-format '(before . mode-line-front-space))

     (setq evil-normal-state-tag   (propertize " N " 'face 'my-evil-normal-tag-face)
           evil-emacs-state-tag    (propertize " E " 'face 'my-evil-emacs-tag-face)
           evil-insert-state-tag   (propertize " I " 'face 'my-evil-insert-tag-face)
           evil-motion-state-tag   (propertize " M " 'face 'my-evil-motion-tag-face)
           evil-visual-state-tag   (propertize " V " 'face 'my-evil-visual-tag-face)
           ;; evil-operator-state-tag (propertize " O " 'face 'my-evil-operator-tag-face))))
           evil-operator-state-tag (propertize " O " 'face 'my-evil-operator-tag-face))

     ;; cursor color
     (setq evil-normal-state-cursor `(box ,(face-background 'my-evil-normal-tag-face))
           evil-insert-state-cursor `((bar . 3) ,(face-background 'my-evil-insert-tag-face))
           evil-visual-state-cursor `(hollow ,(face-background 'my-evil-visual-tag-face))
           evil-emacs-state-cursor  `((hbar . 5) ,(face-background 'my-evil-emacs-tag-face)))

  ;; ----------
  ;; currently not use
  (defmacro define-key-evil-visual (vsel key cmd)
    ;; FIXME giving (kbd "c") to arg key occurs not work
    ;; FIXME giving function like (defun hoge (beg end) (interactive "r") ..) to arg cmd occurs not work
    `(define-key evil-visual-state-map ,key
       #'(lambda() (interactive)
           (if (eq evil-visual-selection ,vsel)
               (funcall ,cmd)
             ;; (funcall ,(lookup-key evil-visual-state-map (kbd "c")))))))
             (funcall ,(lookup-key evil-visual-state-map key))))))

  ;; ----------
  (defun my-adv-evil-change--without-kill-new (orig-fn beg end &optional type _ &rest args)
    "\"c\" (evil-change) without kill-new"
    (apply orig-fn beg end type ?_ args))

  (advice-add 'evil-change :around 'my-adv-evil-change--without-kill-new)

  ;; ----------
  (defun my-beginning-of-line ()
      (interactive)
      (if (bolp)
          (back-to-indentation)
        (beginning-of-line)))

  ;; ----------
  (defun my-end-of-line ()
    (interactive)
    (cl-labels ((my-eolp () (if (or (evil-normal-state-p) (evil-motion-state-p))
                                (= (1+ (point)) (line-end-position))
                              (eolp))))
      (if (my-eolp)
          (progn
            (beginning-of-line)
            (when (comment-search-forward (line-end-position) t)
              (re-search-backward comment-start-skip (line-beginning-position) t)
              (skip-syntax-backward "< " (line-beginning-position))))
        (end-of-line))))

  ;; ----------
  (defun my-copy-whole-buffer ()
    "Copy whole buffer to kill ring."
    (interactive)
    (kill-new (buffer-substring-no-properties (point-min) (point-max)))
    (message "Copied whole buffer to kill-ring"))

  ;; ----------
(defun my-evil-beginning/end-of-buffer ()
    (interactive)
    (goto-char (cond ((bobp) (point-max))
                     (t (point-min)))))
  ;; (define-key evil-motion-state-map (kbd "g g") #'my-evil-beginning/end-of-buffer)


  ;; ----------
  ;; motion-state-map (normal + vilual)
  (define-key evil-motion-state-map (kbd "!") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "@") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "#") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "$") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "%") #'nop)                            ; unmap
  ;; (define-key evil-motion-state-map (kbd "^") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "&") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "*") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "(") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd ")") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "C-M-d") #'nop)                        ; unmap
  (define-key evil-motion-state-map (kbd "H") #'evil-backward-little-word-begin)
  (define-key evil-motion-state-map (kbd "L") #'evil-forward-little-word-begin)
  ;; (define-key evil-motion-state-map (kbd "c") #'nop)                            ; unmap evil-goto-error
  ;; (define-key evil-motion-state-map (kbd "g e")   #'evil-goto-error)
  (define-key evil-motion-state-map (kbd "g -")   #'evil-jump-backward)

  (define-key evil-motion-state-map (kbd "i")   #'nop)                          ; unmap
  ;; (define-key evil-motion-state-map (kbd "V")   #'nop)                          ; unmap
  ;; (define-key evil-motion-state-map (kbd "C-v") #'nop)                          ; unmap
  (define-key evil-motion-state-map (kbd "M-v") #'nop)                          ; unmap
  (define-key evil-motion-state-map (kbd "C-6") nil)        ; evil-switch-to-windows-last-buffer
  (define-key evil-motion-state-map (kbd "C-f") nil)
  (define-key evil-motion-state-map (kbd "C-b") nil)
  (define-key evil-motion-state-map (kbd "C-o") nil)		; evil-jump-backward
  (define-key evil-motion-state-map (kbd "C-e") nil)
  (define-key evil-motion-state-map (kbd "C-d") nil)        ; evil-scroll-down
  (define-key evil-motion-state-map (kbd "'") nil)          ; evil-goto-mark-line
  (define-key evil-motion-state-map (kbd "\"") nil)         ; evil-use-register
  ;; (define-key evil-motion-state-map (kbd "C-p") nil)
  (define-key evil-motion-state-map (kbd "C-h") nil)
  (define-key evil-motion-state-map (kbd "C-y") nil)        ; evil-scrollline-up
  (define-key evil-motion-state-map (kbd "M-h") nil)

  (define-key evil-motion-state-map (kbd "m") #'evil-scroll-page-down)
  (define-key evil-motion-state-map (kbd "M") #'evil-scroll-page-up)
  (define-key evil-motion-state-map (kbd "j") #'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "k") #'evil-previous-visual-line)
  ;; (define-key evil-motion-state-map (kbd "6") #'evil-first-non-blank)
  ;; (define-key evil-motion-state-map (kbd "v") #'my-evil-visual-cycle)
  (define-key evil-motion-state-map (kbd "M-w") #'my-forward-word)
  ;; (define-key evil-motion-state-map (kbd "g g") #'my-evil-beginning-of-buffer)
  ;; (define-key evil-motion-state-map (kbd "g e") #'my-evil-end-of-buffer)
  (define-key evil-motion-state-map (kbd "g p") #'flymake-goto-prev-error)
  (define-key evil-motion-state-map (kbd "g n") #'flymake-goto-next-error)
  (define-key evil-motion-state-map (kbd "g h") 'evil-jump-backward)
  ;; (define-key evil-motion-state-map (kbd "Y") #'my-evil-yank-whole-buffer)
  (define-key evil-motion-state-map (kbd "TAB") #'evil-indent-line)
  (define-key evil-motion-state-map "/" #'evil-search-forward)
  (define-key evil-motion-state-map "?" #'evil-search-backward)
  (define-key evil-motion-state-map (kbd ":") #'nop)        ; unmap :
  (define-key evil-motion-state-map (kbd ";") #'evil-ex)    ; works as :
  (define-key evil-motion-state-map "f" #'avy-goto-char-timer)
  (define-key evil-motion-state-map (kbd "4") #'my-end-of-line)
  (define-key evil-motion-state-map (kbd "0") #'my-beginning-of-line)

  ;; normal-state-map
  (define-key evil-normal-state-map (kbd "3") #'evil-search-word-backward)      ; works as #
  (define-key evil-normal-state-map (kbd "8") #'evil-search-word-forward)       ; works as *
  (define-key evil-normal-state-map (kbd "9") #'evilmi-jump-items)
  (define-key evil-normal-state-map (kbd "!") #'shell-command)
  (define-key evil-normal-state-map (kbd "q") nil)
  (define-key evil-normal-state-map (kbd "m") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)        ; evil-repeat-pop-next
  (define-key evil-normal-state-map (kbd "C-p") nil)        ; evil-paste-pop
  (define-key evil-normal-state-map (kbd "M-u") nil)
  (define-key evil-normal-state-map (kbd "M-i") nil)
  (define-key evil-normal-state-map (kbd "g p") nil)        ; evil-paste-after-cursor-after
  (define-key evil-normal-state-map (kbd "C-r") nil)        ; evil-redo
  (define-key evil-normal-state-map (kbd "U")   #'evil-redo)
  ;; (define-key evil-normal-state-map (kbd "M-p") #'counsel-yank-pop)
  ;; (define-key evil-normal-state-map (kbd "SPC") #'evil-force-normal-state)
  ;; (define-key evil-normal-state-map (kbd "g f") #'my-beginning-of-defun)
  (define-key evil-normal-state-map (kbd "g I") #'my-put-file-compile-flags)
  (define-key evil-normal-state-map (kbd "g i") #'xref-find-references)
  (define-key evil-normal-state-map (kbd "g o") #'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "g 0") #'xref-go-back)
  (define-key evil-normal-state-map (kbd "g j") #'git-gutter:next-hunk)
  (define-key evil-normal-state-map (kbd "g k") #'git-gutter:previous-hunk)
  (define-key evil-normal-state-map (kbd "g s") #'git-gutter:popup-hunk)        ;; git diff
  (define-key evil-normal-state-map (kbd "g r") #'git-gutter:revert-hunk)       ;; git unstage
  (define-key evil-normal-state-map (kbd "g u") #'git-gutter:revert-hunk)       ;; git unstage
  (define-key evil-normal-state-map (kbd "g a") #'git-gutter:stage-hunk)        ;; git add
  ;; (define-key evil-normal-state-map (kbd "g p") #'what-cursor-position)         ;; g a --> g p
  (define-key evil-normal-state-map (kbd "A") #'nop)                 ; unmap A
  (define-key evil-normal-state-map (kbd "a") #'evil-append-line)    ; works as A
  (define-key evil-normal-state-map (kbd "1 1") #'show-overlay-and-prop-and-face-at)
  (define-key evil-normal-state-map (kbd "' a") #'my-copy-whole-buffer)
  (define-key evil-normal-state-map "x" 'delete-forward-char)       ; "x" command without kill-new
  (define-key evil-normal-state-map "X" 'delete-backward-char)      ; "X" command without kill-new

  ;; insert-state-map
  (define-key evil-insert-state-map (kbd "C-h") #'delete-backward-char)
  ;; (define-key evil-insert-state-map (kbd "M-h") #'my-backward-kill-word)
  (define-key evil-insert-state-map (kbd "TAB") #'(lambda () (interactive) (insert-tab)))
  ;; (define-key evil-insert-state-map (kbd "'") #'my-insert-squote)
  ;; (define-key evil-insert-state-map (kbd "\"") #'my-insert-dquote)

  ;; visual-state-map
  (define-key evil-visual-state-map (kbd "v") #'evil-visual-paste)
  (define-key evil-visual-state-map (kbd "e") #'my-evil-visual-eval-region)
  (define-key evil-visual-state-map (kbd "w") #'my-evil-visual-write-region)
  (define-key evil-visual-state-map (kbd "a") #'my-evil-visual-align-region)
  (define-key evil-visual-state-map (kbd ";") #'my-evil-visual-comment-region)
  (define-key evil-visual-state-map (kbd "i") #'my-evil-visual-indent-region)
  ;; (define-key evil-visual-state-map (kbd "g g") #'my-evil-beginning/end-of-buffer)

  ;; (add-to-list 'evil-emacs-state-modes 'macrostep-mode)

  ;; ;; not work, fixme
  ;; (add-hook 'macrostep-mode-hook #'(lambda ()
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "RET") 'macrostep-expand)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "=") 'macrostep-expand)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "e") 'macrostep-expand)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "DEL") 'macrostep-collapse)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "u") 'macrostep-collapse)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "c") 'macrostep-collapse)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "TAB") 'macrostep-next-macro)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "n") 'macrostep-next-macro)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "M-TAB") 'macrostep-prev-macro)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "p") 'macrostep-prev-macro)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "q") 'macrostep-collapse-all)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "C-c C-c") 'macrostep-collapse-all)))

  ;; ----------
  (defun my-put-file-compile-flags (&optional dir)
    "put minimum 'compile-flags.txt' for LSP () "
    (interactive "p")
    (let* ((file-name "compile_flags.txt")
           (insert-default-directory t)
           (dir (read-directory-name (concat file-name " --> ")))
           (path (concat dir "/" file-name))
           (lines "-xc
-std=c17"))
      (if (file-exists-p path)
          (message "already exists. do nothing.")
        (write-region lines nil path)
        )))

  (defun my-evil-visual-eval-region (beg end)
    (interactive "r")
    (if (eq evil-visual-selection 'line)
        (progn
          (message "eval-region: %s"
                   ;; (cond ((eq major-mode 'emacs-lisp-mode)
                   ;;        (eval-region beg end))
                   ;;       ((eq major-mode 'lisp-mode)
                   ;;        (slime-eval-region beg end))))
                   (eval-region beg end))
          (evil-exit-visual-state))
      (evil-forward-WORD-end)
      (evil-backward-char)))

  (defun my-evil-visual-write-region (beg end)
    (interactive "r")
    (if (eq evil-visual-selection 'line)
        (let ((fn (read-file-name "write-region:")))
            (write-region beg end fn))
      (evil-forward-word-begin)))

  (defun my-evil-visual-align-region (beg end)
    (interactive "r")
    (when (eq evil-visual-selection 'line)
      (align beg end)
      (evil-exit-visual-state)
      (message "aligned")))

  (defun my-evil-visual-comment-region (beg end)
    (interactive "r")
    (when (eq evil-visual-selection 'line)
      (comment-or-uncomment-region beg end)
      (evil-exit-visual-state)))

  (defun my-evil-visual-indent-region (beg end)
    (interactive "r")
    (when (eq evil-visual-selection 'line)
      (indent-region beg end)
      (evil-exit-visual-state)))

  ;; ----------
  (defun my-evil-visual-narrow-to-region (beg end)
    "Narrow to region from BEG END."
    (when (or (eq evil-visual-selection 'char)
              (eq evil-visual-selection 'line))
      (narrow-to-region beg end)
      (evil-exit-visual-state)
      (my-evil-operator-narrow-to-region-mode 1)))

  (define-minor-mode my-evil-operator-narrow-to-region-mode
    "Buffer local minor mode of narrow-to-region operator for Evil."
    :lighter ""
    :keymap (make-sparse-keymap)
    :after-hook (when my-evil-operator-narrow-to-region-mode (goto-char (point-min)))
    (evil-normalize-keymaps)
    (defun my-evil-visual-narrow-to-region-exit ()
      (interactive)
      (widen)
      (my-evil-operator-narrow-to-region-mode 0)
      (evil-normal-state)))

    (evil-define-key 'normal my-evil-operator-narrow-to-region-mode-map
                             "q" #'my-evil-visual-narrow-to-region-exit)

  ;; ----------
  ;; (defun evil-return-insert-mode-after-save ()
  ;;   (when evil-insert-state-minor-mode
  ;;     (funcall (evil-escape--escape-normal-state))))

  ;; (add-hook 'after-save-hook #'evil-return-insert-mode-after-save)

  ;; ----------
  (defun my-evil-paste (&optional arg)
    (interactive  "P")
    (if (memq last-command '(evil-paste-before evil-paste-after))
        (call-interactively #'evil-paste-pop)
      (call-interactively (if arg #'evil-paste-before #'evil-paste-after))))
;;  (define-key evil-normal-state-map (kbd "p") #'my-evil-paste)

  ;; ----------
  (defun my-evil-search-dummy-func ()
    (remove-hook 'isearch-mode-hook #'my-evil-search-dummy-func)
    (setq unread-command-events (listify-key-sequence (kbd "RET"))))

  (defun my-evil-search-dummy ()
    "workaround for `my-evil-search-from-region-next'. swapping search direction is prevent by calling this function prior 'my-evil-search-from-region-next'."
    (add-hook 'isearch-mode-hook #'my-evil-search-dummy-func)
    (call-interactively 'evil-search-forward))

  (defvar my-evil-search-first-time t)

  (defun my-evil-search-from-region-next (beg end)
    "under the evil-visual-state, jump next immediately after selecting region and pressing specified key."
    (when my-evil-search-first-time
      (my-evil-search-dummy)
      (setq my-evil-search-first-time nil))
    (my-evil-search-from-region-1 beg end t))

  (defun my-evil-search-from-region-prev (beg end)
    "under the evil-visual-state, jump previous immediately after selecting region and pressing specified key."
    (interactive "r")
    (unless (save-excursion (goto-char beg) (search-forward "\n" end t))
      (when my-evil-search-first-time
        (my-evil-search-dummy)
        (setq my-evil-search-first-time nil))
      (my-evil-search-from-region-1 beg end nil)))

  (defun my-evil-search-from-region-1 (beg end forward-p)
    "pull string from region as search string then jump"
    (let ((s (buffer-substring-no-properties beg end)))
      (delete-duplicates (push s (if evil-regexp-search regexp-search-ring search-ring))
                         :test 'string= :from-end t)
      (evil-normal-state nil)
      (evil-search s forward-p)))

  (defun my-evil-visual-narrow-or-jump (beg end)
    (interactive "r")
    (if (save-excursion (goto-char beg) (search-forward "\n" end t)) ;; multiple lines?
        (my-evil-visual-narrow-to-region beg end)
      (my-evil-search-from-region-next beg end)))

  (define-key evil-visual-state-map (kbd "n") #'my-evil-visual-narrow-or-jump)
  (define-key evil-visual-state-map (kbd "N") #'my-evil-search-from-region-prev)

  ;; ----------
  (defvar my-evil-visual-surround-pairs '((?\" . ?\") (?\' . ?\') (?\( . ?\)) (?\{ . ?\}) (?\[ . ?\]) (?\< . ?>) (?\` . ?`)))
  (defun my-evil-visual-surround-add (beg end s)
    "Surround selected string with specified character."
    (let* ((prompt (format "Surround '%s' with:" s))
           (c (read-char prompt))
           (pair (my-evil-visual-surround-get-pair c))
           head tail)
      (if pair
          (setq head (car pair) tail (cdr pair))
        (setq head c tail c))
      (save-excursion
        (goto-char end)
        (insert (char-to-string tail))
        (goto-char beg)
        (insert (char-to-string head)))))

  (defun my-evil-visual-surround-change (beg end s)
    "Change surrounding character pair with new character, or delete it if you input RET specifically."
    (let* ((prompt (format "Re-surround '%s' with:" (substring s 1 (1- (length s)))))
           (c (read-char prompt))
           (pair (my-evil-visual-surround-get-pair c))
           head tail)
      (if pair
          (setq head (car pair) tail (cdr pair))
        (setq head c tail c))
      (save-excursion
        (goto-char (1- end))
        (delete-char 1)
        (unless (eq tail #xd) (insert (char-to-string tail)))
        (goto-char beg)
        (delete-char 1)
        (unless (eq head #xd) (insert (char-to-string head))))))

(defun my-evil-visual-surround-get-tail (head)
  (or (cdr (assoc head my-evil-visual-surround-pairs))
      0))

  (defun my-evil-visual-surround-get-pair (head-or-tail)
    (or (assoc head-or-tail my-evil-visual-surround-pairs)
        (rassoc head-or-tail my-evil-visual-surround-pairs)))

(defun my-evil-visual-surround (beg end)
    (interactive "r")
    (let* ((s (buffer-substring-no-properties beg end))
           (head (aref s 0))
           (tail (aref s (1- (length s)))))
      (cond ((= (- end beg) 1)
             (my-evil-visual-surround-add beg end s))
            ((or (= head tail) (= tail (my-evil-visual-surround-get-tail head)))
             (my-evil-visual-surround-change beg end s))
            (t (my-evil-visual-surround-add beg end s)))))

  (define-key evil-visual-state-map "s" 'my-evil-visual-surround)

  ;; ----------
  ;; (defun my-evil-beginning-of-buffer ()
  ;;   (interactive)
  ;;   (if (eq last-command this-command)
  ;;       (exchange-point-and-mark t)
  ;;     (push-mark (point) t)
  ;;     (goto-char (point-min))))

  ;; (defun my-evil-end-of-buffer ()
  ;;   (interactive)
  ;;   (if (eq last-command this-command)
  ;;       (exchange-point-and-mark t)
  ;;     (push-mark (point) t)
  ;;     (goto-char (point-max))))

  (defun my-evil-yank-whole-buffer ()
    (interactive)
    (evil-yank-line (point-min) (point-max))
    (message "Copied whole buffer"))

  ;; ----------
  (defvar-local my-evil-visual-cycle-state nil)

  (lexical-let (pos-init)
    (defun my-evil-visual-cycle ()
      "Cycle evil-visual like: V-CHAR -> V-LINE -> V-BLOCK -> V-CHAR ..."
      (interactive)
      (cl-labels ((= (state) (eq my-evil-visual-cycle-state state))
                  (-> (state) (setq my-evil-visual-cycle-state state))
                  (set-tag (s) (setq evil-visual-state-tag (propertize (concat " " s " ") 'face
             `((:background ,(mycolor 'green) :foreground ,(face-background 'mode-line) :weight bold))))))
        (cond ((= nil) (-> 'char)
               (setq pos-init (point))
               (evil-visual-char) (set-tag "VISUAL"))
              ((= 'char) (-> 'line)
               (evil-visual-contract-region)
               (evil-visual-line) (set-tag "V-LINE"))
              ((= 'line) (-> 'block)
               (evil-visual-make-region pos-init evil-visual-point 'block)
               (add-hook 'minibuffer-setup-hook #'my-evil-visual-cycle-emulate-evil-block)
               (let ((suggest-key-bindings nil))
                 (call-interactively 'execute-extended-command))
               (evil-visual-block) (set-tag "V-BLOCK"))
              ((= 'block) (-> 'char)
               (evil-visual-contract-region)
               (evil-visual-char) (set-tag "VISUAL"))
              (t (error "Invalid state: %s" my-evil-visual-cycle-state))))))

  (add-hook 'evil-visual-state-exit-hook #'(lambda () (setq-local my-evil-visual-cycle-state nil)))
  (defun my-evil-visual-cycle-emulate-evil-block ()
    (remove-hook 'minibuffer-setup-hook #'my-evil-visual-cycle-emulate-evil-block)
    (insert "evil-visual-block")
    (setq unread-command-events (listify-key-sequence (kbd "RET"))))

  ;; ----------
  (lexical-let (so)
    (defun my-evil-visual-state-entry-hook-symbol-overlay-off ()
      (setq so (if symbol-overlay-mode +1 -1))
      (symbol-overlay-mode -1))
    (defun my-evil-visual-state-exit-hook-symbol-overlay-on ()
      (symbol-overlay-mode so)))

  (add-hook 'evil-visual-state-entry-hook #'my-evil-visual-state-entry-hook-symbol-overlay-off)
  (add-hook 'evil-visual-state-exit-hook #'my-evil-visual-state-exit-hook-symbol-overlay-on)

  ;; ----------
  (defvar my-evil-paste-rgn '())

  (defun my-adv-evil-paste-before--save-rgn (orig-fun &rest _arg)
    (let ((beg (point)))
      (apply orig-fun _arg)
      (setq my-evil-paste-rgn (cons (1+ (point)) beg))))

  (defun my-adv-evil-paste-after--save-rgn (orig-fun &rest _arg)
    (let ((beg (point)))
      (apply orig-fun _arg)
      (setq my-evil-paste-rgn (cons beg (1+ (point))))))

  ;; (defun my-adv-counsel-yank-pop--oeverwrite (orig-fun &rest _arg)
  ;;   "Delete the region before inserting poped string."
  ;;   (cond ((and evil-mode (eq 'visual evil-state))
  ;;          (let ((beg (copy-marker (region-beginning) t))
  ;;                (end (copy-marker (region-end) t)))
  ;;            (apply orig-fun _arg)
  ;;            (delete-region beg end)))
  ;;         ((and evil-mode
  ;;               (or (eq last-command 'evil-paste-before) (eq last-command 'evil-paste-after)))
  ;;          (goto-char (line-end-position))
  ;;          (apply orig-fun _arg)
  ;;          (delete-region (car my-evil-paste-rgn) (cdr my-evil-paste-rgn)))
  ;;         (t (apply orig-fun _arg))))

  (advice-add 'evil-paste-before :around #'my-adv-evil-paste-before--save-rgn)
  (advice-add 'evil-paste-after  :around #'my-adv-evil-paste-after--save-rgn)
  ;; (advice-add 'counsel-yank-pop  :around #'my-adv-counsel-yank-pop--oeverwrite)

  ;; ----------
  ;; exclude new line when v$
  ;; https://github.com/emacs-evil/evil/issues/897
  (setq evil-v$-gets-eol nil)

  ;; re-difined
  (evil-define-motion evil-end-of-line (count)
    "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
    :type inclusive
    (move-end-of-line count)
    (when evil-track-eol
      (setq temporary-goal-column most-positive-fixnum
            this-command 'next-line))
    ;; (unless (evil-visual-state-p)
    (unless (and (evil-visual-state-p) evil-v$-gets-eol)    ;; mod
      (evil-adjust-cursor)
      (when (eolp)
        ;; prevent "c$" and "d$" from deleting blank lines
        (setq evil-this-type 'exclusive))))

  ;; ----------
  (add-hook 'evil-visual-state-entry-hook #'(lambda () (show-paren-mode -1)))
  (add-hook 'evil-visual-state-exit-hook  #'(lambda () (show-paren-mode 1)))

  ;; re-defined
  (evil-define-command evil-quit (&optional force)
    (nop))        ;; do *not* exit from emacs after :q and :wq

  ;; paste in evil-normal-state
  ;; (evil-define-command my-evil-normal-paste-to-prev-line ()
  ;;   (if (evil-visual-state-p)
  ;;       (call-interactively 'evil-paste-before)
  ;;     (evil-first-non-blank)
  ;;     (yank)
  ;;     (newline)
  ;;     (call-interactively #'evil-indent-line)))
  ;;
  ;; (define-key evil-normal-state-map "P" #'my-evil-normal-paste-to-prev-line)

  ;; (evil-define-command my-evil-normal-paste-to-next-line ()
  ;;   (if (evil-visual-state-p)
  ;;       (call-interactively 'evil-paste-after)
  ;;     (end-of-line)
  ;;     (newline)
  ;;     (call-interactively #'evil-indent-line)
  ;;     (yank)))
  ;;
  ;; (define-key evil-normal-state-map "p" #'my-evil-normal-paste-to-next-line)

  (fset 'evil-backward-section-begin #'nop)     ;; disabled

  (defun my-adv--evil-join--delete-space (&rest _)
    (when (eq (char-after) ?\ )
      (delete-char 1)))
  (advice-add 'evil-join :after #'my-adv--evil-join--delete-space)

  ;; moving without no message such as 'Beginning of buffer' or 'End of buffer'
  ;; (defun no-error-evil-cmd (func &optional count crossline)
  ;;   (funcall func count crossline t))

  ;; (defun my-adv-evil-search-next/previous-no-wrap (orig-fun &rest _arg)
  ;;   "Disable `evil-wrap-search' before this command"
  ;;   (let ((evil-search-wrap nil))
  ;;         (apply orig-fun _arg)))
  ;;
  ;; (advice-add 'evil-search-next :around #'my-adv-evil-search-next/previous-no-wrap)
  ;; (advice-add 'evil-search-previous :around #'my-adv-evil-search-next/previous-no-wrap)

  (define-key evil-normal-state-map "h" #'(lambda () (interactive) (evil-backward-char 1 nil t)))
  (define-key evil-normal-state-map "j" #'(lambda () (interactive) (evil-line-move 1 t)))
  (define-key evil-normal-state-map "k" #'(lambda () (interactive) (evil-line-move -1 t)))
  (define-key evil-normal-state-map "l" #'(lambda () (interactive) (evil-forward-char 1 nil t)))
)

;; ----------------------------------------------------------------------
(use-package evil-collection
  ;; :disabled
  :after evil
  :ensure t
  :config
  (evil-collection-init '(edebug dired neotree slime help calc ediff magit))

  ;; optional: this is the evil state that evil-magit will use
  (setq evil-magit-state 'normal)
  ;; optional: disable additional bindings for yanking text
  (setq evil-magit-use-y-for-yank nil)
  ;; (require 'evil-collection-magit)

  (evil-collection-define-key 'normal 'dired-mode-map
    [return]   'dired-open-in-accordance-with-situation
    [right]    'dired-open-in-accordance-with-situation
    "."        'dired-open-in-accordance-with-situation
    [left]     'kill-current-buffer-and-dired-up-directory
    "H"        'kill-current-buffer-and-dired-up-directory
    "L"        'kill-current-buffer-and-dired-up-directory
    "q"        'kill-current-buffer
    "r"        'revert-buffer)                                    ; reload
)

;; ----------------------------------------------------------------------
(use-package evil-little-word
  :load-path "elisp"
  :config
  ;; (defalias #'forward-evil-word #'forward-evil-symbol) ;; CAUTION!! `'evil-little-word' does NOT working

(defun my-forward-evil-symbol ()
  (interactive)
  (unless (= (char-syntax (char-after)) ?w)
    (forward-char +1))
  (forward-evil-symbol +2)
  (forward-evil-symbol -1))

(defun my-backward-evil-symbol ()
  (interactive)
  (forward-evil-symbol -1))

(defun my-forward-evil-symbol-end ()
  (interactive)
  (let ((pt (point)))
    (unless (and (= (char-syntax (char-after)) ?w)
                 (= (char-syntax (char-after (1+ (point)))) ?w))
      (forward-char +1))
    (forward-evil-symbol +1)
    (evil-backward-little-word-end)))

(defun my-evil-forward-little-word-begin ()
    (interactive)
    (evil-forward-little-word-begin +1)
    (unless (= (char-syntax (char-after)) ?w) (forward-char +1))
    )

(defun my-evil-backward-kill-word ()
  (interactive)
  (let ((end (point)))
    (forward-evil-little-word -1)
    (evil-delete (point) end)))

  ;; (define-key evil-normal-state-map (kbd "w") 'my-forward-evil-symbol)
  ;; (define-key evil-normal-state-map (kbd "b") 'my-backward-evil-symbol)
  ;; (define-key evil-normal-state-map (kbd "e") 'my-forward-evil-symbol-end)
  (evil-define-key 'motion prog-mode-map (kbd "g w") 'my-evil-forward-little-word-begin)
  (evil-define-key 'normal prog-mode-map (kbd "g w") 'my-evil-forward-little-word-begin)
  (evil-define-key 'motion prog-mode-map (kbd "g e") 'evil-forward-little-word-end)
  (evil-define-key 'motion prog-mode-map (kbd "g b") 'evil-backward-little-word-begin)

  (global-set-key "\M-h" 'my-evil-backward-kill-word)
  )

;; ----------------------------------------------------------------------
(use-package evil-cleverparens
  :config
  (require 'evil-cleverparens-text-objects)

  )
;; ----------------------------------------------------------------------
(use-package evil-escape
  :disabled
  :after evil
  :diminish evil-escape-mode
  :config
  (evil-escape-mode 1)
  (setq-default evil-esc-delay 0)
  (setq-default evil-escape-delay 0.3)
  (setq-default evil-escape-key-sequence "jj")
  (setq-default evil-escape-excluded-states '(normal visual multiedit emacs motion))
  )

;; ----------------------------------------------------------------------
(use-package evil-lion
  :disabled
  :ensure t
  :after evil
  :config
  (evil-lion-mode)
)

;; ----------------------------------------------------------------------
(use-package evil-matchit
  :ensure t
  :hook ((prog-mode . turn-on-evil-matchit-mode))
  :init
  (setq evilmi-shortcut "]")
  ;; (global-evil-matchit-mode 1)

  )

;; ----------------------------------------------------------------------
(use-package orderless
  :custom
  `((completion-styles '(orderless partial-completion basic))
    (orderless-matching-styles
     . '(orderless-prefixes
         orderless-flex
         orderless-regexp
         orderless-initialism
         orderless-literal)))
  )

(use-package marginalia)

(use-package consult
  ;; :disabled
  :config
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  (defun my-consult-after-init-hook ()
    (vertico-mode)
    (marginalia-mode)
    (savehist-mode))        ;; Verticoの順番を永続化する

  (add-hook 'after-init-hook #'my-consult-after-init-hook)

  ;; (setq consult-preview-key (kbd "C-l"))
  (setq consult-preview-key '(:debounce 0.1 any))

  (setq my-consult-ripgrep-exclude-list-orig
    '("#*#"
      ".eslintrc.*" "node_modules/**"
      "__*__/**"
      ".DS_Store"))

  (defvar my-consult-ripgrep-exclude-list '())
  (defun my-consult-ripgrep-exclude-list-initialize-1 ()
      (setq my-consult-ripgrep-exclude-list my-consult-ripgrep-exclude-list-orig))

  (defun my-consult-ripgrep-exclude-list-initialize ()
    (interactive)
    (my-consult-ripgrep-exclude-list-initialize-1))

  (defun my-consult-ripgrep-exclude-add-1 (s)
    (add-to-list my-consult-ripgrep-exclude-list s))

  (defun my-consult-ripgrep-exclude-add (&optional s)
    (interactive)
    (if (interactive-p)
        ()      ;; todo
      (my-consult-ripgrep-exclude-add-1 s)))

  (defun my-consult-ripgrep-exclude-add-via-vertico ()
    (interactive)
    ;; (let ((s (consult--read nil)))      ;; todo
    ;; todo
      (my-consult-ripgrep-exclude-add))
  ;; )

  ;; (define-key vertico-map (kbd "") #'my-consult-ripgrep-exclude-add-via-vertico)

  (setq consult-ripgrep-args-orig
    "--no-ignore --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number")

  (setq consult-ripgrep-args
        (concat "rg "
                (mapconcat 'concat
                           (mapcar #'(lambda (s) (format "-g !%s" s)) my-consult-ripgrep-exclude-list)
                           " ")
                " "
                consult-ripgrep-args-orig
                ))

  ;; (setq consult-ripgrep-args (concat "rg " "-g !#*# " consult-ripgrep-args-orig " ."))

  (defun my-consult-ripgrep (&optional parg dir initial)
    "`consult-ripgrep` with symbol-at-point.
Besides, it can be Specified top directory to search using prefix-argument, e.g. C-u."
    (interactive "p")
    (setq initial (thing-at-point 'symbol))
    (setq dir (pcase parg
                (1 nil)               ;; not given prefix-arg
                (_ (let ((insert-default-directory t))
                     (read-directory-name "Ripgrep Dir: ")))))
    (consult-ripgrep dir initial))

  (defun my-consult-line-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))

  (defun my-consult-apropos-symbol-at-point ()
    (interactive)
    (consult-apropos (thing-at-point 'symbol)))

  (defun my-consult-buffer ()
    (interactive)
    (consult-buffer '(consult--source-hidden-buffer consult--source-buffer)))

  ;; mod
  ;; from consult.el
  (defun my-consult-apropos-at-point ()
  "Select pattern and call `apropos'.

The default value of the completion is the symbol at point. As a better
alternative, you can run `embark-export' from commands like `M-x' and
`describe-symbol'."
  (interactive)
  (let ((pattern
         (consult--read
          obarray
          :prompt "Apropos: "
          :predicate (lambda (x) (or (fboundp x) (boundp x) (facep x) (symbol-plist x)))
          :history 'consult--apropos-history
          :category 'symbol
          ;; :default (thing-at-point 'symbol))))       ;; mod
          :initial (thing-at-point 'symbol))))          ;; mod
    ;; (when (string= pattern "")                       ;; mod
    ;;   (user-error "No pattern given"))               ;; mod
    (apropos pattern)))

  ;; fdコマンドを使ってconsult-findを利用する
  (setq consult-find-command "fd --color=never --full-path ARG OPTS")

  ;; affe (affe-grep, affe-find)
  ;; https://blog.tomoya.dev/posts/a-new-wave-has-arrived-at-emacs/#affe%E3%82%92%E4%BD%BF%E3%81%A3%E3%81%9F%E3%83%95%E3%82%A1%E3%82%B8%E3%83%BC%E3%83%9E%E3%83%83%E3%83%81%E3%83%B3%E3%82%B0
  (setq
   ;; Orderlessを利用する
   affe-highlight-function 'orderless-highlight-matches
   affe-regexp-function 'orderless-pattern-compiler
   ;; findのかわりにfdを利用する
   affe-find-command "fd --color=never --full-path")

  (defun my-consult-ripgrep-command (parg)
    (interactive "P")
    (if parg
        (affe-grep)
      (my-consult-ripgrep)))

  (defun my-consult-find-command (parg)
    (interactive "P")
    (if parg
        (affe-find)
      (consult-find)))

  :bind (("M-r" . consult-recent-file)
         ("M-l" . my-consult-line-at-point)
         ("M-a" . my-consult-apropos-at-point)
         ("M-o" . my-consult-ripgrep-command)
         ("C-x C-g" . my-consult-find-command)
         ("M-e" . embark-act)
         ("C-x C-b" . my-consult-buffer))
 )

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark
  :bind (:map embark-general-map
           ("?" . embark-keymap-help))
  :config
  ;; face
  (set-face-foreground 'embark-collect-group-title        (face-foreground 'font-lock-comment-face))
  (set-face-foreground 'embark-collect-group-separator    (face-foreground 'font-lock-comment-face))
  (set-face-foreground 'embark-verbose-indicator-shadowed (face-foreground 'font-lock-comment-face))

  ;; (define-key embark-general-map "E" nil)   ;; embark-export
  (define-key embark-general-map (kbd "M-e") 'embark-export)   ;; embark-export

  ;; re-define
  (defvar-keymap embark-file-map        ;; todo remove duplicates
    :doc "Keymap for Embark file actions."
    :parent embark-file-map
    "RET" #'find-file
    "f"   #'find-file
    "F"   nil                   ;; find-file-literally)
    "o" nil                   ;; find-file-other-window)
    "d" #'delete-file
    "D" #'delete-directory
    "r" #'rename-file
    "c" #'copy-file
    ;; ("j" embark-dired-jump)
    "!" nil                   ;; shell-command)
    "&" nil                   ;;async-shell-command)
    "$" nil                   ;; embark-eshell)
    "<" nil                   ;; insert-file)
    "m" nil                   ;; chmod)
    "M" #'chmod
    "=" nil                   ;; ediff-files)
    "+" #'make-directory
    "\\" nil                  ;; embark-recentf-remove)
    "I" nil                   ;; embark-insert-relative-path)
    "W" nil                   ;; embark-save-relative-path)
    ;; ("e" eww-open-file)
    "L" nil                   ;; load-file)
    "B" nil                   ;; byte-compile-file)
    ;; ("R" byte-recompile-directory)
    "v" nil                   ;; 'embark-vc-file-map)
    "C-l" #'consult-preview-at-point
    )

  ;; re-define keymap for embark-act(M-e)
  (define-key embark-file-map "f" nil)  ;; find-file)
  (define-key embark-file-map "F" nil)  ;; find-file-literally)
  (define-key embark-file-map "o" nil)  ;; find-file-other-window)
  (define-key embark-file-map "j" nil)  ;; embark-dired-jump)
  (define-key embark-file-map "!" nil)  ;; shell-command)
  (define-key embark-file-map "&" nil)  ;; async-shell-command)
  (define-key embark-file-map "$" nil)  ;; embark-eshell)
  (define-key embark-file-map "<" nil)  ;; insert-file)
  (define-key embark-file-map "\\" nil) ;; embark-recentf-remove)
  (define-key embark-file-map "W" nil)  ;; embark-save-relative-path)
  (define-key embark-file-map "e" nil)  ;; eww-open-file)
  (define-key embark-file-map "l" nil)  ;; load-file)
  (define-key embark-file-map "L" nil)  ;; load-file)
  (define-key embark-file-map "B" nil)  ;; byte-compile-file)
  (define-key embark-file-map "R" nil)  ;; byte-recompile-directory)
  (define-key embark-file-map "v" nil)  ;; 'embark-vc-file-map)
  (define-key embark-file-map "x" nil)  ;; #'consult-file-externally)
  )

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package vertico
  :config
  (setq vertico-count 20)
  (setq completion-styles '(substring orderless basic))     ;; In order to support completing prefixes, combine
                                                            ;; orderless with substring in your completion-styles

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)   ;; for vertico-repeat (M-z)

  ;; override from vertico.el for consult-ripgrep
  (custom-set-variables '(vertico-group-format (concat ;; #("    " 0 4 (face vertico-group-separator))
                                  #(" %s " 0 4 (face vertico-group-title))
                                  #(" " 0 1 (face vertico-group-separator display (space :align-to right))))))

  ;; 補完候補以外を選びづらい問題(updated) https://misohena.jp/blog/2022-08-15-transition-ivy-to-vertico.html
  ;; 候補更新時に最初の候補を選択しない
  ;; (setq vertico-preselect 'prompt)
  ;; ただし、require-matchがt(やそれに類するもの)で入力が空ではなくマッ
  ;; チする候補がある場合は、その候補の先頭を選択する。
  (defun my-vertico--recompute (orig-fun pt content &rest args)
    (let ((result (apply orig-fun pt content args)))
      (if (and (not (equal content "")) ;;入力が空の時は(require-matchであっても)defaultまたはnilを返すことになっている。
               (> (alist-get 'vertico--total result) 0)
               ;; completing-readの説明によれば
               ;; nil,confirm,confirm-after-completion以外はtのように
               ;; 振る舞うべき。
               (not (memq minibuffer--require-match
                          '(nil confirm confirm-after-completion))))
          (setf (alist-get 'vertico--index result) 0))
      result))
  (advice-add #'vertico--recompute :around #'my-vertico--recompute)

  (defun my-vertico-tab ()
    "My customised function for tab completion"
    (interactive)
    (let ((old (minibuffer-contents-no-properties)))
      (and (progn (minibuffer-complete)
                  (string= old (minibuffer-contents-no-properties)))
           (pcase (length vertico--candidates)
             (0 (nop))
             (1 (vertico-first)
                (vertico-insert))
             (_ (if (and (not (eq last-command 'find-file))
                         ())        ;; todo
                    ()
                (vertico-exit)))))))

  :bind (:map vertico-map
         ("TAB" . minibuffer-complete)
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("M-y" . vertico-save)

         :map evil-motion-state-map
         ("M-z" .  vertico-repeat))
  )

(use-package vertico-directory
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  (setq vertico-directory-up ())
  (defun my-vertico-directory-up ()
    (interactive)
    (if (eq 'file (vertico--metadata-get 'category))
        (let ((pt (point)))
          (call-interactively #'vertico-directory-up)    ;; first, trying this
          (when (= pt (point))
            (backward-kill-word 2)))                     ;; do this if it does not move the point
      (backward-kill-word 1)))

  (defun my-vertico-directory-insert-current-candidate ()
    (interactive)
    (let ((delim "/"))      ;; todo system types
      (pcase vertico--total
        (0 (nop t))
        (1 (if (string= (car (last (split-string (minibuffer-contents) delim)))
                        (car vertico--candidates))
               (vertico-exit)
             (vertico-first)
             (vertico-insert)))
        (_ (if (< vertico--index 0)
               (vertico-first)    ;; goto first cand unless selected yet
             (vertico-insert))))))

  :bind (:map vertico-map
         ("TAB" . my-vertico-directory-insert-current-candidate)
         ;; ("M-h" . my-vertico-directory-up)
         ("M-h" . vertico-directory-up)
         ;; ("M-l" . vertico-directory-enter)  ;; enter dired
         ("M-d" . vertico-directory-delete-char))
  )

(use-package affe
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-.")

  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  )

; (use-package avy
;   :config
;   (setq avy-timeout-seconds 0.7)
;   )

;; ----------------------------------------------------------------------
;; (use-package ivy
;;   :disabled
;;   :diminish counsel-mode
;;   :init
;;   (ivy-mode 1)

;;   :config
;;   (counsel-mode 1)
;;   (setq ivy-use-virtual-buffers t                                ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
;;         ivy-height 20
;;         ivy-initial-inputs-alist nil                             ;; no regexp by default
;;         ivy-on-del-error-function 'ignore
;;         ivy-extra-directories nil                                ;; '("../")
;;         ivy-re-builders-alist '((t . ivy--regex-ignore-order))   ;; configure regexp engine. allow input not in order
;;         avy-timeout-seconds 0.4
;;         counsel-find-file-ignore-regexp "\\.ex4$\\|\\.elc\\'\\|\\.DS_Store\\|^#.+#$"
;;         ;; ivy-display-style t
;;   )

;;   ;; color
;;   ;; (set-face-foreground 'ivy-action (mycolor 'red))
;;   ;; (set-face-background 'ivy-confirm-face "'green")
;;   ;; (set-face-background 'ivy-current-match "#0a5770")
;;   ;; (set-face-attribute  'ivy-current-match nil
;;   ;;                   :foreground (mycolor 'black) :background (mycolor 'red))
;;   ;; (set-face-background 'ivy-cursor "'brown")
;;   ;; (set-face-background 'ivy-highlight-face "'SkyBlue")
;;   ;; (set-face-background 'ivy-match-required-face "#ce123e")

;;   ;; (set-face-background 'ivy-minibuffer-match-face-1 "#cc8800")
;;   ;; (set-face-background 'ivy-minibuffer-match-face-2 "#0a5770")
;;   ;; (set-face-background 'ivy-minibuffer-match-face-3 "'DarkGray")
;;   ;; (set-face-background 'ivy-minibuffer-match-face-4 "'DarkCyan")
;;   ;; (set-face-attribute 'ivy-minibuffer-match-face-1 nil :foreground nil :background nil :bold t :underline t)
;;   ;; (copy-face 'ivy-minibuffer-match-face-1 'ivy-minibuffer-match-face-2)
;;   ;; (copy-face 'ivy-minibuffer-match-face-1 'ivy-minibuffer-match-face-3)
;;   ;; (copy-face 'ivy-minibuffer-match-face-1 'ivy-minibuffer-match-face-4)

;;   ;; disable mouse hover in minibuffer
;;   ;; mod
;;   (defun ivy--format-minibuffer-line (str)
;;   "Format line STR for use in minibuffer."
;;   (let* ((str (ivy-cleanup-string str))
;;          (str (if (eq ivy-display-style 'fancy)
;;                   (funcall ivy--highlight-function (copy-sequence str))
;;                 (copy-sequence str))))
;;     ;; (add-text-properties
;;     ;;  0 (length str)
;;     ;;  '(mouse-face
;;     ;;    ivy-minibuffer-match-highlight
;;     ;;    help-echo
;;     ;;    (format
;;     ;;     (if tooltip-mode
;;     ;;         "mouse-1: %s\nmouse-3: %s"
;;     ;;       "mouse-1: %s   mouse-3: %s")
;;     ;;     ivy-mouse-1-tooltip ivy-mouse-3-tooltip))
;;     ;;  str)
;;     (let ((annotation-function (plist-get completion-extra-properties :annotation-function)))
;;       (if annotation-function
;;           (concat str (funcall annotation-function str))
;;         str))))

;;   (copy-face 'ivy-current-match 'ivy-prompt-match)
;;   (set-face-background 'ivy-modified-buffer "#008800")
;;   (set-face-foreground 'ivy-remote (mycolor 'pink))
;;   (set-face-foreground 'ivy-subdir (mycolor 'blue))
;;   (set-face-foreground 'ivy-virtual (mycolor 'orange))

;;   (defalias 'list-faces 'counsel-faces)
;;   (fset 'list-faces-display nil)

;;   (defun my-ivy-done ()
;;     (interactive)
;;     (if (and (boundp 'my-ivy-immediate-flag) (eq my-ivy-immediate-flag t))
;;         (ivy-immediate-done)
;;       (ivy-done)))

;;   (define-key ivy-minibuffer-map [(return)] 'my-ivy-done)

;;   ;; (defun my-counsel-find-file ()
;;   ;;   (interactive)
;;   ;;   (let ((my-ivy-immediate-flag t))
;;   ;;     (call-interactively
;;   ;;      (cond ((and (fboundp 'counsel-gtags-find-file) (locate-dominating-file default-directory "GTAGS"))
;;   ;;             'counsel-gtags-find-file)
;;   ;;            ((and (fboundp 'magit-find-file) (locate-dominating-file default-directory ".git"))
;;   ;;             'magit-find-file)
;;   ;;            (t 'counsel-find-file)))))


;;   ;; refrect .ignore to the root of the project
;;   (setq counsel-git-cmd "rg --files")

;;   (defvar my-counsel-rg-exclude-list '(".DS_Store" ".eslintrc.*" "node_modules/**" "__*__/**"))

;;   (defun my-counsel-rg (&optional initial-input)
;;     "counsel-rg at point in the specified directory"
;;     (interactive)
;;     (let ((my-ivy-immediate-flag t))
;;       (ivy-read "rg dir: " 'read-file-name-internal
;;               :matcher #'counsel--find-file-matcher
;;               :initial-input initial-input
;;               :action #'my-counsel-rg-1
;;               :preselect (counsel--preselect-file)
;;               :require-match 'confirm-after-completion
;;               :history 'file-name-history
;;               :keymap counsel-find-file-map
;;               :caller 'my-counsel-rg)))

;;   (defvar my-counsel-rg-exe "")  ;; will be overridden by _windows.el or _mac.el

;;   (defun my-counsel-rg-1 (input)
;;     (let* ((ignores (mapconcat 'concat (mapcar #'(lambda (e) (format "-g '!%s'" e)) my-counsel-rg-exclude-list)
;;                                " "))
;;            (rg-opts (format " -i --smart-case --no-heading --line-number %s " ignores))
;;            (counsel-ag-base-command (concat my-counsel-rg-exe
;;                                             ;; " -i --smart-case --no-heading --line-number --color never %s ."))
;;                                             rg-opts "%s ."))
;;            (initial-input (if (symbol-at-point) (symbol-name (symbol-at-point)) ""))
;;            ;; (input-list (split-string input "\t"))
;;            (input-list (split-string input " "))
;;            (initial-directory (car input-list))
;;            (extra-rg-args (mapconcat #'identity
;;                                      (mapcar #'(lambda (x) (format "-g '*%s'" x))   ;; e.g.: /hoge-dir .h .c
;;                                              (cdr input-list))
;;                                      " "))
;;            (rg-prompt (car input-list))
;;            (my-ivy-immediate-flag nil))
;;       ;; (message "%s, %s" rg-prompt extra-rg-args)
;;       (counsel-ag initial-input initial-directory extra-rg-args rg-prompt)))

;;   (cl-pushnew 'my-counsel-rg-1 ivy-highlight-grep-commands)

;;   (defun my-find ()
;;     (interactive)
;;     (let ((my-ivy-immediate-flag t))
;;       (ivy-read "Find: " 'read-file-name-internal
;;                 :matcher #'counsel--find-file-matcher
;;                 :action #'my-find-1
;;                 :preselect (counsel--preselect-file)
;;                 :require-match 'confirm-after-completion
;;                 :history 'file-name-history
;;                 :keymap counsel-find-file-map
;;                 :caller 'my-find)))

;;   (defun my-find-1 (dir)
;;     (let* ((my-ivy-immediate-flag nil))
;;       (counsel-file-jump "" dir)))

;;   (defun my-dired ()
;;     (interactive)
;;     (let ((my-ivy-immediate-flag t))
;;       (ivy-read "Dired: " 'read-file-name-internal
;;                 :matcher #'counsel--find-file-matcher
;;                 :action #'my-dired-1
;;                 :preselect (counsel--preselect-file)
;;                 :require-match 'confirm-after-completion
;;                 :history 'file-name-history
;;                 :keymap counsel-find-file-map
;;                 :caller 'my-dired)))

;;   (defun my-dired-1 (dir)
;;     (let ((my-ivy-immediate-flag nil))
;;       (dired dir)))

;;   ;; re-defun from counsel.el
;;   ;; Usage: C-x C-f M-x m
;;   (defun counsel-find-file-move (x)
;;     "Move or rename file X."
;;     (let* ((name (if (and ivy--directory (string-match "/$" (ivy-state-current ivy-last)))
;;                      (substring (ivy-state-current ivy-last) 0 -1)
;;                    (ivy-state-current ivy-last)))
;;            (new-name (expand-file-name (read-no-blanks-input (format "mv \"%s\" -> " name) name))))
;;       (require 'dired-aux)
;;       (dired-rename-file name new-name 1)))

;;   ;--------------
;;   (defun my-counsel-ibuffer-kill-buffer (x)
;;     "Kill buffer X."
;;     (let ((buf-name (cdr x)))
;;       (condition-case err
;;           (kill-buffer buf-name)
;;         (error (error "Can not kill buffer: %s" buf-name)))))

;;   (ivy-set-actions
;;    'counsel-ibuffer
;;    '(("k" my-counsel-ibuffer-kill-buffer "kill buffer")))

;;   ;--------------
;;   (defun my-ivy-find-file-copy-file-name-to-kill-ring (x)
;;     "Copy file name to kill ring."
;;     (let ((name (file-name-nondirectory x)))
;;       (kill-new name)))

;;   (ivy-set-actions
;;    'counsel-find-file
;;    '(("w" my-ivy-find-file-copy-file-name-to-kill-ring "copy file name")))

;;   ;--------------
;;   ;; mod from counsel.el
;;   (defun counsel-find-file-delete (x)
;;     "Move file X to backup directory instead of deleting it."
;;     (if (and (stringp my-backup-directory)
;;              (file-exists-p my-backup-directory))
;;         (let ((dest (my-backup-get-suffixed-file-name
;;                      (path-join my-backup-directory (file-name-nondirectory x)))))
;;           (rename-file x dest)
;;           (message "Moved to: %s" dest))
;;       (error "Invalid my-backup-directory: %s" my-backup-directory)))

;;   ;--------------
;;   (defun my-counsel-write-file ()
;;     "Supported creating unexisted parent-directories and
;; using a new file name regardless of the candidates"
;;     (interactive)
;;     (let ((ivy-minibuffer-map (make-sparse-keymap)))
;;       (define-key ivy-minibuffer-map (kbd "RET") #'ivy-immediate-done)
;;       (ivy-read "Write file to: "
;;                 #'read-file-name-internal
;;                 :preselect (or (buffer-file-name) "")
;;                 :history 'write-file-history
;;                 :action #'my-counsel-write-file-action-function
;;                 :caller 'my-counsel-write-file)))

;; (defun my-counsel-write-file-action-function (fn)
;;     (let ((dir (file-name-directory fn)))
;;       (cond ((file-exists-p fn)
;;              (if (y-or-n-p "Overwrite? ")
;;                  (write-file fn)
;;                (message "Canceled")))
;;             ((not (file-exists-p dir))
;;              (create-directory-recursive dir)
;;              (write-file fn))
;;             (t (write-file fn)))))

;; (defun win-path-p (path)
;;   (string= (substring (first dirs) -1 nil) ":"))

;; (defun win-ulp-path-p (path)
;;   (or (string= (substring (first dirs) 0 1) "\\\\")
;;       (string= (substring (first dirs) 0 1) "////")))

;; (defun create-directory-recursive (path)
;;   (let* ((slash "/")
;;          (full-path (expand-file-name path))
;;          (dirs (split-string full-path slash t))
;;          (s ""))
;;     (cond ((win-path-p path)
;;            (setq dirs (push (concat (first dirs) slash (second dirs)) (cl-subseq dirs 2 ))))
;;           ((win-ulp-path-p path)
;;            ())
;;           (t nil))
;;     (dolist (d dirs)
;;       (if (and (> (length d) 1) (string= (substring d 1 2) ":"))
;;           (setq s (concat s d))
;;         (setq s (concat s slash d)))
;;       (unless (file-exists-p s)
;;         (make-directory (directory-file-name s))
;;         (unless (file-directory-p s)
;;           (error "Can not create directory: %s" s))))))

;;   ;--------------
;; ;; todo
;;   (defun my-font-list ()
;;     "List font using ivy"
;;     (interactive)
;;     (ivy-read "Font: "
;;               (font-family-list)
;;               :require-match t
;;               :action (lambda (x) (insert x))
;;               :caller 'my-font-list))

;;   ;--------------
;;   :bind (("M-z"     . ivy-resume)
;;          ("M-r"     . counsel-recentf)
;;          ("M-o"     . my-counsel-rg)
;;          ("C-x C-g" . my-find)
;;          ("C-x C-b" . counsel-ibuffer)
;;          ("C-x C-w" . write-file)
;;          ;; ("C-x C-w" . my-counsel-write-file)
;;          ;; ("C-x C-f" . my-counsel-find-file)
;;          ;; ("C-s"     . swiper)

;;          :map ivy-minibuffer-map
;;          ;; ([remap ivy-done] . ivy-immediate-done)
;;          ([(return)] . my-ivy-done)
;;          ("C-j" . ivy-next-line)
;;          ("C-k" . ivy-previous-line)
;;          ("M-h" . ivy-backward-kill-word)
;;          ("C-o" . nil)
;;          ("M-x" . ivy-dispatching-done)                 ; M-o   --> M-x
;;          ("C-M-x" . ivy-dispatching-call)               ; C-M-o --> C-M-x
;;          ("M-j" . ivy-next-history-element)
;;          ("M-k" . ivy-previous-history-element)
;;          ("M-<down>" . ivy-next-history-element)
;;          ("M-<up>"   . ivy-previous-history-element)
;;          ;; ("C-f" . ivy-avy)

;;          :map counsel-find-file-map
;;          ("M-c" . ivy-immediate-done)                   ; M-c == M-RET

;;          :map counsel-mode-map
;;          ("M-RET" . ivy-immediate-done)

;;          :map evil-motion-state-map
;;          ("f" . avy-goto-char-timer))
;;   )

;; ----------------------------------------------------------------------
;; (use-package all-the-icons-ivy
;;   :disabled
;;   :init
;;   (setq all-the-icons-scale-factor 1.0)
;;   (defun all-the-icons-ivy-icon-for-file (s)
;;     "Return icon for filename S.
;; Return the octicon for directory if S is a directory.
;; Otherwise fallback to calling `all-the-icons-icon-for-file'."
;;     (cond
;;      ((string-match-p "\\/$" s)
;;       (all-the-icons-octicon "file-directory" :face 'all-the-icons-ivy-dir-face))
;;      (t (all-the-icons-icon-for-file s :v-adjust 0.02))))

;;   (all-the-icons-ivy-setup)
;;   )

;; ----------------------------------------------------------------------
;; (use-package counsel-etags
;;   :disabled
;;   ;; :diminish
;;   :after counsel

;;   )

;; ----------------------------------------------------------------------
;; (use-package counsel-gtags
;;   :disabled
;;   :after counsel evil
;;   :diminish '(counsel-gtags-mode . "Gtags")
;;   :hook ((c-mode . counsel-gtags-mode))
;;   :init
;;   ;; (add-hook 'c-mode-hook 'counsel-gtags-mode)

;;   :config
;;   (setq counsel-gtags-auto-update t
;;         counsel-gtags-path-style 'root)

;;   ;; (defun gtags-update ()
;;   ;;   (interactive)
;;   ;;   (let ((s (shell-command-to-string "global -uv")))
;;   ;;     (if (string-match "not found" s)
;;   ;;         (call-interactively 'helm-gtags-create-tags)
;;   ;;       (message "Updated GTAGS files."))))

;;   (defalias 'my-gtags-update 'counsel-gtags-update-tags)

;;   (defun my-gtags-create (rootdir)
;;     "Create tag database in ROOTDIR. Prompt for ROOTDIRif not given.  This command is asynchronous."
;;     (interactive (list
;;                   (let ((my-ivy-immediate-flag t))
;;                     (read-directory-name "GTAGS Dir: " nil nil t))))
;;     (let* ((default-directory rootdir)
;;            (proc-buf (get-buffer-create " *counsel-gtags-tag-create*"))
;;            (proc (start-file-process
;;                   "counsel-gtags-tag-create" proc-buf
;;                   "gtags" "-q" (concat "--gtagslabel=default"))))
;;       (set-process-sentinel
;;        proc
;;        (counsel-gtags--make-gtags-sentinel 'create))))
;;     (fset 'counsel-gtags-create-tags nil)               ; undefine original command

;;     (setenv "GTAGSLIBPATH" "/usr/local/Cellar/avr-gcc/7.3.0/avr/include") ; for qmk_firmware on Mac

;;   :bind (("C-x C-g" . counsel-gtags-find-file)
;;          :map evil-normal-state-map
;;          ("g t" . counsel-gtags-dwim)
;;          ;; ("g t" . counsel-gtags-find-definition)
;;          ("g r" . gtags-find-reference)
;;          ("g s" . gtags-find-symbol)
;;          ("g h" . counsel-gtags-go-backward))
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; なんかよくわからんコードの断片が残っているので、一旦コメントアウトしておく
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ----------------------------------------------------------------------
;; ;; which-func-mode
;;  (setq which-func-unknown "-"
;;        which-func-modes '(emacs-lisp-mode lisp-interaction-mode c-mode python-mode ruby-mode)
;;        which-func-format '(:propertize which-func-current face which-func))
;;
;;  (which-function-mode 0)        ;; global
;;
;;  ;; ----------------------------------------------------------------------
;;
;;  ;; (lisp-interaction-mode)                            ;; workaround for scratch-log
;;
;;
;;  ;; disable mhtml-mode so avoiding conflict with web-mode
;;  (delete-if #'(lambda (elm) (eq (cdr elm) 'mhtml-mode)) auto-mode-alist)
;;
;;  (setq enable-local-variables nil)  ;; disable "emacs the local variables list in..." when find-file
;;
;;
;; ;; ======================================================================
;; ;; auto-insert
;; (add-hook 'find-file-hook 'auto-insert)
;; (setq auto-insert-directory "~/.emacs.d/templates")
;; (defvar auto-insert-alist nil)
;; (setq auto-insert-alist (cons '("\\.mq4" . "mq4")
;;                                 auto-insert-alist))
;;
;; ;; ----------------------------------------------------------------------
;; ;; Stop "Active processes exist; kill them and exit anyway?"
;; (require 'cl-lib)
;; (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;;   "Prevent annoying \"Active processes exist\" query when you quit Emacs."
;;   (cl-letf (((symbol-function #'process-list) (lambda ())))
;;     ad-do-it))
;;
;; ;; fixme need this?
;; ;; ----------------------------------------------------------------------
;; ;; utility for use-package
;; (defun my-font-exists-p ($font-name)
;;   (if (null (x-list-fonts $font-name))
;;       nil t))

;; ----------------------------------------------------------------------
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  (setq wgrep-enable-key "r")

  ;; rg -> embark-export -> replace の手順
  ;; 1. `M-o`でrgする
  ;; 2. `M-e`でembarkに入る
  ;; 3. `M-e`でembark-exportする
  ;; 4. `r`を押してwgrepに入る
  ;; 5. `M-%`でquery-replaceする
  ;; 6. `C-c C-c`でファイルに反映する

  :bind (:map grep-mode-map
              ("C-o" . nil))
  )
;; ----------------------------------------------------------------------
(use-package diminish
  :config
  (defmacro diminish-minor-mode (file mode &optional new-name)
    "https://github.com/larstvei/dot-emacs/blob/master/init.org"
    `(with-eval-after-load ,file
       (diminish ,mode ,new-name)))

  (defmacro diminish-major-mode (hook new-name)
    `(add-hook ,hook #'(lambda ()
                         (setq mode-name ,new-name))))

  ;; minor mode
  (diminish-minor-mode "eldoc" 'eldoc-mode)
  (diminish-minor-mode "abbrev" 'abbrev-mode)
  (diminish-minor-mode "cwarn" 'cwarn-mode)
  (diminish-minor-mode "super-save" 'super-save-mode)
  (diminish-minor-mode "git-gutter" 'git-gutter-mode)
  ;; (diminish-minor-mode "ivy" 'ivy-mode)
  (diminish-minor-mode "yasnippet" 'yas-minor-mode)
  (diminish-minor-mode "symbol-overlay" 'symbol-overlay-mode)
  (diminish-minor-mode "hideif" 'hide-ifdef-mode)


  ;; major mode
  (diminish-major-mode 'emacs-lisp-mode-hook "Elisp")
  (diminish-major-mode 'lisp-interaction-mode-hook "LispInt")
  (diminish-major-mode 'js-mode-hook "JS")
  (diminish-major-mode 'dts-mode-hook "DTS")

  (defun flymake--transform-mode-line-format (ret)
    "Change the output of `flymake--mode-line-format'."
    (setf (seq-elt (car ret) 1) " ")
    (setf (seq-elt (second ret) 1) "")
    (setf (seq-elt (sixth ret) 1) "")
    ret)
  (advice-add #'flymake--mode-line-format
              :filter-return #'flymake--transform-mode-line-format)
)

;; ----------------------------------------------------------------------
(use-package hide-mode-line
  :if window-system
  :hook ((neotree-mode) . hide-mode-line-mode)
  )

;; ----------------------------------------------------------------------
(use-package all-the-icons
  :disabled t
  :if window-system
  :config
  (setq inhibit-compacting-font-caches t)
  (setq all-the-icons-color-icons nil)
  )

;; ----------------------------------------------------------------------
(use-package nerd-icons)

;; ----------------------------------------------------------------------
(defun my-font-lock-add-keywords-elisp ()
  (font-lock-add-keywords nil
                          '(("(\\(lambda\\|cons\\|car\\|cdr\\|nth\\|eq\\|equal\\|null\\|remove\\|delete
\\|mapc\\|mapcar\\|fset\\|set
\\|memq\\|member\\|delq\\|funcall\\|fboundp\\|list\\|add-to-list\\|concat\\|call-interactively
\\|assoc\\|rassoc\\|add-hook\\|remove-hook\\|define-key\\|global-set-key\\|local-set-key\\|define-key
\\|ad-activate\\|ad-enable-advice\\|ad-disable-advice\\|propertize\\|run-hooks\\)[ \t\n]" . font-lock-keyword-face))))

(add-hook 'emacs-lisp-mode-hook #'my-font-lock-add-keywords-elisp)
;; (add-hook 'emacs-lisp-mode-hook #'flymake-mode)
;; (add-hook 'lisp-interaction-mode-hook #'my-font-lock-add-keywords-elisp)
;; (add-hook 'lisp-interaction-mode-hook #'flymake-mode)   ;; これを削除すると .elファイルを開くときに↓のエラーが出るので注意
;;                                                         File mode specification error: (wrong-type-argument stringp

;; ----------------------------------------------------------------------
(use-package undo-tree
  :disabled t
  :load-path "local-fix/undo-tree"
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil)            ;; do not create xxx.~undo-tree~

  ;; (define-key undo-tree-map (kbd "C-?") 'nil)
  ;; (define-key undo-tree-map (kbd "C-r") 'nil)    ;; undo-tree-redo      FIXME: not work
  )

;; ----------------------------------------------------------------------
(use-package undo-fu)

;; ----------------------------------------------------------------------
(use-package neotree
  :disabled t
  :if window-system
  :after evil all-the-icons
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-show-hidden-files t)
  (setq neo-create-file-auto-open t)
  (setq neo-smart-open nil)
  (setq neo-persist-show t)
  (setq neo-keymap-style 'concise)

  (global-set-key (kbd "M-q") 'neotree-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "M-q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "q")   'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "l")   'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)

  (defun text-scale-twice ()
    (interactive)
    (text-scale-adjust 0)
    (text-scale-decrease 1))
  (add-hook 'neo-after-create-hook #'(lambda (_)(call-interactively 'text-scale-twice)))

  )

;; ----------------------------------------------------------------------
(use-package projectile
  :defer 100
  :config
  (projectile-mode +1)
  ;; Recommended keymap prefix on macOS
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

  )

;; ----------------------------------------------------------------------
(use-package swap-buffers
  :config
  ;; override
  (defun swap-buffers-display-number (win num)
  "Create an overlay to diplay in the WIN window with label NUM while choosing."
  (let* ((label (swap-buffers-label num))
         (buffer (window-buffer win))
         (wp (window-point win))
         (ol (make-overlay wp wp buffer)))
    ;; (overlay-put ol 'before-string (propertize label 'face (list :height 4.0 :foreground "red")))
    (overlay-put ol 'before-string (propertize label 'face 'swap-buffers-label-face))
    (overlay-put ol 'window win)
    ol))

  (defface swap-buffers-label-face  `((t (:height 4.0 :foreground "red"))) "Face that is used as window indicators by swap-buffers.")

  :bind (("M-C-o" . swap-buffers))
  )
;; ----------------------------------------------------------------------
(use-package recentf
  ;; :defer 200
  :config
  (setq recentf-max-saved-items 5000) ;; 履歴保存の数
  ;; (setq recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない network経由のときに有効にする
  (setq recentf-exclude '("/recentf" ".recentf" ".my-save-frame" "batch-script.el"
                          ".emacs.d/bookmarks" "**/*.el.gz" "/eln-cache/*"
                          "autoloads.el" "compile_commands.json"))
  ;; (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  (let ((inhibit-message t))
    (recentf-mode 1))
  )

;; ----------------------------------------------------------------------
(use-package smartparens
  :diminish smartparens-mode
  :config
  (use-package smartparens-config
    :after smartparens
    :hook (prog-mode-hook . smartparens-mode))

  (sp-with-modes '(rust-mode rust-ts-mode rustic-mode
                             ruby-mode)
    (sp-local-pair "|" "|"
                   :unless '(sp-in-comment-p sp-in-string-quotes-p sp-in-rust-lifetime-context)
                   :post-handlers'(:rem sp-escape-quotes-after-insert)))


  ;; :config
  ;; (smartparens-global-mode)
  ;; (show-smartparens-global-mode t)
  ;; (setq sp-autoinsert-pair nil)
  ;; (setq sp-autodelete-wrap nil)
  ;; (setq sp-autodelete-pair nil)
  ;; (setq sp-autodelete-closing-pair nil)
  (set-face-background 'sp-show-pair-match-face "#4C6DA6")

  ;; (ad-disable-advice 'delete-backward-char 'before 'sp-delete-pair-advice) ; disable C-h
  ;; (ad-activate 'delete-backward-char)

  ;; ;; use show-paren to hilight content in parenthesis
  ;; (setq show-paren-style 'expression)
  ;; ;; faces like `show-paren-match' is used from theme
  ;; (setq show-paren-delay 0.2)
  ;; (show-paren-mode 1)

  ;; ;; depends on modes
  ;; (sp-with-modes '(lisp-mode lisp-interaction-mode emacs-lisp-mode)
  ;;  (sp-local-pair "'" nil :actions nil)
  ;;  (sp-local-pair "`" nil :actions nil))
  )

;; ----------------------------------------------------------------------
(use-package expand-region
  :after evil symbol-overlay
  :config
  (push 'er/mark-outside-pairs er/try-expand-list)
  (setq expand-region-smart-cursor nil)
  (setq expand-region-subword-enabled t)
  (setq expand-region-show-usage-message nil)
  ;; (setq expand-region-autocopy-register "e")
  ;; (setq expand-region-autocopy-kill-ring t)
  (define-key evil-normal-state-map (kbd "=") 'er/expand-region)
  (define-key evil-normal-state-map (kbd "-") 'er/contract-region)
  (define-key evil-visual-state-map (kbd "=") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "-") 'er/contract-region)

  (add-hook 'web-mode-hook #'(lambda () (er/enable-mode-expansions 'web-mode 'er/add-web-mode-expansions)))
  )

;; ----------------------------------------------------------------------
(use-package rainbow-delimiters
  ;; :disabled
  :after cl-lib color
  :hook ((prog-mode . rainbow-delimiters-mode))
  :config
  (setq rainbow-delimiters-outermost-only-face-count 1)
  ;; (set-face-bold 'rainbow-delimiters-depth-1-face t)
  )

;; ----------------------------------------------------------------------
(use-package rainbow-mode
  :diminish rainbow-mode
  ;; :hook ((prog-mode . rainbow-mode))
  :config
  (setq rainbow-html-colors nil)
  )

;; ----------------------------------------------------------------------
(use-package symbol-overlay
  :ensure t
  :hook (prog-mode . symbol-overlay-mode)
  :config
  (setq symbol-overlay-idle-time 0.2)

  (defvar my-symbol-overlay-marker (make-marker))

  (defun my-symbol-overlay-enter ()
    (interactive)
    (set-marker my-symbol-overlay-marker (point))
    (symbol-overlay-put))

  (defun my-symbol-overlay-exit ()
    (interactive)
    (symbol-overlay-put)    ;; exit
    (symbol-overlay-remove-all)
    (when my-symbol-overlay-marker
      (goto-char my-symbol-overlay-marker)
      (set-marker my-symbol-overlay-marker nil)))

  :bind (:map evil-normal-state-map
         ("M-s"    . symbol-overlay-mode)
         ("s"      . my-symbol-overlay-enter)
         :map symbol-overlay-map
         ("q"      . nop)
         ("h"      . nil)
         ("?"      . symbol-overlay-map-help)
         ("j"      . symbol-overlay-jump-next)
         ("k"      . symbol-overlay-jump-prev)
         ("c"      . symbol-overlay-save-symbol)
         ("C-g"    . my-symbol-overlay-exit)
         ([escape] . my-symbol-overlay-exit)
         ([(return)] . my-symbol-overlay-exit)
         ("s"      . my-symbol-overlay-exit))
  )

;; ----------------------------------------------------------------------
(use-package scratch-log
  :after recentf
  :if window-system
  :ensure t
  :config
  (add-to-list 'recentf-exclude "scratch-log-autoloads.el")

  (defun my-adv--sl-restore-scratch--no-modified (&rest _)
    (set-buffer-modified-p nil))
  (advice-add 'sl-restore-scratch :after #'my-adv--sl-restore-scratch--no-modified)
  )

;; ----------------------------------------------------------------------
(use-package quick-back
  :load-path "elisp"
  :bind (:map evil-normal-state-map
              ("q SPC" . quick-back-mark)
              ("q q"   . quick-back-jump))
  )

;; ----------------------------------------------------------------------
(use-package gist
  :if window-system
  :after evil
  :config
  (evil-add-hjkl-bindings gist-list-menu-mode-map 'emacs
    (kbd "x")       'gist-kill-current
    (kbd "d")       'gist-kill-current
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous)

  :bind (:map gist-mode-map
              ("q"       . my-kill-buffer)
              ("C-x C-s" . gist-mode-save-buffer)
              ("C-c C-c" . gist-mode-save-buffer))
  )

;; ----------------------------------------------------------------------
(use-package git-gutter
  :hook ((focus-in . git-gutter))
  :config
  ;; (setq git-gutter:modified-sign " "
  ;;       git-gutter:added-sign " "
  ;;       git-gutter:deleted-sign " ")
  (global-git-gutter-mode t)

  (use-package git-gutter-fringe
    :custom
    (left-fringe-width 16)
    (right-fringe-width 20)

    :config
    (fringe-helper-define 'git-gutter-fr:modified nil
      "................"    ;; 1
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"    ;; 18
      "................"    ;; 19
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................")   ;; 36
    (fringe-helper-define 'git-gutter-fr:added nil
      "................"    ;; 1
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"    ;; 18
      "................"    ;; 19
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................")   ;; 36
    (fringe-helper-define 'git-gutter-fr:deleted nil
      "................"    ;; 1
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"    ;; 18
      "................"    ;; 19
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"
      "................"))  ;; 36
  )

;; ----------------------------------------------------------------------
(use-package anzu
  :disabled t
  :config
  (defun my-query-replace (&optional arg)
    (interactive "P")
    (call-interactively (if arg
                            #'anzu-query-replace-regexp
                          #'anzu-query-replace)))
  :bind (("M-%" . my-query-replace))

  )

;; ----------------------------------------------------------------------
(use-package pulsar
  :ensure t
  :config
  (setq pulsar-face 'cursor)
  (pulsar-global-mode +1)
  )

;; ----------------------------------------------------------------------
(use-package dired
  :if window-system
  ;; :disabled
  :config
  (setq dired-dwim-target t                   ; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
        dired-recursive-copies 'always        ; ディレクトリを再帰的にコピーする
        dired-isearch-filenames t             ; diredバッファでC-sした時にファイル名だけにマッチするように
        ls-lisp-ignore-case t                 ; ファイル名の大文字小文字無視でソート
        ls-lisp-dirs-first t                  ; ディレクトリとファイルを分けて表示
        dired-listing-switches "-AlGh"        ; グループ表示なし、'.'/'..'を非表示、サイズをK/Mに変換
        )

  ;; ファイルなら別バッファで、ディレクトリなら同じバッファで開く
  ;; http://nishikawasasaki.hatenablog.com/entry/20120222/1329932699
  (defun dired-open-in-accordance-with-situation ()
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (if (file-directory-p file)
          (dired-find-alternate-file)
        (dired-find-file))))

  (put 'dired-find-alternate-file 'disabled nil) ;; dired-find-alternate-file の有効化

  (defun kill-current-buffer-and-dired-up-directory (&optional other-window)
    "In Dired, dired-up-directory and kill previously selected buffer."
    (interactive "P")
    (let ((b (current-buffer)))
      (dired-up-directory other-window)
      (kill-buffer b)))

  :bind (("C-x d"    . dired)
         ("C-x C-d"  . dired)
         :map dired-mode-map
         ("C-o"   . nil)                     ;; other-window instead of dired-display-file
         ("o"     . dired-display-file)
         ("q"     . kill-this-buffer)        ;; kill-buffer instead to bury-buffer
         ("H"     . dired-up-directory)
         ("L"     . nop)
         ("a"     . dired-find-file)
         ("RET"   . dired-open-in-accordance-with-situation)
         ([right] . dired-open-in-accordance-with-situation)
         ([left]  . kill-current-buffer-and-dired-up-directory)
         ("r"     . revert-buffer))                                    ; reload

  )

;; ----------------------------------------------------------------------
(use-package flycheck
  :disabled t
  :if window-system
  :init
  (defun flycheck-c-mode-hook-func ()
    (flycheck-add-mode 'javascript-eslint 'js-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    ;; (flycheck-select-checker 'my-c)
    ;; (flycheck-mode t)
    ;; (setq flycheck-check-syntax-automatically '(mode-enabled save)) ;; new-line also possible
    )

  (defun flycheck-python-mode-hook-func ()
    (flycheck-mode 1))

  (defun flycheck-ruby-mode-hook-func ()
    (setq flycheck-checker 'ruby-rubocop))

  :hook (
         ;; (c-mode    . flycheck-c-mode-hook-func)
         ;; (nim-mode  . flycheck-nim-mode-hook-func)
         ;; (js-mode   . flycheck-js-mode-hook-func)
         ;; (web-mode  . flycheck-js-mode-hook-func)
         (python-mode . flycheck-python-mode-hook-func)
         (ruby-mode . flycheck-ruby-mode-hook-func))
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)

  :config
  ;; (setq flycheck-display-errors-delay 0.3)
  (setq flycheck-idle-change-delay 0.3)
  ;; (setq flycheck-idle-buffer-switch-delay 0.0)

  (let ((color (face-foreground 'error)))
    (set-face-underline 'flycheck-error `(:style wave :color ,color)))

  (define-key evil-motion-state-map (kbd "M-j") 'flycheck-next-error)
  (define-key evil-motion-state-map (kbd "M-k") 'flycheck-previous-error)

  (setq flycheck-indication-mode 'right-fringe)

(define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow-right
  (vector #b00011011
          #b00110110
          #b01101100
          #b11011000
          #b01101100
          #b00110110
          #b00011011))

(flycheck-define-error-level 'error
  :severity 100
  :compilation-level 2
  :overlay-category 'flycheck-error-overlay
  :fringe-bitmap 'flycheck-fringe-bitmap-double-arrow-right
  :fringe-face 'flycheck-fringe-error
  :error-list-face 'flycheck-error-list-error)

  (flycheck-define-error-level 'warning
    :severity 10
    :compilation-level 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-double-arrow-right
    :fringe-face 'flycheck-fringe-warning
    :error-list-face 'flycheck-error-list-warning)

  (flycheck-define-error-level 'info
    :severity -10
    :compilation-level 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-double-arrow-right
    :fringe-face 'flycheck-fringe-info
    :error-list-face 'flycheck-error-list-info)

  ;; :config
  ;; (flycheck-define-checker my-c
  ;;   "My C checker using gcc"
  ;;   :command ("gcc" "-Wall" "-Wextra" source)
  ;;   :standard-input t
  ;;   :error-patterns  ((error line-start
  ;;                            (file-name) ":" line ":" column ":" " error: " (message)
  ;;                            line-end)
  ;;                     (warning line-start
  ;;                              (file-name) ":" line ":" column ":" " warning: " (message)
  ;;                              line-end))
  ;;   :modes (c-mode c++-mode))
  ;;

  ;; ;; The following might be needed to ensure flycheck is loaded.
  ;; ;; Hooking is required if flycheck is installed as an ELPA package (from any repo).
  ;; ;; If you use ELPA, you might want to merge this with any existing hook you might have.
  ;; (add-hook 'after-init-hook
  ;;           #'(lambda ()
  ;;               (after-packages-loaded-hook)))

  ;; (defun after-packages-loaded-hook ()
  ;;   (require 'flycheck))

  )

;; ----------------------------------------------------------------------
(use-package cc-mode
  :mode (("\\.c$" . c-mode)
         ("\\.h$" . c-mode)
         ;; ("\\.h$"       . c++-mode)
         ("\\.cpp$"     . c++-mode)
         ("\\.c\\+\\+$" . c++-mode)
         ("\\.hpp$"     . c++-mode))

  :config
  (advice-add 'c-update-modeline :around #'ignore)      ;; C++//l => C++

  (add-hook 'c-mode-common-hook
            #'(lambda ()
              (local-set-key "\C-m" 'reindent-then-newline-and-indent)
              (local-set-key "\C-i" 'indent-or-insert-tab)
              ;; (local-set-key "(" 'my-insert-paren)
              ;; (local-set-key "{" 'my-insert-brace)
              ;; (setq case-fold-search nil)                 ; t: case sensitive
              (c-set-style "stroustrup")
              (c-set-offset 'case-label '+)
              (c-set-offset 'statement-cont 'c-lineup-math)
              (setq comment-start "//")                   ; コメントを // にする
              (setq comment-end "")
              ;; (setq compilation-read-command nil)         ; make のオプションの確認は不要
              (setq compilation-ask-about-save nil)       ; make するとき save する
              ;; (setq compile-command "make")               ; make時のデフォルトコマンド
              (c-toggle-hungry-state 0)                   ; backspace時にカーソルの左の空白をすべて削除
              (cwarn-mode)
              ;; (which-function-mode 1)
              (display-line-numbers-mode)
              (setq compilation-scroll-output t)
              ;; (setq compile-command "cd ~/git-clone/qmk_firmware; make dichotemy:default")
              (setq compilation-auto-jump-to-first-error t)
              (setq compilation-window-height 10)

              (setq hide-ifdef-shadow t)
              (hide-ifdef-mode 1)

              ;; (setq flycheck--automatically-enabled-checkers '(c/c++-gcc))
              ;; (flycheck-disable-checker 'c/c++-clang)
              ;; (flycheck-mode 1)      ;; flycheck will be enabled in prog-mode
              ))


  ;; (defun my-flycheck-c-setup ()
  ;;   (setq flycheck-c/c++-gcc-executable "gcc")
  ;;   (setq flycheck-clang-language-standard "gnu99"))
  ;; (add-hook 'c-mode-hook #'my-flycheck-c-setup)

  ;; (defun my-flycheck-c++-setup ()
  ;;   (setq flycheck-c/c++-gcc-executable "g++")
  ;;   (setq flycheck-clang-language-standard "c++11"))
  ;; (add-hook 'c++-mode-hook #'my-flycheck-c++-setup)

  ;; ;; never show *compilation* buffer
  ;; (defadvice compilation-start (around inhidbit-display (command &optional mode name-function highlight-regexp))
  ;;   (flet ((display-buffer))
  ;;     (fset 'display-buffer 'ignore) ad-do-it))
  ;; (ad-activate 'compilation-start)

  ;; enable ANSI color in *compilation* buffer
  ;; (require 'ansi-color)
  (defun colorize-compilation-mode-hook ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook #'colorize-compilation-mode-hook)

  (defun truncate-compilation-mode-hook ()
    (setq truncate-lines t) ;; automatically becomes buffer local
    (set (make-local-variable 'truncate-partial-width-windows) nil))
  (add-hook 'compilation-mode-hook 'truncate-compilation-mode-hook)

  (set-face-attribute 'font-lock-comment-face nil :slant 'normal)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'normal)

  (defun flymake-cc-init ()
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name)))
           (ext (file-name-extension local-file))
           (compiler (executable-find (cond ((or (string= ext "c")
                                                 (string= ext "h"))
                                             "gcc")
                                            (t
                                             "g++")))))
      (unless (file-executable-p compiler)
        (error "Not found: %s" compiler))
      (list compiler (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))

  ;; (push '("\\.c$" flymake-cc-init) flymake-allowed-file-name-masks)
  ;; (push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
  ;; (push '("\\.c++$" flymake-cc-init) flymake-allowed-file-name-masks)

  ;; (add-hook 'c++-mode-hook #'(lambda () (flymake-mode t)))
  ;; (add-hook 'c-mode-hook   #'(lambda () (flymake-mode t)))

  ;; (defun my-cpp-eglot-enable ()
  ;;   "enable variables and hooks for eglot cpp IDE"
  ;;   (interactive)
  ;;   (setq clangd-exe (pcase system-type
  ;;                      ('darwin "/usr/local/opt/llvm/bin/clangd")
  ;;                      (_ nil)))
  ;;   (unless clangd-exe
  ;;     (error "Not found 'clangd', quit."))

  ;;   (defun my-projectile-proj-find-function (dir)
  ;;     (let ((root (projectile-project-root dir)))
  ;;       (and root (cons 'transient root))))

  ;;   (setq company-backends
  ;;         (cons 'company-capf
  ;;               (remove 'company-capf company-backends)))
  ;;   (with-eval-after-load 'projectile
  ;;     (add-to-list 'project-find-functions
  ;;                  'my-projectile-proj-find-function))
  ;;   (add-to-list 'eglot-server-programs
  ;;                `((c++-mode c-mode) . ,clangd-exe))
  ;;   (add-hook 'c++-mode-hook 'eglot-ensure)
  ;;   (add-hook 'c-mode-hook 'eglot-ensure)
  ;;   )

  ;; (defun my-cpp-eglot-disable ()
  ;;   "disable hook for eglot"
  ;;   (interactive)
  ;;   (remove-hook 'c++-mode-hook 'eglot-ensure)
  ;;   (remove-hook 'c-mode-hook 'eglot-ensure)
  ;;   )


  ;; :after telephone-line
  ;; :config
  ;; ;; workaround for *compilation* buffer
  ;; (dolist (f '(compilation-info compilation-warning compilation-error))
  ;;   (set-face-background f (face-attribute 'telephone-line-accent-inactive :background)))

  :bind (
         :map c-mode-map
         ("C-c C-c"   . quickrun))
  )

;; ----------------------------------------------------------------------
(use-package prog-mode
  :config
  (defun prog-mode-hooks-func ()
    (modify-syntax-entry ?_ "w")  ;; treat '_' as a part of word for evil-search-word-forward/backward
    (electric-pair-mode +1)
    ;; (c-toggle-auto-newline +1)
    ;; (add-hook 'before-save-hook #'delete-trailing-whitespace nil 'buffer-local)

    ;; (set-face-background 'trailing-whitespace (face-foreground 'error))      ;; should be defined by theme
    (setq show-trailing-whitespace t)

    ;; (flycheck-mode +1)
    )

  (add-hook 'prog-mode-hook #'prog-mode-hooks-func)
  )

;; ----------------------------------------------------------------------
(use-package nim-mode
  :mode (("\\.nim$" . nim-mode))
  :hook (nim-mode . lsp)
  :config
  (use-package flycheck-nim)
  :bind (;; global
         ;; (""   . #'func)
         ;; :map nim-mode-map
         ;; ("M-j" .  #'my-popper-echo)
         ;; ("M-k" .  #'my-popper-echo)
         )
  )

;;; ----------------------------------------------------------------------
(use-package arduino-mode
  ;; :disabled t
  :hook (arduino-mode . flymake-mode)
  :mode (("\\.pde$" . arduino-mode)
         ("\\.ino$" . arduino-mode))
  ;; :bind (("C-c C-c" . arduino-cli-compile)
         ;; ("C-c C-u" . arduino-cli-upload))
  :config
  ;; (defun flymake-arduino-init ()
  ;;   (unless arduino-exe-path
  ;;     (error "Not defined arduino-exe-path"))
  ;;   (unless (file-exists-p arduino-exe-path)
  ;;     (error "Not found %s" arduino-exe-path))
  ;;   (unless (file-executable-p arduino-exe-path)
  ;;     (error "Not found %s" arduino-exe-path))
  ;;   (let* ((temp-file   (flymake-proc-init-create-temp-buffer-copy
  ;;                        ;; 'flymake-create-temp-inplace))
  ;;                        'flymake-proc-create-temp-with-folder-structure))
  ;;          (local-dir   (file-name-directory buffer-file-name)))
  ;;     (list arduino-exe-path (list "compile"
  ;;                                  (concat "--fqbn=" arduino-fqbn)
  ;;                                  (substring local-dir 0 -1)))))
  ;;
  ;; (push '("\\.ino$" flymake-arduino-init) flymake-proc-allowed-file-name-masks)
  ;; (push '("^\\(.+\.ino\\):\\([0-9]+\\):\\([0-9]+\\): \\(.+\\)$" 1 2 3 4) flymake-err-line-patterns)

  (use-package arduino-cli-mode
    :ensure t

    :custom
    (arduino-cli-warnings 'all)
    (arduino-cli-verify t)

    :bind ( :map arduino-mode-map
            ("C-c C-c" . arduino-cli-compile)
            ("C-c C-u" . arduino-cli-upload))
    )
  )

;; ----------------------------------------------------------------------
(use-package rustic
  :disabled t
  ;; :defer t
  :ensure t
  :mode ("\\.rs$" . rustic-mode)
  ;; :hook ((rustic-mode . eglot-ensure))
  :config
  (setq rustic-lsp-client 'eglot)
  )

;; ----------------------------------------------------------------------
(use-package rust-mode
  ;; :disabled t
  :mode ("\\.rs$" . rust-mode)
  :hook (
         ;; (rust-mode . my/rust-mode-hook-func)
         (rust-mode . eglot-ensure)
         (rust-mode . eldoc-mode)
         (rust-mode . flymake-mode))

  :bind (
         :map rust-mode-map
         ("C-c C-c"   . my-rust-run)
  ;; :map compilation-mode-map
  ;; ("q"     . kill-this-buffer))        ;; kill-buffer instead to bury-buffer
  ;; ("q"     . kill-this-buffer))        ;; kill-buffer instead to bury-buffer
         )

  :custom (rust-format-on-save t)
  :config
  (defun my-rust-run ()
    (interactive)
    (rust-run)
    (pop-to-buffer "*compilation*")
    (setq truncate-lines nil)
    (scroll-up-line 2))

  ;; https://syohex.hatenablog.com/entry/2022/11/08/000610
  (defun my/find-rust-project-root (dir)
   (when-let ((root (locate-dominating-file dir "Cargo.toml")))
     (list 'vc 'Git root)))

  (defun my/rust-mode-hook-func ()
    (setq-local project-find-functions (list #'my/find-rust-project-root)))
)

;; ----------------------------------------------------------------------
(use-package jal-mode
  :disabled     ;; temporary
  :load-path "~/git-clone/jal-mode"
  ;; :load-path "~/tmp/jal-mode"

  :config
  (setq tab-width 4)
  (setq indent-tabs-mode t)

  (setq jal-mode-compiler-path "/Users/g/test/jal/jallib-pack-bee-jalv25r6-20220522/compiler/jalv2-osx")
  (setq jal-mode-lib-path "/Users/g/test/jal/jallib-pack-bee-jalv25r6-20220522/lib")

  ;; enable flymake
  ;; (setq temporary-file-directory "~/tmp")     ;; as you like
  ;; (add-hook 'jal-mode-hook #'flymake-mode)
  ;; :init
  ;; (setq flymake-log-level 3)


  ;; enable flycheck
  (use-package flycheck-jal
    :config
    (add-hook 'jal-mode-hook #'flycheck-mode))

  ;; use rp2pic programmer
  (setq jal-mode-prog-func #'jal-mode-prog-func--rp2pic)
  )

;; ----------------------------------------------------------------------
(use-package mql-mode
  :mode (("\\.mq4$" . mql-mode)
         ("\\.mqh$" . mql-mode))
  :bind (:map mql-mode-map
         ([S-down] . flymake-goto-next-error)
         ([S-up]   . flymake-goto-prev-error))
  :config
  (setq mq4-compiler "C:/Program Files (x86)/XMTrading MT4/metaeditor.exe")
  (add-hook 'mql-mode-hook #'(lambda ()
                             (flycheck-mode -1)
                             (flymake-mode t)
                             ;; (counsel-gtags-mode -1)
                             (symbol-overlay-mode t)
                             ;; (which-function-mode 1)
                             ))
  )

;; ----------------------------------------------------------------------
;; disable for byte-compile due to: Error (use-package): slime/:config: Not found roswell: /usr/local/bin/ros Disable showing Disable logging
;; (use-package slime
;;   :disabled
;;   :if window-system
;;   :init
;;   (load (expand-file-name "~/.roswell/helper.el"))

;;   :config
;;   (setq slime-startup-animation nil)
;;   (defalias 'slime-reset 'slime-restart-inferior-lisp)
;;   (setq inferior-lisp-program "ros -Q run")
;;   (setq slime-net-coding-system 'utf-8-unix)
;;   (add-hook 'slime-load-hook #'(lambda () (require 'slime-fancy)))
;;   (slime-setup '(slime-fancy slime-banner))

;;   ;; 分割したウィンドウでslime起動
;;   (defun my-slime (&optional command coding-system)
;;     "Run slime and split window."
;;     (interactive)
;;     (if (< (count-windows) 2)
;;         (split-window-vertically))
;;     (slime command coding-system))

;;   ;; 選択範囲をslime-replへ送って評価
;;   (defun slime-repl-send-region (start end)
;;     "Send region to slime-repl."
;;     (interactive "r")
;;     (let ((buf-name (buffer-name (current-buffer)))
;;           (sbcl-buf (get-buffer "*slime-repl sbcl*")))
;;       (cond (sbcl-buf
;;              (copy-region-as-kill start end)
;;              (switch-to-buffer-other-window sbcl-buf)
;;              (yank)
;;              (slime-repl-send-input "\n")
;;              (switch-to-buffer-other-window buf-name))
;;             (t (message "Not exist *slime-repl sbcl* buffer!")))))

;;   ;; LISPモードで新しくファイルを開いたらウィンドウが上下に分割して下にREPL
;;   (add-hook 'lisp-mode-hook
;;             #'(lambda ()
;;               (global-set-key "\C-cC-h" 'hyperspec-lookup)
;;               (cond ((not (featurep 'slime))
;;                      (require 'slime)
;;                      (normal-mode)))
;;               (my-slime)
;;               (other-window)))

;;   :bind (:map lisp-mode-map
;;              ("M-r" . nil)
;;              ("C-x C-e" . slime-eval-last-expression-in-repl)
;;              ("C-c C-c" . slime-compile-and-load-file)
;;              ("C-c C-r" . slime-repl-send-region)
;;              ("C-c C-f" . slime-compile-defun))
;;   )


;; ----------------------------------------------------------------------
(use-package shell-script-mode
  :mode (("zshrc" . shell-script-mode))
  )

;; ----------------------------------------------------------------------
(use-package web-mode
  :disabled t
  :after flycheck
  :mode (("\\.html$" . web-mode))
  ;; :requires html-mode
  ;; :hook ((web-mode . lsp))
  ;; :commands lsp

  ;; :hook (web-mode . flycheck-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-current-element-highlight t)
  ;; (web-mode-engines-alist '(("django" . "\\.html$")))     ;; django template (this is temporary)

  :config
  (setq-default web-mode-comment-formats
              '(("java"       . "/*")
                ("javascript" . "//")
                ("css"        . "//")
                ("php"        . "/*")))

  (setq web-mode-extra-snippets
   '(("django" . (("{" . "{{ | }}")
                  ("c" . "{# -- | -- #}")
                  ("%" . "{% | %}\n\n{% end %}\n")
                  ("ext" . "{% extends '|' %}\n")
                  ("if" . "{% if | %}\n\n{% else %}\n\n{% endif %}\n")
                  ("for" . "{% for | in  %}\n\n{% endfor %}\n")
                  ("block" . "{% block | %}\n\n{% endblock %}\n")

                  ("<" . "<|>")
                  ))))

  (evil-define-key 'normal web-mode-map (kbd "M-[") 'web-mode-snippet-insert)
  (evil-define-key 'insert web-mode-map (kbd "M-[") 'web-mode-snippet-insert)

  (defun cesco/django ()
    (if (projectile-project-p)
        (if (file-exists-p (concat (projectile-project-root) "manage.py"))
            (web-mode-set-engine "django")
            (message "do not exists") ;; You can safely delet this line
            )
        )
    )

  (add-hook 'web-mode-hook 'cesco/django)

  ;; :config
  ;; ;; flymake setting
  ;; :if (executable-find "tidy")      ;; html
  ;; :config
  ;; (delete '("\\.html?\\'" flymake-proc-xml-init) flymake-proc-allowed-file-name-masks)
  ;; (defun flymake-proc-html-init ()
  ;;   (let* ((temp-file (flymake-proc-init-create-temp-buffer-copy
  ;;                      'flymake-proc-create-temp-inplace))
  ;;          (local-file (file-relative-name
  ;;                       temp-file
  ;;                       (file-name-directory buffer-file-name))))
  ;;     ;; (list "tidy" (list local-file))))
  ;;     (list "tidy" (list "-utf8" "-eq" local-file))))

  ;; (add-to-list 'flymake-proc-allowed-file-name-masks
  ;;              '("\\.html$\\|\\.ctp" flymake-proc-html-init))

  ;; (setq flymake-proc-err-line-patterns '())
  ;; (add-to-list 'flymake-proc-err-line-patterns
  ;;              '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
  ;;                nil 1 2 4))

  ;; :config
  ;; (flycheck-add-mode 'html-tidy 'web-mode)

  ;; re-defined
  (flycheck-define-checker html-tidy
    "A HTML syntax and style checker using Tidy.

See URL `https://github.com/htacg/tidy-html5'."
    :command ("tidy" (config-file "-config" flycheck-tidyrc)
           ;; "-lang" "en"
              "-utf8"
              "-e" "-q")
    :standard-input t
    :error-patterns
    ((error line-start
            "line " line
            " column " column
            " - Error: " (message) line-end)
     (warning line-start
              "line " line
              " column " column
              " - Warning: " (message) line-end))
 ;; :modes (html-mode mhtml-mode nxhtml-mode))
    :modes (web-mode))

  :bind (:map web-mode-map
              ("M-;" . comment-line))
  ;;        ([S-down] . flycheck-next-error)
  ;;        ([S-up]   . flycheck-previous-error))


  ;; ;; my-web-mode-replace-tag
  ;; (defface my-web-mode-replace-tag-highlight-face
  ;;   `((t :foreground "black" :background "yellow"))
  ;;   "Face to highlight tag name temporary in my-web-mode-replace-tag")
  ;;
  ;; (defun my-web-mode-replace-tag (&optional marker len from to)
  ;;   "Replace html-tag at out side near the point in web-mode."
  ;;   (interactive)
  ;;   (flet ((delete-word (n) (delete-region (point) (progn (forward-word n) (point))))
  ;;          (delete-backward-word () (delete-word -1))
  ;;          (add-overlay (beg end)
  ;;                       (prog1
  ;;                           (overlay-put (make-overlay beg end) 'face 'my-web-mode-replace-tag-highlight-face)
  ;;                         (setq mark-active nil))))
  ;;     (save-excursion
  ;;       (cond ((and marker len from to)  ;; third call
  ;;              (delete-backward-word)
  ;;              (insert to))
  ;;             ((and marker len)            ;; second call
  ;;              (when (text-property-search-forward 'face 'web-mode-html-tag-face t)
  ;;                ;; highlighting
  ;;                (let* ((ol1-end (point))
  ;;                       (ol1-beg (- (point) len))
  ;;                       (ol2-end marker)
  ;;                       (ol2-beg (- marker len)))
  ;;                  (add-overlay ol1-beg ol1-end)
  ;;                  (add-overlay ol2-beg ol2-end)
  ;;                  (ignore-errors
  ;;                    (unwind-protect
  ;;                        (let* ((from (word-at-point t))
  ;;                               (to (read-string (format "Rename tag \"%s\":" from))))
  ;;                          (remove-overlays ol1-beg ol1-end 'face 'my-web-mode-replace-tag-highlight-face)
  ;;                          (remove-overlays ol2-beg ol2-end 'face 'my-web-mode-replace-tag-highlight-face)
  ;;                          (delete-backward-word)
  ;;                          (insert to)
  ;;                          (goto-char marker)
  ;;                          (my-web-mode-replace-tag marker len from to))
  ;;                        (remove-overlays ol1-beg ol1-end 'face 'my-web-mode-replace-tag-highlight-face)
  ;;                        (remove-overlays ol2-beg ol2-end 'face 'my-web-mode-replace-tag-highlight-face)))
  ;;                  )))
  ;;             (t                  ;; first call
  ;;              ;; (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;;              ;; (undo-boundary)
  ;;              (when (text-property-search-forward 'face 'web-mode-html-tag-face t)
  ;;                (let* ((marker (point-marker))
  ;;                       (len (- marker (progn (forward-word -1) (point)))))
  ;;                  (web-mode-tag-match)
  ;;                  (my-web-mode-replace-tag marker len))))))))
  ;;
  ;; (set-face-background 'my-web-mode-replace-tag-highlight-face (face-foreground 'warning))
  ;; (evil-define-key 'normal web-mode-map (kbd "t") 'my-web-mode-replace-tag)
  )

;; ----------------------------------------------------------------------
(use-package web-beautify

  )

;; ----------------------------------------------------------------------
(use-package js
  :config
  (setq js-indent-level 2)

  )


;; ----------------------------------------------------------------------
(use-package ruby-mode
  :mode "\\.rb\\'"
  )
;; ----------------------------------------------------------------------
(use-package python-mode
  :mode "\\.py\\'"
  :config
  (let ((count 0))
    (defun my-insert-quote-python ()
      (interactive)
      (setq count
            (if (eq last-command this-command)
                (mod (1+ count) 4)
              0))
      (my-insert-quote-python-1 count ?')))

  (let ((count 0))
    (defun my-insert-double-quote-python ()
      (interactive)
      (setq count
            (if (eq last-command this-command)
                (mod (1+ count) 4)
              0))
      (my-insert-quote-python-1 count ?\")))


  (defun my-insert-quote-python-1 (count q)
    (pcase count
      (0 (insert q))
      (1 (insert q) (backward-char))
      (2 (insert q q q q) (backward-char 2))
      (3 (backward-char 3) (delete-char 6) (backward-delete-char)
         (setq count 0)
         (my-insert-quote-python-1 count q))))

  ;; (evil-define-key 'insert python-mode-map (kbd "'") #'my-insert-quote-python)
  ;; (evil-define-key 'insert python-mode-map (kbd "\"") #'my-insert-double-quote-python)
  )

;; ----------------------------------------------------------------------
(use-package auto-rename-tag
  :hook (web-mode . auto-rename-tag-mode))

;; ----------------------------------------------------------------------
(use-package posframe
  :if window-system
  :config
  (setq posframe-mouse-banish nil)

  (defun my-adv--frame-redraw (&rest _)
    ;; (message "!")
    ;; (redraw-display))                       ;; work but frickering
    ;; (redraw-frame (selected-frame)))        ;; not work
    (redraw-modeline 'all))                 ;; working good!
    ;; (force-mode-line-update t))             ;; work but frickering
  (advice-add 'posframe-show :after #'my-adv--frame-redraw)
  ;; (advice-remove 'posframe-show  #'my-adv--frame-redraw) ;; for testing
  )

;; ----------------------------------------------------------------------
(use-package flycheck-posframe
  :if window-system
  :ensure t
  :after flycheck
  :config
  (setq flycheck-posframe-border-width 4)
  (set-face-attribute 'flycheck-posframe-error-face nil)
  (set-face-attribute 'flycheck-posframe-warning-face nil)
  (set-face-attribute 'flycheck-posframe-info-face nil)
  (setq flycheck-posframe-error-prefix "")
  (setq flycheck-posframe-warning-prefix "")
  (setq flycheck-posframe-info-prefix "")
  (set-face-background 'flycheck-posframe-background-face "dim gray")
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
  )

;; ----------------------------------------------------------------------
(use-package flymake
  ;; :disabled t
  :config

  ;; fringe indicator for HiDPI
  ;; https://www.reddit.com/r/emacs/comments/1bawdau/making_flymake_supports_error_indicators_in_margin/
  ;;
  ;;;; with nerd font
  ;;
  ;; (advice-add #'flymake--fringe-overlay-spec :override
  ;;             (lambda (bitmap &optional recursed)
  ;;               (if (and (symbolp bitmap)
  ;;                        (boundp bitmap)
  ;;                        (not recursed))
  ;;                   (flymake--fringe-overlay-spec
  ;;                    (symbol-value bitmap) t)
  ;;                 (and flymake-fringe-indicator-position
  ;;                      bitmap
  ;;                      (propertize "!" 'display
  ;;                                  `((margin left-margin)
  ;;                                    ,bitmap))))))
  ;;
  ;; (put 'flymake-error 'flymake-bitmap (propertize "⚠️" 'face `(:inherit (error default) :underline nil)))
  ;; (put 'flymake-warning 'flymake-bitmap (propertize "⛔" 'face `(:inherit (warning default) :underline nil)))
  ;; (put 'flymake-note 'flymake-bitmap (propertize "🟢" 'face `(:inherit (success default) :underline nil)))
  ;;
  ;;;; without nerd font
  (advice-add
   #'flymake--fringe-overlay-spec :override
   (lambda (bitmap &optional recursed)
     (if (and (symbolp bitmap)
              (boundp bitmap)
              (not recursed))
         (flymake--fringe-overlay-spec
          (symbol-value bitmap) t)
       (and flymake-fringe-indicator-position
            bitmap
            (concat
             (propertize "!" 'display
                         `((margin left-margin)
                           ,bitmap))
             (propertize " "
                         'face
                         '(:inherit default
                                    :underline nil
                                    :stipple nil)
                         'display
                         `((margin left-margin)
                           (space :width 5))))))))
  )

;; ----------------------------------------------------------------------
(use-package flymake-posframe
  ;; :disabled t
  :if window-system
  :load-path "elisp/flymake-posframe"
  :hook (flymake-mode . flymake-posframe-mode)
  :config
  (set-face-attribute 'flymake-error nil :underline `(:color ,(mycolor 'red) :style wave))
  (set-face-attribute 'flymake-warning nil :underline '(:color "orange" :style wave))

  (setq flymake-posframe-error-prefix (propertize "" 'face `(foreground-color . ,(mycolor 'red))))
  (setq flymake-posframe-warning-prefix (propertize "" 'face '(foreground-color . "orange")))
  (setq flymake-posframe-note-prefix (propertize "" 'face `(foreground-color . "yellow green")))

  (defun my-flymake-posframe-display-adv-delay (orig-fun)
    (unless (company--active-p)
      (when (sit-for 0)
        (funcall orig-fun))))

  (advice-add 'flymake-posframe-display :around #'my-flymake-posframe-display-adv-delay)
  (add-hook 'find-file-hook #'flymake-posframe-hide)
  ;; (advice-add 'switch-to-buffer :after #'flymake-posframe-hide)
  )

;; ----------------------------------------------------------------------
(use-package super-save
  :ensure t
  :config
  (add-to-list 'super-save-triggers 'tabbar-forward-tab)
  (add-to-list 'super-save-triggers 'tabbar-backward-tab)

  (defun my-adv-super-save-command--disable-message (orig-fun)
    (let ((inhibit-message t))
      (funcall orig-fun)))
  (advice-add 'super-save-command :around #'my-adv-super-save-command--disable-message)

  ;; (setq super-save-auto-save-when-idle t
  ;;       super-save-idle-duration 10)
  (super-save-mode +1)
  )

;; ----------------------------------------------------------------------
(use-package migemo
  :disabled
  :config
  ;; fixme not work in _mac.el
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init)

  (setq evil-regexp-search nil)
  (defun my-adv-evil-search-function--migemo (&rest _)
    "Enable migemo when evil-search by / or ?.
Thx to https://qiita.com/duloxetine/items/0adf103804b29090738a"
    (cl-flet ((search-forward 'migemo-forward)
              (search-backward 'migemo-backward))))

  (advice-add 'evil-search-function :before #'my-adv-evil-search-function--migemo)
  )

;; ----------------------------------------------------------------------
(use-package default-text-scale
  :if window-system
  :config
  (setq default-text-scale-amount 30)
  (default-text-scale-mode 1)

  (defun my-adv-default-text-scale--reset-frame (&rest _)
    (modify-frame-parameters nil initial-frame-alist))

  (advice-add 'default-text-scale-reset :after #'my-adv-default-text-scale--reset-frame)
  )

;; ----------------------------------------------------------------------
(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook
            #'(lambda ()
              (setq indent-tabs-mode nil)
              (setq c-basic-offset 4)
              (c-set-offset 'substatement-open 0)
              ;; (flycheck-mode 1)
              (omnisharp-mode)))
  )

;;; ----------------------------------------------------------------------
(use-package slime
  :disabled t
  :if window-system
  :config
  (setq slime-startup-animation nil)
  (defalias 'slime-reset 'slime-restart-inferior-lisp)

  (defvar ros-exe "path/to/ros.exe")
  (defvar my-slime-helper "path/to/helper.el")
  (setq my-slime-helper (expand-file-name my-slime-helper))

  (unless (file-executable-p ros-exe)
      (error "Not found roswell: %s" ros-exe))
  (unless (file-exists-p my-slime-helper)
      (error "Not found slime helper: %s" helper))
  (load my-slime-helper)
  (setq inferior-lisp-program (format "%s -Q run" ros-exe))
  (add-hook 'slime-load-hook #'(lambda () (require 'slime-fancy)))
  (slime-setup '(slime-repl slime-fancy slime-banner))

  (defun slime-smart-quit ()
    (interactive)
    (without-yes-or-no
      (when (slime-connected-p)
        (if (equal (slime-machine-instance) "my.workstation")
            (slime-quit-lisp)
          (slime-disconnect)))
      (slime-kill-all-buffers)))

  (add-hook 'kill-emacs-hook 'slime-smart-quit)

    ;; 分割したウィンドウでslime起動
  (defun my-slime (&optional command coding-system)
    "Run slime and split window."
    (interactive)
    (when (< (count-windows) 2)
      (split-window-vertically))
    (slime command coding-system)
    (other-window))

  ;; 選択範囲をslime-replへ送って評価
  (defun slime-repl-send-region (start end)
    "Send region to slime-repl."
    (interactive "r")
    (let ((buf-name (buffer-name (current-buffer)))
          (sbcl-buf (get-buffer "*slime-repl sbcl*")))
      (cond (sbcl-buf
             (copy-region-as-kill start end)
             (switch-to-buffer-other-window sbcl-buf)
             (yank)
             (slime-repl-send-input "\n")
             (switch-to-buffer-other-window buf-name))
            (t (message "Not exist *slime-repl sbcl* buffer!")))))

  ;; LISPモードで新しくファイルを開いたらウィンドウが上下に分割して下にREPL
  (add-hook 'lisp-mode-hook
            #'(lambda ()
              (global-set-key "\C-cC-h" 'hyperspec-lookup)
              (cond ((not (featurep 'slime))
                     (require 'slime)
                     (normal-mode)))
              (my-slime)))

  ;; (evil-define-key 'normal sldb-mode-map (kbd "M-j") 'centaur-tabs-backward)
  ;; (evil-define-key 'normal sldb-mode-map (kbd "M-k") 'centaur-tabs-forward)

  :bind (:map lisp-mode-map
             ("M-r" . nil)
             ("C-x C-e" . slime-eval-last-expression-in-repl)
             ("C-c C-c" . slime-compile-and-load-file)
             ("C-c C-r" . slime-repl-send-region)
             ("C-c C-f" . slime-compile-defun))
  )

;; ----------------------------------------------------------------------
(use-package corfu
  :ensure t
  :hook ((prog-mode . corfu-mode))
  :bind (:map corfu-map
         ("TAB"        . corfu-next)
         ("<tab>"      . corfu-next)
         ("C-j"        . corfu-next)
         ("C-k"        . corfu-previous)
         ("RET"        . corfu-insert)
         ("<return>"   . corfu-insert)
         ("C-c"   . my-corfu-quit)
         ("C-g"   . my-corfu-quit))

  :custom ((corfu-auto t)
           (corfu-auto-delay 0)
           (corfu-auto-prefix 1)
           (corfu-cycle t)
           ;; (corfu-on-exact-match nil)
           (corfu-bar-width 1)
           (corfu-right-margin-width 2.5))

  :config
  (corfu-popupinfo-mode)

  (defun my-corfu-quit ()
    (interactive)
    ;; (message "!!!")
    (corfu--popup-hide)
    (corfu-quit)
    ;; (with-no-message (undo))
    ;; (evil-normal-state)
    ;; (evil-forward-char)
    (redraw-display)            ;; for evil state indicator
    )
  )

;; ----------------------------------------------------------------------
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)

  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)    ;; forcely clear cache for lsp-server
  )

;; ----------------------------------------------------------------------
(use-package yasnippet
  ;; :disabled
  :ensure t
  :hook
  (snippet-mode . my-yas-keybindings)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

  (defalias 'yas #'yas-new-snippet)
  (defalias 'yas-edit #'yas-visit-snippet-file)

  (defun my-adv--yas-load-snippet-buffer--kill-buffer (&rest _)
    (kill-buffer (current-buffer)))
  (advice-add 'yas-load-snippet-buffer :after #'my-adv--yas-load-snippet-buffer--kill-buffer)

  (defun my-yas-keybindings ()
    (define-key snippet-mode-map (kbd "C-x C-s") #'yas-load-snippet-buffer))

  (setq yas-new-snippet-default  "\
# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# --
$0`(yas-escape-text yas-selected-text)`

# e.g.: console.log('\\${1:debug} \\${2:value}: ' + \\$2);\\$0")

  (yas-global-mode 1)
  )

;; ----------------------------------------------------------------------
(use-package treesit-auto
  :disabled t
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode)
)

;; ----------------------------------------------------------------------
(use-package eglot
;;  :disabled t
  :ensure t
  :hook ((c-mode-common . eglot-ensure)    ;; C/C++
         ;; (dts-mode      . eglot-ensure)    ;; device tree source
         (rust-mode     . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '((rust-mode) "rust-analyzer"))

  ;; (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))     ;; for ginko_ls if it needs
  ;; (add-to-list 'eglot-server-programs '((dts-mode) "ginko_ls"))
  ;; (add-to-list 'eglot-server-programs '((dts-mode) "~/git-clone/ginko/target/debug/ginko_ls"))
  )
;; ----------------------------------------------------------------------
(use-package lsp-mode
  :disabled
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  ;; (prog-major-mode . lsp-prog-major-mode-enable)
  ;; (c++-mode . lsp)
  ;; (c-mode   . lsp)
  ;; (nim-mode . lsp)
  (js-mode . lsp-deferred)

  :custom
  ;; debug
  (lsp-print-io nil)                     ;; => *lsp-log*
  (lsp-trace nil)
  (lsp-print-performance nil)
  ;; general
  (lsp-auto-guess-root t)
  ;; (lsp-document-sync-method 'incremental) ;; always send incremental document
  (lsp-response-timeout 5)
  ;; (lsp-enable-completion-at-point nil)
  (lsp-enable-snippet t)
  (lsp-enable-indentation nil)     ;; disable when using ccls
  ;; (lsp-prefer-flymake t)
  (lsp-prefer-capf t)              ;; use capf instead of company
  ;; (lsp-document-sync-method 2)
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning nil)
  (lsp-idle-delay 0.1)
  (create-lockfiles nil)

  :config
  ;; nim-mode into lsp
  (add-to-list 'lsp-language-id-configuration '(nim-mode . "nim"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "nimlsp")
                    :major-modes '(nim-mode)
                    :server-id 'nimlsp))

  ;; :init
  ;; (unbind-key "C-l")
  :bind
  (
   ;; ("C-l C-l"  . lsp)
   ;; ("C-l h"    . lsp-describe-session)
   ;; ("C-l <f5>" . lsp-restart-workspace)

   ;; :map evil-motion-state-map
   ;; ("g h" . xref-quit-and-pop-marker-stack)
   ;; ("g t" . lsp-goto-type-definition)
   ;; ("g d" . xref-find-definitions)
   ;; ("g r" . xref-find-references)
   ;; ("g R" . lsp-rename)
   ;; ("g l" . lsp-lens-mode)

   :map xref--xref-buffer-mode-map
   ("C-o" . other-window)
   ("C-0" . quit-window)
   ("q q" . quit-window)
   ("RET" . xref-goto-xref)
   )
  )

;; ----------------------------------------------------------------------
(use-package lsp-ui
  :disabled t
  :commands lsp-ui-mode
  :after lsp-mode
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable t)
  ;; (lsp-ui-doc-header t)           ;; tabbar.el が使うし、'>'が文字化けするので無効化
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-max-width  60)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)

  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable nil)

  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)

  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable nil)
  (lsp-ui-imenu-kind-position 'top)

  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-peek-peek-height 30)
  (lsp-ui-peek-list-width 30)
  (lsp-ui-peek-fontify 'always)

  :hook
  (lsp-mode . lsp-ui-mode)

  :config
  (defun my-lsp-ui-peek-select-next ()
    (interactive)
    (if ()))

  :bind
  (
   ;; ("C-l C-l"  . lsp)
   ;; ("C-l h"    . lsp-describe-session)
   ;; ("C-l <f5>" . lsp-restart-workspace)
   :map evil-motion-state-map
   ("g s" . xref-quit-and-pop-marker-stack)
   ("g t" . lsp-goto-type-definition)
   ("g R" . lsp-rename)
   ("g l" . lsp-lens-mode)

   :map lsp-ui-mode-map
   ("g d" . lsp-ui-peek-find-definitions)
   ("g r" . lsp-ui-peek-find-references)

   :map lsp-ui-peek-mode-map
   ("j" . lsp-ui-peek--select-next)
   ("k" . lsp-ui-peek--select-prev)

   :map xref--xref-buffer-mode-map
   ("C-o" . other-window)
   ("C-0" . quit-window)
   ("q q" . quit-window)
   ("RET" . xref-goto-xref)
   )

  (("C-l s"   . lsp-ui-sideline-mode)
   ("C-l C-d" . lsp-ui-peek-find-definitions)
   ("C-l C-r" . lsp-ui-peek-find-references))
  )

;; ----------------------------------------------------------------------
(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup)
  )

;; ----------------------------------------------------------------------
(use-package ccls
  :ensure t
  :disabled
  :custom
  ;; (ccls-executable "/usr/local/bin/ccls")        ;; defined in _mac.el, _windows.el
  (ccls-sem-highlight-method 'font-lock)
  (ccls-use-default-rainbow-sem-highlight)
  ;; :hook ((c-mode c++-mode mql-mode) .
  :hook ((c-mode c++-mode) .
         (lambda () (require 'ccls) (lsp))))
;; ----------------------------------------------------------------------
(use-package dumb-jump
  :disabled t
  :ensure t
  :config
  ;; (setq dumb-jump-default-project "")
  ;; (setq dumb-jump-quiet t)
  (setq dumb-jump-force-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; (define-key evil-motion-state-map (kbd "g d") #'dumb-jump-go)      ; obsolete
  ;; (define-key evil-motion-state-map (kbd "g h") #'dumb-jump-back)    ; obsolete
  (dumb-jump-mode)
  )
;; ----------------------------------------------------------------------
(use-package jumplist
  :ensure t
  :config
  ;; (define-key evil-motion-state-map (kbd "g p") 'jumplist-previous)
  ;; (define-key evil-motion-state-map (kbd "g n") 'jumplist-next)
  (global-set-key (kbd "C->") 'jumplist-next)

  (setq jumplist-hook-commands '(
          avy-goto-char-timer
          evilmi-jump-items
          isearch-forward isearch-backward
          isearch-forward-regexp isearch-backward-regexp
          evil-search-word-backward evil-search-word-forward
          evil-search-next evil-search-previous
          my-beginning-of-defun
          my-beginning-of-line my-end-of-line
          symbol-overlay-jump-next
          symbol-overlay-jump-prev
          ))
  )
;; ----------------------------------------------------------------------
(use-package bm
  :config
  (set-face-background 'bm-face (face-foreground 'warning))

  (define-key evil-motion-state-map (kbd "g m") #'bm-toggle)
  (define-key evil-motion-state-map (kbd "g SPC") #'bm-next)
  )

;; ----------------------------------------------------------------------
(use-package intel-hex-mode
  :config
  )


;; ----------------------------------------------------------------------
(use-package add-node-modules-path
  :hook (js-mode js2-mode)
  )

;; ----------------------------------------------------------------------
(use-package nodejs-repl
  :config
  (add-hook 'js-mode-hook
          #'(lambda ()
            (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
            (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
            (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
            (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
            (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))

  )

;; ----------------------------------------------------------------------
(use-package minibuffer-timer)

;; ----------------------------------------------------------------------
(use-package vterm
  ;; :disabled t                ;; Vterm はWindowsでは非推奨っぽいので使わない
  ;; :if (string= window-system "ns")
  ;; :if t

  :ensure t
  :custom
  (vterm-buffer-name-string "> %s")
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 100000)

  :config
  ;; send keys to terminal directly for `backward-kill-word` in vterm
  (define-key vterm-mode-map (kbd "M-h")
    #'(lambda () (interactive) (vterm-send-key (kbd "C-w"))))

  ;; unbinding keys
  (dolist (k '("M-r" "M-o" "M-u" "M-i"
               "C-o" "C-0" "C-1" "C-2"))
    (define-key vterm-mode-map (kbd k) nil))

  ;; (define-key vterm-mode-map (kbd "C-M-f") #'toggle-frame-fullscreen)
  (define-key vterm-mode-map (kbd "C-p") #'(lambda () (interactive) (vterm-send-up)))
  (define-key vterm-mode-map (kbd "C-n") #'(lambda () (interactive) (vterm-send-down)))
  (define-key vterm-mode-map (kbd "C-g") #'(lambda () (interactive) (vterm-send-C-c)))
  (define-key vterm-mode-map (kbd "C-e") #'(lambda ()
                                             ;; zsh-autosuggestions
                                             (interactive)
                                             (let ((n (- (save-excursion (vterm-end-of-line) (point))
                                                         (point))))
                                               (while (> n 0)
                                                 (vterm-send-right)
                                                 (setq n (1- n))))))
  ;; force emacs-state
  (defun my-adv-switch-to-bufffer--force-evil-emacs-state-in-vterm (&rest _)
    (when (eq major-mode 'vterm-mode)
      (evil-emacs-state nil)))
  (advice-add 'switch-to-buffer :after #'my-adv-switch-to-bufffer--force-evil-emacs-state-in-vterm)

  (add-to-list 'evil-emacs-state-modes 'vterm-mode)

  ;; vterm in popper
  ;; thx to https://www.reddit.com/r/emacs/comments/u5rx6z/comment/i54fdpt/?utm_source=reddit&utm_medium=web2x&context=3
  (defun vt-in-popper ()
    "Open vterm in popper."
    (interactive)
    (vterm--internal popper-display-function))

  (defun my-vterm-return-to-emacs-state ()
    (interactive)
    (when (or (evil-normal-state-p)
              (evil-visual-state-p))
      (evil-normal-state +1)
      (evil-emacs-state +1)
      (goto-char (1- (point-max)))
      (call-interactively 'keyboard-quit)))

  (evil-define-key 'motion vterm-mode-map (kbd "C-g") #'my-vterm-return-to-emacs-state)

  (define-key vterm-mode-map (kbd "C-S-j") #'(lambda () (interactive) (funcall #'scroll-up 5)))
  (define-key vterm-mode-map (kbd "C-S-k") #'(lambda () (interactive) (funcall #'scroll-down 5)))

  (defvar open-vterm-buffer-name-prefix (substring vterm-buffer-name-string 0 2))
  (defun open-vterm ()
    (interactive)
    (let ((vterm-buf (cl-remove-if-not #'(lambda (buf)
                                           (or (string= (substring (buffer-name buf) 0 2) open-vterm-buffer-name-prefix)
                                               (string= (substring (buffer-name buf)) "*vterm*")))
                                       (buffer-list))))
      (if vterm-buf
          (switch-to-buffer (car vterm-buf))
        (vterm))))

  (define-key evil-normal-state-map (kbd "M-t") 'open-vterm)
  )

;; ----------------------------------------------------------------------
(use-package re-builder
  :ensure t
  :config
  (define-key reb-mode-map (kbd "C-x k") 'reb-quit)
  )

;; ----------------------------------------------------------------------
(use-package ediff
  :config
  (setq ediff-split-window-function 'split-window-horizontally)     ;; side by side

  (set-face-attribute 'ediff-fine-diff-A nil
                      :foreground "white" :background "dark green")
  )

;; ----------------------------------------------------------------------
(use-package eshell
  :config
  (defun eshell-mode-hook-func ()
    (define-key eshell-hist-mode-map (kbd "M-r") nil)
    )

  :hook (eshell-mode . eshell-mode-hook-func)
  )
;; ----------------------------------------------------------------------
;; yet another point-undo
(use-package goto-chg
  :bind (:map evil-normal-state-map
         ("g ," . goto-last-change)
         ("g ." . goto-last-change-reverse))
  )

;; ----------------------------------------------------------------------
(use-package disable-mouse
  :disabled
  :config
  (global-disable-mouse-mode)

  ;; in evil
  ;; need restart emacs to reflect changing
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map))
  )

;; ----------------------------------------------------------------------
(use-package popper
;;  :disabled
  :ensure t ; or :straight t
  :bind (("M-0"   . popper-toggle-latest)
         ("M--"   . popper-cycle)
         ("C-M-0" . popper-toggle-type)
         :map popper-mode-map
         ("M-u"   . my-prev-tab)
         ("M-i"   . my-next-tab))
  :init

  ;; my-tabbar-buffer-list と popper-reference-buffers でとちらで表示するかを考える
  ;;                tabbar    popper
  ;;    -----------------------------
  ;;    *scratch*      o        -
  ;;    vterm          o        -
  ;;    reb            -        o
  (setq popper-reference-buffers
        '(
          "\\*Backtrace\\*"
          "\\*Apropos\\*"
          "\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*"
          "\\*Async Shell Command\\*"
          "\\*quickrun\\*"
          ;; "\\*scratch\\*"
          ;; "^>.*$"  vterm-mode  ; see `vterm-buffer-name-string'
          ;; shortdoc
          reb-mode
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)             ; For echo area hints

  ;; :custom
  (setq popper-mode-line nil)       ; hide modeline from popper
  (setq popper-echo-dispatch-keys '())
  (setq popper-echo-prompt "")
  (setq popper-echo-prompt-group-format "GG (%%s)")
  (setq popper-echo-delimiter "|")

  :config
  (defun my-popper-echo () (interactive) (popper-echo))

  (defun my-adv--before--close-popper (&rest _)
    "Close popper window prior to execute specified　command.
For example, `consult-recent-file' try to embed its preview into popper window if popper already opened. This advice can be used to prevent these glitch."
    (when popper-popup-status
      ;; (message "close popper")
      (popper-close-latest)))
  (advice-add 'consult-recent-file :before #'my-adv--before--close-popper)

  (defun my-next-tab-1 (func)
    "Move to previous window before call the `tabbar-forward/backward-tab' to prevent tabbar embed buffer content into popper window."
    (cond ((minibuffer-p) 'nop)
          (popper-popup-status
           (with-selected-window (previous-window nil 'no-minibuf)
             (funcall func)))
          (t (funcall func)))
    )

  (defun my-next-tab ()
    "My customized `tabbar-forward-tab' to consider the popper and flycheck-posframe."
    (interactive)
    (my-next-tab-1 #'tabbar-forward-tab))

  (defun my-prev-tab ()
    "My customized `tabbar-backward-tab' to consider the popper and flycheck-posframe."
    (interactive)
    (my-next-tab-1 #'tabbar-backward-tab))
  )

;; ----------------------------------------------------------------------
(use-package quickrun
  :config
  ;; (defun my-adv-around--quickrun--with-selected-window (orig-fn &rest plist)
  ;;     (apply orig-fn plist)
  ;;     (other-window -1))
  ;; (advice-add 'quickrun :around #'my-adv-around--quickrun--with-selected-window)

  (defun my-hook--quickrun-after-run-hook--with-selected-window ()
      (other-window -1))
  (add-hook 'quickrun-after-run-hook #'my-hook--quickrun-after-run-hook--with-selected-window)

  (defalias 'r 'quickrun)

  ;; :bind (("C-c C-c"   . quickrun))	;; global マップにはバインドしない
  )

;; ----------------------------------------------------------------------
(use-package mode-line-bell
  :custom
  (mode-line-bell-flash-time 0.2)

  :config
  (mode-line-bell-mode)

  ;; redefined
  (defun mode-line-bell--begin-flash ()
    "Begin flashing the mode line."
    (unless mode-line-bell--flashing
      ;; (invert-face 'mode-line)
      (invert-face 'mode-line-active)           ;; mod
      (setq mode-line-bell--flashing t)))

  ;; redefined
  (defun mode-line-bell--end-flash ()
    "Finish flashing the mode line."
    (when mode-line-bell--flashing
      ;; (invert-face 'mode-line)
      (invert-face 'mode-line-active)           ;; mod
      (setq mode-line-bell--flashing nil)))
  )

;; ----------------------------------------------------------------------
;; https://gist.github.com/jordonbiondo/6385874a70420b05de18
;; e.g. M-x consult-imenu RET use evil
(use-package imenu
  :config
  (defun zakame/imenu-use-package ()
    (add-to-list 'imenu-generic-expression
                 '("use-package"
                   "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
  (add-hook 'emacs-lisp-mode-hook #'zakame/imenu-use-package)
  )

;; ----------------------------------------------------------------------
(use-package my-backup
  :load-path "elisp"
  :defer 3

  :config
  (setq my-backup-directory "~/bak")

  (defalias 'bak 'my-backup)
  (defalias 'bak-list 'my-backup-show-list)
  (defalias 'bak-restore 'my-backup-restore)

  )
;; ----------------------------------------------------------------------
(use-package magit
  :disabled     ;; temporary
  :defer t
  :custom
  (magit-auto-revert-mode nil)

  :config
  (dolist (f '(magit-diff-our-highlight
               magit-diff-base-highlight
               magit-diff-added-highlight
               magit-diff-their-highlight
               magit-diff-context-highlight
               magit-diff-removed-highlight))
    (set-face-attribute f nil :weight 'normal))
  )
;; ----------------------------------------------------------------------
(use-package markdown-mode
  :custom
  (truncate-lines nil)
  )

;; ----------------------------------------------------------------------
(use-package dts-mode
  ;; :disabled
  :mode ("\\(\\.keymap\\|\\.overlay\\)$" . dts-mode)
  ;; :mode (("\\.keymap\\'"  . dts-mode)      ;; for zephyr/zmk
         ;; ("\\.overlay\\'" . dts-mode))
  :hook (dts-mode . (lambda () (setq comment-start "//"
                                     comment-end   "")))
  )

;; ----------------------------------------------------------------------
(use-package v-mode
  :mode ("\\(\\.v?v\\|\\.vsh\\)$" . 'v-mode)
  ;; :hook ((before-save . v-format-buffer))
  :bind (("C-c C-c" . quickrun)
         ("C-c C-v" . v-menu)
         ("C-c C-f" . v-format-buffer))

  :config
  (defun v-build-tags ()
    ;; nop
    )
  (defun v-after-save-hook ()
    ;; nop
    )
  )

;; ----------------------------------------------------------------------
(use-package lua-mode
  :mode "\\.lua\\'"
  :bind (("C-c C-c" . quickrun)
         )

  :config
  )

;; ----------------------------------------------------------------------
(use-package kconfig-mode
  :mode "\\^Kconfig.\\(defconfig\\|shield\\)\\'"
  :load-path "elisp/kconfig-mode"      ;; https://github.com/delaanthonio/kconfig-mode
  )

;; ----------------------------------------------------------------------
(use-package go-translate
  :init
  (use-package web-api-auth-key
    :load-path dropbox-dir)   ;; defined at _mac.el or _windows.el

  :config

  ;; your languages pair used to translate
  (setq gts-translate-list '(("en" "ja") ("ja" "en")))

  ;; config the default translator, it will be used by command gts-do-translate
  (setq gts-default-translator
        (gts-translator

         :picker ; used to pick source text, from, to. choose one.

         ;;(gts-noprompt-picker)
         ;;(gts-noprompt-picker :texter (gts-whole-buffer-texter))
         (gts-prompt-picker)
         ;;(gts-prompt-picker :single t)
         ;;(gts-prompt-picker :texter (gts-current-or-selection-texter) :single t)

         :engines ; engines, one or more. Provide a parser to give different output.

         (list
          ;;(gts-bing-cn-engine)
          ;;(gts-google-engine)
          ;;(gts-google-rpc-engine)
          (gts-deepl-engine :auth-key web-api-auth-key-deepl :pro nil)
          ;;(gts-google-engine :parser (gts-google-summary-parser))
          ;;(gts-google-engine :parser (gts-google-parser))
          ;;(gts-google-rpc-engine :parser (gts-google-rpc-summary-parser) :url "https://translate.google.com")
          ;;(gts-google-rpc-engine :parser (gts-google-rpc-parser) :url "https://translate.google.com")
          )

         :render ; render, only one, used to consumer the output result. Install posframe yourself when use gts-posframe-xxx

         ;; (gts-buffer-render)
         ;; (gts-posframe-pop-render)
         (gts-posframe-pop-render :backcolor "dim gray" :forecolor "#ffffff" :padding 10)
         ;; (gts-posframe-pin-render)
         ;;(gts-posframe-pin-render :position (cons 1200 20))
         ;;(gts-posframe-pin-render :width 80 :height 25 :position (cons 1000 20) :forecolor "#ffffff" :backcolor "#111111")
         ;;(gts-kill-ring-render)
         ))

  ;;(setq go-translate-buffer-follow-p t)       ; focus the result window
  ;;(setq go-translate-buffer-source-fold-p t)  ; fold the source text in the result window
  ;;(setq go-translate-buffer-window-config ..) ; config the result window as your wish

  (setq gts-posframe-pop-render-timeout 'infinit)
  (define-key evil-visual-state-map (kbd "t") #'gts-do-translate)

  ;; (defun my-gts-posframe-show-function ()
  ;;   (posframe-hide-all)
  ;;   (run-with-timer 0.1 nil #'evil-exit-visual-state))


  (add-hook 'gts-after-buffer-render-hook #'(lambda ()
            (evil-define-key 'normal gts-buffer-local-map [escape] #'posframe-hide-all)
            (define-key gts-buffer-local-map (kbd "C-g") #'posframe-hide-all)))
  )

;; ----------------------------------------------------------------------
;; emacs lisp reference manual (Japanese)
;;
;; todo does not work!
;;
;; (let ((dir "~/.emacs.d/emacs-25.1-doc-lispref-master/"))
;;   (setq Info-default-directory-list
;;         (append Info-default-directory-list (list (expand-file-name dir))))
;;   (defun Info-find-node--elisp-ja (orig-fn filename &rest args)
;;     (apply orig-fn
;;            (pcase filename
;;              ("elisp" "elisp-ja")
;;              (_ filename))
;;            args))
;;   (advice-add 'Info-find-node :around 'Info-find-node--elisp-ja))

;; ----------------------------------------------------------------------
;; customize setting
(setq custom-file "~/.emacs.d/custom.el") ; write custom settings into external file instead of init.el
(load custom-file nil t)

;; disable start greeting message such as "for information about gnu emacs and the gnu system type c-h c-a"
(fset 'display-startup-echo-area-message 'ignore)
(setq inhibit-startup-message t)

;; show emacs version and startup time in mini-buffer
 (message "%s / %.3f sec"
          (substring (version) 0 14)
          (float-time (time-subtract after-init-time before-init-time)))

;;; init.el ends here
