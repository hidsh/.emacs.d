;;; -*- coding:utf-8; mode:emacs-lisp -*-
;;;
;;; _linux.el
;;;
(message "--> loading \"_linux.el\"...")

(add-to-list 'exec-path (expand-file-name "/bin"))
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))
;(setq my-counsel-rg-exe (expand-file-name "~/bin/rg"))

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; (setq coding-system-for-read 'utf-8-unix)
;; (setq coding-system-for-write 'utf-8-unix)

;; http://sakito.jp/emacs/emacsshell.html#id7

;;
;; for emacs mac port
;;

;; 以降はGUIのときのみ
(when window-system
  ;; server 起動
  ;; ターミナルからは `$ emacsclient -nw hogefile` で使う
  ;; (require 'server)
  ;; (unless (server-running-p)
  ;;   (server-start))

  ;;;; fix fringe width when `export GDK_DPI_SCALE=1.0'
  ;;;; https://github.com/blahgeek/emacs-fringe-scale
  ;; (set-fringe-mode 16)
  ;; (require 'fringe-scale)
  ;; (fringe-scale-setup)

  ;; (setq default-frame-alist '((undecorated . t)))       ;; hide title bar
  (setq frame-resize-pixelwise t)       ;; for X tiling window manager

  ;; font
  ;; ;; 日本語が合成されたフォントを設定する場合
  ;(set-default-font (myfont 'default)) ;; ~26.3
  ;; (set-frame-font (myfont 'default))	;; 27.2~
  (set-frame-font "Cica-12")	;; 27.2~
  ;; (set-frame-font "Utatane-12")	;; 27.2~    ;; Utataneにするとisearchでwrong type argument: arrayp, nilが出る
  ;; (set-frame-font "Input MonoLight-11" nil t)	;; 27.2~

  ;; 日本語のフォントを個別に設定する場合
  ;; (set-face-attribute 'default nil :family "Input Mono" :height 120 :weight 'light)
  ;; (set-fontset-font nil '(#x80 . #x10ffff) (font-spec :family "Cica" :height 12))
  ;; (setq use-default-font-for-symbols nil)

  ;; emacs-mozc
  (add-to-list 'load-path "~/.emacs.d/local-fix/emacs-mozc")
  (require 'mozc)
  ;; (set-language-environment "Japanese")
  (set-language-environment 'utf-8)
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'overlay)
;; (prefer-coding-system 'utf-8)

  ;; for im-on/off in init.el
  (defun im-ctl (on)
    ;; (mac-input-source)

;    (mac-select-input-source
;     (if on
;         ;; google 日本語入力
;         "com.google.inputmethod.Japanese.base"
;       "com.apple.keylayout.US")))
)

  (defun im-on-p ()
;    (not (string-match "\\.US$" (mac-input-source))))
)

  ;; migemo
  ;; fixme not work
  ;; (setq migemo-command "/usr/local/bin/cmigemo")
  ;; (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

;;;
;;; appearance adjust
;;;
  (setq my-face-adj-line-number-height 1.0)
  (setq my-face-adj-tabbar-height 1.0)
  (setq my-face-adj-mode-line-height 1.0)
  ;; (add-hook 'after-init-hook 'mac-change-language-to-us)          ;; emacs 起動時は英数モードから始める
  ;; (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)    ;; minibuffer 内は英数モードにする
  ;; (add-hook 'isearch-mode-hook 'mac-change-language-to-us)        ;; [migemo]isearch のとき IME を英数モードにする

  ;; ;; アイコンやdockから起動したemacsのpathやexec-pathが正しく設定されてないのをなんとかする
  ;; ;; http://yukihr.github.com/blog/2012/03/02/emacs-path-settings/
  ;; ;; when opened from desktop entry, path won't be set to shell's value.
  ;; (let ((path-str
  ;;        (replace-regexp-in-string "\n+$" "" (shell-command-to-string "echo $path"))))
  ;;     (setenv "path" path-str)
  ;;     (setq exec-path (nconc (split-string path-str ":") exec-path)))


  ;; 日本語環境設定 for mac
  ;; http://maro.air-nifty.com/maro/2009/02/carbon-emacs-sh.html
  ;; (set-language-environment "japanese")

  ;; 入力モードが日本語の時はカーソルの色を変える
  (defvar my-cursor-color-bak nil)
  (defun my-mac-selected-keyboard-input-source-change ()
    (unless my-cursor-color-bak
      (setq my-cursor-color-bak (face-background 'cursor)))
    (set-cursor-color (if (string-match "\\.US$" (mac-input-source))
                          my-cursor-color-bak
                        (mycolor 'red))))

;  (add-hook 'mac-selected-keyboard-input-source-change-hook 'my-mac-selected-keyboard-input-source-change)

  ;; ;; ミニバッファで入力する際に自動的にASCIIにする
  ;; (when (fboundp 'mac-auto-ascii-mode)
  ;;   (mac-auto-ascii-mode 1))

  ;; fullscreen
;  (global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen)

  ;; org-mode
;  (setq org-directory "~/Dropbox/org")

  ;; check-emacs-setting
  ;; (setq check-emacs-setting-diff-pgm "/Applications/Meld.app/Contents/MacOS/Meld")
  ;; (setq check-emacs-setting-cmp-pgm "cmp")

  ;; slime
;  (setq ros-exe "/usr/local/bin/ros")
;  (setq my-slime-helper "~/.roswell/helper.el")

  ;; nim-mode
;  (let ((bin-path (expand-file-name "~/.nimble/bin")))
;   (add-to-list 'exec-path bin-path)
;   (setenv "PATH" (concat bin-path ":" (getenv "PATH")))
;
;   )
  )

;; go-translate
(setq dropbox-dir "~/Dropbox")

(message "<-- done    \"_linux.el\"")
(provide '_linux)
;;; _linux.el ends here
