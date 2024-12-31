;;; -*- coding:utf-8; mode:emacs-lisp -*-
;;;
;;; _mac.el
;;;
(message "--> loading \"_mac.el\"...")

(add-to-list 'exec-path (expand-file-name "~/bin"))
(setq my-counsel-rg-exe (expand-file-name "~/bin/rg"))

(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;; (setq coding-system-for-read 'utf-8-unix)
;; (setq coding-system-for-write 'utf-8-unix)

;; http://sakito.jp/emacs/emacsshell.html#id7

;; mac os x の hfs+ ファイルフォーマットではファイル名は nfd (の様な物)で扱うため以下の設定をする必要がある
(require 'ucs-normalize)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

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

  ;; window size and position
  (let ((wleft (if (= (cl-fourth (car (frame-monitor-attributes))) 1920)
                   1009          ; EIZO EV3237 (1920x1080)
                 529)))          ; Macbook Air late 2020 (2560x1600)

    (setq initial-frame-alist `(
                              (top    . 0)
                              (left   . ,wleft)
                              (height . 64)
                              (width  . 110))))
  (setq default-frame-alist initial-frame-alist)

  ;; font
  ;(set-default-font (myfont 'default)) ;; ~26.3
  (set-frame-font (myfont 'default))	;; 27.2~

  ;; for im-on/off in init.el
  (defun im-ctl (on)
    ;; (mac-input-source)
    (mac-select-input-source
     (if on
         ;; google 日本語入力
         "com.google.inputmethod.Japanese.base"
       "com.apple.keylayout.US")))

  (defun im-on-p ()
    (not (string-match "\\.US$" (mac-input-source))))

  ;; migemo
  ;; fixme not work
  ;; (setq migemo-command "/usr/local/bin/cmigemo")
  ;; (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

;;;
;;; appearance adjust
;;;
  (setq my-face-adj-line-number-height 1.1)
  (setq my-face-adj-tabbar-height 1.1)
  (setq my-face-adj-mode-line-height 1.1)
  ;; (add-hook 'after-init-hook 'mac-change-language-to-us)          ;; emacs 起動時は英数モードから始める
  ;; (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)    ;; minibuffer 内は英数モードにする
  ;; (add-hook 'isearch-mode-hook 'mac-change-language-to-us)        ;; [migemo]isearch のとき IME を英数モードにする

  ;; ;; アイコンやdockから起動したemacsのpathやexec-pathが正しく設定されてないのをなんとかする
  ;; ;; http://yukihr.github.com/blog/2012/03/02/emacs-path-settings/
  ;; ;; when opened from desktep entry, path won't be set to shell's value.
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

  (add-hook 'mac-selected-keyboard-input-source-change-hook 'my-mac-selected-keyboard-input-source-change)

  ;; ;; ミニバッファで入力する際に自動的にASCIIにする
  ;; (when (fboundp 'mac-auto-ascii-mode)
  ;;   (mac-auto-ascii-mode 1))

  ;; fullscreen
  (global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen)

  ;; org-mode
  (setq org-directory "~/Dropbox/org")

  ;; check-emacs-setting
  ;; (setq check-emacs-setting-diff-pgm "/Applications/Meld.app/Contents/MacOS/Meld")
  ;; (setq check-emacs-setting-cmp-pgm "cmp")

  ;; slime
  (setq ros-exe "/usr/local/bin/ros")
  (setq my-slime-helper "~/.roswell/helper.el")

  ;; nim-mode
  (let ((bin-path (expand-file-name "~/.nimble/bin")))
   (add-to-list 'exec-path bin-path)
   (setenv "PATH" (concat bin-path ":" (getenv "PATH")))

   )
  )

;; go-translate
(setq dropbox-dir "~/Library/CloudStorage/Dropbox")


(message "<-- done    \"_mac.el\"")

(provide '_mac)
;;; _mac.el ends here
