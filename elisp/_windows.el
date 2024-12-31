;;; -*- coding:utf-8; mode:emacs-lisp -*-
;;;
;;; _windows.el
;;;
;;; Windowsの環境変数HOMEに"D:\_docu"を設定しておくこと！
;;; でないとthemeがロードされない→my-evil-normal-tag-faceが未定義とか言われるので注意

(message "--> loading \"_windows.el\"...")

;; 外部コマンドはmsys2/mingwを使う
;;D:\pgm\msys64\usr\bin
(setenv "PATH" (concat "D:\\pgm\\msys64\\mingw64\\bin;"
                       ;; "D:\\pgm\\msys64\\mingw64\\usr\\bin;"
                       (getenv "PATH")))
;; (setenv "PATH" (concat "D:\\pgm\\msys64\\usr\\bin;" (getenv "PATH")))

(add-to-list 'exec-path "D:/pgm/msys64/mingw64/bin")

;; ;;
;; ;; native compile for windows (copy from MSYS2)
;; (custom-set-variables
;;  '(native-comp-driver-options '("-B" "D:/pgm/Emacs/for_native_comp"))
;;  )
;; 事前にやっておく
;; $ cp /d/pgm/Emacs/for_native_comp/libgccjit-0.dll /d/pgm/Emacs/emacs-29.1/bin/

;; (setq bin-windows-path (expand-file-name (path-join (file-name-directory user-init-file) "bin-windows")))
;; (add-to-list 'exec-path bin-windows-path)
;; (add-to-list 'exec-path (path-join bin-windows-path "PortableGit/bin"))
;; (setq my-counsel-rg-exe (path-join bin-windows-path "rg.exe"))

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
;(require 'ucs-normalize)
;(setq file-name-coding-system 'utf-8-hfs)
;(setq locale-coding-system 'utf-8-hfs)

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
  (let ((wleft (if (= (cl-fourth (car (frame-monitor-attributes))) 3840)
                   1209          ; EIZO EV3237 (1920x1080)
                 nil)))          ; others (temporary)

    (setq initial-frame-alist `(
                              (top    . 0)
                              (left   . ,wleft)
                              (height . 63)
                              (width  . 110))))
  (setq default-frame-alist initial-frame-alist)

  ;; font
  ;(set-default-font (myfont 'default)) ;; ~26.3
  (set-frame-font (myfont 'default))	;; 27.2~

  ;; for im-on/off in init.el
  (defun im-ctl (on)
    ;; (mac-input-source)
;    (mac-select-input-source
;     (if on
         ;; google 日本語入力
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
  (setq my-face-adj-line-number-height 1.2)
  (setq my-face-adj-tabbar-height 1.2)
  (setq my-face-adj-mode-line-height 1.2)
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
      )
;    (set-cursor-color (if (string-match "\\.US$" (mac-input-source))
;                          my-cursor-color-bak
;                        (mycolor 'red))))

;  (add-hook 'mac-selected-keyboard-input-source-change-hook 'my-mac-selected-keyboard-input-source-change)

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

  ;; ;; nim-mode
  ;; (let ((bin-path (expand-file-name "~/.nimble/bin")))
  ;;  (add-to-list 'exec-path bin-path)
  ;;  (setenv "PATH" (concat bin-path ":" (getenv "PATH")))
  ;;  )
  )

;; go-translate
(setq dropbox-dir "D:/Dropbox")



(message "<-- done    \"_windows.el\"")


(provide '_windows)
;; _windows.el ends here
