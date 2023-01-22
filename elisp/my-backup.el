;;; my-backup.el --- Trivial backup utility -*- coding: utf-8-unix; mode: emacs-lisp -*-

;;; Commentary:

;; description:
;;      カレントバッファのバックアップファイルを作成する
;;      デフォルトファイル名は *.bakXX (XXは00から始まるサフィックス番号)
;;      すでに同名ファイルが存在する場合は *.bak01, *.bak02....となる
;;
;;
;; requirements:
;;      consult
;;      marginalia?
;;
;; usage:
;;      バックアップファイル作成
;;      M-x bak           デフォルト拡張子を使用
;;      C-u M-x bak       オリジナル拡張子を使用
;;      C-u C-u M-x bak   使用する拡張子をたずねる
;;
;;      バックアップファイルの一覧をdiredで閲覧
;;      M-x bak-list
;;
;;
;; install:
;;      add into your dot.emacs.
;;
;;      (require 'my-backup)
;;
;;
;; customize: (in your ".emacs")
;;
;;      ;; デフォルトの拡張子(前置ドット不要)
;;      ;; カスタマイズしない場合は"bak"
;;      (setq my-backup-default-ext "backup")
;;
;;      ;; オリジナルの拡張子(前置ドット不要)
;;      ;; カスタマイズしない場合は"orig"
;;      (setq my-backup-original-ext "original")
;;
;;      ;; バックアップファイル作成先のディレクトリ
;;      ;; nilの場合は編集中のファイルと同じ場所とみなす。
;;      ;; カスタマイズしない場合は nil
;;      (setq my-backup-directory "~/backup_files")


(require 'consult)          ;; consult--read
(require 'discrete)         ;; path-join

;;; Code:
(defvar my-backup-default-ext "bak"
  "デフォルトの拡張子。前置ドットは不要。")

(defvar my-backup-original-ext "orig"
  "オリジナルの拡張子。前置ドットは不要。")

(defvar my-backup-directory nil
  "バックアップファイル作成先のディレクトリ。
nilの場合は編集中のファイルと同じ場所とみなす。")

;;; Code:

(defun my-backup-get-suffixed-file-name (src)
  " ﾃﾞﾌｫﾙﾄの拡張子の適当なｻﾌｨｯｸｽﾅﾝﾊﾞｰを探し、そのファイル名を返す。"
  (let ((ext (concat "." my-backup-default-ext))
	(n 0)
	(nyet t)
	name)
    (while nyet
      (setq name (concat src ext (format "%02d" n)))
      (if (file-exists-p name)
	  ; 既に同名ﾌｧｲﾙがあれば次のｻﾌｨｯｸｽﾅﾝﾊﾞｰをあたる
	  (setq n (1+ n))
	; ﾌｧｲﾙがなければそのｻﾌｨｯｸｽﾅﾝﾊﾞｰを採用しﾌｧｲﾙ名を決定する
	(setq nyet nil)))
    name))

(defun my-backup-read-ext-from-minibuffer (body)
  (let ((ext (read-no-blanks-input "extension: "
				  my-backup-original-ext)))
    (unless (string-match "^[.].*" ext)
      (setq ext (concat "." ext)))
  (concat body ext)))

(defun my-backup (arg)
  "現在編集中のファイルのバックアップコピーを作成する。

編集中のバッファが変更されているかどうかに関わらず、最終保存時のファイルをコ
ピーしてバックアップファイルとする。

         M-x bak   デフォルト拡張子(\"bak\"等)を使用
     C-u M-x bak   オリジナルの拡張子(\"orig\"等)を使用
 C-u C-u M-x bak   使用する拡張子をたずねる

デフォルト拡張子の場合は
  foo.bak01, foo.bak02, foo.bak03....
と自動的にサフィックス番号がインクリメントされる。

オリジナルの拡張子を指定時、もし同名ファイルがある場合は保存しない。

カスタマイズ例
;; デフォルトの拡張子。\(前置ドット不要\)
;; カスタマイズしない場合は\"bak\"
\(setq my-backup-default-ext \"backup\"\)

;; オリジナルの拡張子。\(前置ドット不要\)
;; カスタマイズしない場合は\"orig\"
\(setq my-backup-original-ext \"original\"\)

;; バックアップファイル作成先のディレクトリ。
;; nilの場合は編集中のファイルと同じ場所とみなす。
;; カスタマイズしない場合は nil
\(setq my-backup-directory \"~/backup_files\"\)"
  (interactive "P")
  (let ((ext ".")
	(bak-filename nil)
	(dir nil)
	(filename-body nil))
    ; ﾊﾞｯﾌｧのﾌｧｲﾙ名があるかﾁｪｯｸ
    (if (buffer-file-name)
	(setq filename-body (file-name-nondirectory (buffer-file-name)))
      (message "this buffer has no file."))
    ; ﾌｧｲﾙ名がある場合のみ次の処理へ
    (when filename-body
      (if my-backup-directory
	  ; ﾃﾞﾌｫﾙﾄのﾊﾞｯｸｱｯﾌﾟ先が指定されている場合
	  (let ((d (expand-file-name
		    (file-name-as-directory my-backup-directory))))
	    (if (and (file-directory-p d)
		     (file-writable-p (make-temp-name
				       (concat d "my-backup"))))
		; ﾊﾞｯｸｱｯﾌﾟ先が書き込み可能の場合
		(setq dir d)
	      ; ﾊﾞｯｸｱｯﾌﾟ先が書き込み不可の場合
	      (message (format "Cannot backup to \"%s\"." d))))
	; ﾃﾞﾌｫﾙﾄのﾊﾞｯｸｱｯﾌﾟ先が指定されてない場合は編集中のﾌｧｲﾙと同じ場所とする
	(setq dir (file-name-directory (buffer-file-name)))))
    ; ﾊﾞｯｸｱｯﾌﾟ先のﾃﾞｨﾚｸﾄﾘが確定した場合のみ次の処理へ
    (when dir
      (setq bak-filename
	    (cond
	     ((not arg)			;前置引数なしの場合
	      (my-backup-get-suffixed-file-name (concat dir filename-body)))
	     ((= (car arg) 4)		;前置引数=1コの場合
	      (concat dir filename-body ext my-backup-original-ext))
	     ((= (car arg) 16)		;前置引数=2コの場合
	      (my-backup-read-ext-from-minibuffer (concat dir filename-body)))))
      ; 再度同名ﾌｧｲﾙがないことを確認
      (if (file-exists-p bak-filename)
	  ; 同名ﾌｧｲﾙがある場合はｴﾗｰ
	  (message (format "\"%s\" already exists." bak-filename))
	; 同名ﾌｧｲﾙがない場合のみﾊﾞｯｸｱｯﾌﾟﾌｧｲﾙを作成
	; ﾊﾞｯｸｱｯﾌﾟﾌｧｲﾙは編集中のﾌｧｲﾙの最終保存状態の単なるｺﾋﾟｰであり、
	; ｶﾚﾝﾄﾊﾞｯﾌｧを書き出しているわけではない。
	(copy-file (expand-file-name (buffer-file-name)) bak-filename)
	(message (format "backup --> \"%s\""
                     (if my-backup-directory
                          (let ((directory-abbrev-alist `((,(concat "\\`" (getenv "HOME")) . "~"))))
                            (abbreviate-file-name bak-filename))
                       (concat "./" (file-name-nondirectory bak-filename)))))))))


;; (defun my-backup-dired-list ()
;;   "現在編集中のバックアップファイルのリストをdiredで表示する。"
;;   (interactive)
;;   (let ((orig nil)
;; 	(dir nil)
;; 	(wild nil))
;;     (if (buffer-file-name)
;; 	(setq orig (expand-file-name (buffer-file-name)))
;;       (message "this buffer has no file."))
;;     (when orig
;;       (if my-backup-directory
;;  	  ; ﾃﾞﾌｫﾙﾄのﾊﾞｯｸｱｯﾌﾟ先が指定されている場合
;; 	  (let ((d (file-name-as-directory my-backup-directory)))
;; 	    (if (file-directory-p d)
;; 		(setq dir d)
;; 	      (message "invalid backup directory \"%s\"." d)))
;; 	; ﾃﾞﾌｫﾙﾄのﾊﾞｯｸｱｯﾌﾟ先が指定されてない場合は編集中のﾌｧｲﾙと同じ場所とする
;; 	(setq dir (file-name-directory (buffer-file-name)))))
;;     (when dir
;;       (setq wild (concat (file-name-nondirectory orig) "*"))
;;
;;       (let ((dired-listing-switches "-lt"))
;;         (dired (concat dir wild))))))

(defun my-backup-get-backup-list (orig)
  "Return the list of backup of original file specified by buffer ORIG.
Each elemnt is an cons like '(\"foo.txt.bak01\" . \"2023/01/22 06:14:40\").
It is similar to the result of shell command 'ls -1t DIR/ORIG-NAME.bak*'
 but without file-mode attributes."
  (let ((sorted (sort (directory-files-and-attributes my-backup-directory nil (format "%s.bak*" orig) 'no-sort)
                #'(lambda (y x) (time-less-p (nth 6 x) (nth 6 y))))))
    (mapcar #'(lambda (x) (list (car x) (format-time-string "%Y/%m/%d %T" (nth 6 x)))) sorted)))

(defun my-backup-show-list ()
  "Show backup list of the file shown in current buffer.

[todo: NOT IMPLEMENT YET]
 There are some additional functions in this command like `dired'.

	RET	open this backup file
	D	Delete this backup file
	C	Delete backup files that exceed the maximum number of backups
	d	Preview diff between this backup file and current buffer"
  (interactive)
  (let* ((dest-buf (current-buffer)))
    (consult--read (my-backup-get-backup-list dest-buf)            ;; todo annotate datetime by marginalia
                   :prompt "backups: " :sort nil  :require-match t
                   :initial (buffer-name dest-buf)))
  ;; no addtional functions
  ;; todo: use embark-act?
  )

(defun my-backup-restore (&optional latest no-confirm no-reload)
  "Restore the file shown in current buffer from its backup.
In a nut shell, this command is equivalent to the shell command
\"cp -a BAK ORIG\".

If optional first argument LATEST is non-nil restore it from the
latest backup, otherwise it　shows list of backups that you can
choose what restore it from.

If optional second argument NO-CONFIRM is non-nil do restoring
without any confirmation, otherwise it confirms you really intend
to restore it beforehand.

If optional third argument NO-RELOAD is non-nil the current buffer
is NOT reloaded after the restoring, otherwise it will be reloaded
automatically."
  (interactive)
  (let* ((dest-buf (current-buffer))
         (dest-path (expand-file-name (or (buffer-file-name dest-buf)
                                          (error "Buffer \"%s\" does not visit any file" dest-buf))))
         (src-path (path-join (expand-file-name my-backup-directory)
                              (progn
              (unless (file-exists-p dest-path)
                (error "File \"%s\" does not exist. Not save yet?" dest-path))
              (let ((backup-list (my-backup-get-backup-list dest-buf)))
                (if (not backup-list)
                    (error "No backups at all")
                  (if latest
                      (car (my-backup-get-backup-list dest-buf))
                    (consult--read (my-backup-get-backup-list dest-buf)
                                   ;; todo add datetime annotatation by marginalia
                                   :prompt "Choose backup: " :sort nil  :require-match t
                                   :initial (buffer-name dest-buf)))))))))
    (unless no-confirm
      (unless (y-or-n-p (format "Sure? %s <-- %s "
                                (abbreviate-file-name dest-path) (abbreviate-file-name src-path)))
        (error "Quit")))

    (copy-file src-path dest-path t t t)    ;; restoring

    (unless no-reload
      (revert-buffer nil t))                ;; reload current buffer automatically
    (message "Restored <-- %s" (expand-file-name src-path))
    ))

;; for autoload
(provide 'my-backup)

;;; my-backup.el ends here
