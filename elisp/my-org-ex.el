;;;
;;; my-org-ex.el (extracted from my init.el)
;;;

;; ----------------------------------------------------------------------
(use-package org
  :disabled t
  :if window-system
  :config
  ;; (setq org-directory "~/Dropbox/org")       ;; defined in _windows.el or _mac.el
  (setq org-default-notes-file (expand-file-name (path-join org-directory "notes.org")))

  (setq org-hide-emphasis-markers t)
  (setq org-todo-keywords '((sequence "[ ]" "[!]" "|" "[X]" )))
  (setq org-capture-templates
        '(("t" "Todo" checkitem (file org-default-notes-file) "" :unnarrowed t)
          ("m" "Memo" entry     (file org-default-notes-file) "* %?" :unnarrowed t)))


  (set-face-attribute 'org-level-2 nil :foreground (face-foreground 'default))

  (set-face-attribute 'org-todo nil :foreground (mycolor 'pink) :background (face-background 'default) :weight 'bold)
  (set-face-attribute 'org-checkbox-statistics-todo nil :foreground (face-foreground 'default) :background (face-background 'default) :weight 'normal)

  (set-face-attribute 'org-done nil :foreground (mycolor 'green) :background (face-background 'default) :weight 'bold)
  (copy-face 'org-done 'org-checkbox-statistics-done)

  (defface my-org-done-date-face
    `((t (:inherit org-todo :foreground ,(face-background 'org-done) :background ,(face-foreground 'org-done) :weight bold))) "")

  (defun font-lock-user-keywords (mode &optional keywords)
    "Add user highlighting to KEYWORDS to MODE.
See `font-lock-add-keywords' and `font-lock-defaults'."
    (unless mode
      (error "Mode should be non-nil"))
    (font-lock-remove-keywords mode (get mode 'font-lock-user-keywords))
    (font-lock-add-keywords mode keywords)
    (put mode 'font-lock-user-keywords keywords))

  (font-lock-user-keywords 'org-mode '(
    ;; todo
    ("^*+ \\[X\\] \\( [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\)\\(.+\\)$" . '(1 'my-org-done-date-face))
    ("^*+ \\[X\\] \\( [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\)\\(.+\\)$" . '(2 'org-done))
    ("^*+ \\[!\\] \\(.+\\)$" . '(1 'org-todo))
    ;; "-" --> "•"
    ("^ *\\([-]\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))
    ;; "* [ ]" --> "[ ]"
    ("^\\(*+ \\)\\[.\\] " (0 (progn () (add-text-properties (match-beginning 1) (match-end 1) '(invisible t)))))
    ;; "https://..." --> ""
    ;; ("\\(http[s]*://.+\\)[ \n]" (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
    ))
  ;; (org-set-font-lock-defaults)
  ;; (font-lock-fontify-buffer)

  ;; ----------
  (defun my-org-get-todo-content ()
    "Return string as todo content if current line has todo content. Otherwise return nil"
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward "^*+ \\[ \\] +\\(.+\\)" (line-end-position) t)
          (match-string 1)
        nil)))

  (defun my-org-todo-get-title ()
    "Return string as todo title if it found. Otherwise return nil"
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward "^*+ \\[ \\] \\(.+\\)" (line-end-position) t)
          (if (re-search-backward "^*+ \\([^[]+\\)\\( \\[.+\\]\\)?$" nil t)
              (string-trim (match-string 1))
            nil)
        nil)))

  (defun my-org-kill-whole-line (&optional point)
    (when point
      (goto-char point)
      (beginning-of-line))
    (org-kill-line)
    (org-kill-line))

  (defvar my-org-move-to-never-do-dest-title "やらないことリスト")
  (defun my-org-move-to-never-do (reason title-orig)
    (let ((title my-org-move-to-never-do-dest-title)
          (pt (save-excursion (my-org-beginning-of-content) (point)))
          (content (my-org-get-todo-content)))
      (when content
        (condition-case err
            (save-excursion
              (goto-char (point-min))
              (re-search-forward (concat "^* " title))
              (forward-line 1)
              (insert (format "** [ ] :%s: %s ← %s\n" title-orig content reason)))
          (error (progn (goto-char pt)
                        (format "Not found: %s" title)))))))

  ;; ----------
  (defun my-org-capture-add-1 (type text)
    (interactive)
    (let ((buf (current-buffer))
          (pt (point))
          (tr-re "[ \t\n\r　]+")
          (title (cond ((eq type 'todo) "目安箱")
                       ((eq type 'memo) "memo")
                       (t nil)))
          (fmt (cond ((eq type 'todo) "** [ ] %s\n")
                     ((eq type 'memo) "** %s\n"))))
      (setq text (string-trim text tr-re tr-re))
      (when (and (not (string= text "")) title)
        (find-file org-default-notes-file)
        (goto-char (point-min))
        (if (re-search-forward (concat "^* " title))
            (progn
              (forward-line 1)
              (insert (format fmt text)))
          (message (format "Not found: %s" title)))
        (if (eq (current-buffer) buf)
            (goto-char pt)
          (let ((inhibit-message t))
            (save-buffer))
          (bury-buffer)))))

  (defun my-org-capture-add-todo (text)
    (interactive "sTODO: ")
    (my-org-capture-add-1 'todo text))

  (defun my-org-capture-add-memo (text)
    (interactive "sMEMO: ")
    (my-org-capture-add-1 'memo text))

  (defun my-org-notes-open ()
    (interactive)
    (if (member org-default-notes-file (org-files-list))
        (let* ((buf-org (get-file-buffer org-default-notes-file))
               (win-org (get-buffer-window buf-org)))
          (if (and buf-org win-org)          ;; org-file is already shown in any windows
              (delete-windows-on buf-org)
            (find-file org-default-notes-file)))
      (find-file org-default-notes-file)))

  (defun my-org-notes-close ()
    (interactive)
    (if (string= (buffer-file-name) org-default-notes-file)
        (progn
          (my-org-global-fold-cycle-folding-store)
          (save-buffer)
          (bury-buffer))
      (my-org-notes-open)))

  ;; ----------
  (defun my-org-goto-title-next ()
    (interactive)
    (my-org-goto-title-next-1 nil))

  (defun my-org-goto-title-prev ()
    (interactive)
    (my-org-goto-title-next-1 t))

  (defun my-org-goto-title-next-1 (backward-p)
    (let ((pt (point))
          (re "^* [^[].+$"))
      (if backward-p
          (if (progn (beginning-of-line) (re-search-backward re nil t))
              (my-org-beginning-of-content)
            (goto-char pt))
        (if (progn (end-of-line) (re-search-forward re nil t))
            (my-org-beginning-of-content)
          (goto-char pt)))))

  ;; ----------
  (defun my-org-todo-goto-working-forward ()
    (interactive)
    (let ((pt (point)))
      (unless (progn (end-of-line) (re-search-forward  "^*+ \\[!\\] " nil t))
        (goto-char pt))))

  (defun my-org-todo-goto-working-backward ()
    (interactive)
    (let ((pt (point)))
    (unless (prog2 (goto-char (line-beginning-position))
                (re-search-backward "^*+ \\[!\\] " nil 1)
              (goto-char (+ (point) (- (match-end 0) (match-beginning 0)))))
      (goto-char pt))))

  ;; ----------
  (defun my-org-dup-heading-up ()
    (interactive)
    (unless (my-org-dup-heading-1 t)
      (evil-open-above 1)))

  (defun my-org-dup-heading-down ()
    (interactive)
    (unless (my-org-dup-heading-1 nil)
      (evil-open-below 1)))

  (defun my-org-dup-heading-1 (up)
    (let ((beg (line-beginning-position))
          (end (line-end-position))
          (pt (point)))
      (goto-char beg)
      (if (re-search-forward "^*+ \\[.\\] \\|^*+ " end t)
          (let ((s (buffer-substring beg (point))))
            (if up
                (evil-open-above 1)
              (evil-open-below 1))
            (insert (replace-regexp-in-string "\\[.\\]" "[ ]" s))
            (unless (eq evil-state 'insert)
              (evil-insert-state 1))
            (org-update-parent-todo-statistics)
            t)
        (goto-char pt)
        nil)))

  ;; ----------
  (defun my-org-beginning-of-content ()
    (interactive)
    (let ((beg (line-beginning-position))
          (end (line-end-position))
          (pt (point)))
      (goto-char beg)
      (if (re-search-forward "^*+ \\[.\\] \\|^* " end t)
          (when (= (point) pt)
            (org-beginning-of-line))
        (org-beginning-of-line))))

  ;; ----------
(defun my-org-global-fold-cycle ()
    (interactive)
    (cl-labels ((= (state) (eq my-org-global-fold-cycle-state state))
                (-> (state) (setq my-org-global-fold-cycle-state state))
                (fold-restore () (my-org-global-fold-cycle-folding-restore))
                (fold-backup  () (my-org-global-fold-cycle-folding-store))
                ;; (message-state () (message "Folding: %s" my-org-global-fold-cycle-state)))
                (message-state () nil))
      (cond ((= 'user)     (-> 'hide-all)                           ;; user -> hide-all
             (fold-backup) (outline-hide-sublevels 1) (message-state))
            ((= 'hide-all) (-> 'show-all)                           ;; hide-all -> show-all
             (outline-show-all) (message-state))
            ((= 'show-all) (-> 'user)                               ;; show-all -> user
             (fold-restore) (message-state))
            (t (error (format "Invalid current folding state: %S" my-org-global-fold-cycle-state))))))


  (defun my-org-global-fold-set (target-state)
    (if (memq target-state '(user hide-all show-all))
        (while (not (eq my-org-global-fold-cycle-state target-state))
          (my-org-global-fold-cycle))
      (error "Invalid target-state: %S" target-state)))


  ;; from org-fold.el
  (defun my-org-global-fold-cycle-folding-store ()
    "Store folding states of org-mode to file for current buffer to `my-org-global-fold-cycle-folding-states'"
    (save-excursion
      (goto-char (point-min))
      (let (foldstates)
        (unless (looking-at outline-regexp)
          (outline-next-visible-heading 1))
        (while (not (eobp))
          (push (if (some (lambda (o) (overlay-get o 'invisible))
                          (overlays-at (line-end-position)))
                    t)
                foldstates)
          (outline-next-visible-heading 1))
        (setq my-org-global-fold-cycle-folding-states (nreverse foldstates)))))

  (defun my-org-global-fold-cycle-folding-restore ()
    "Restore folding states of org-mode from file for current buffer"
    (save-excursion
      (goto-char (point-min))
      (let ((foldstates my-org-global-fold-cycle-folding-states))
        (when foldstates
          (show-all)
          (goto-char (point-min))
          (unless (looking-at outline-regexp)
            (outline-next-visible-heading 1))
          (while (and foldstates (not (eobp)))
            (if (pop foldstates)
                (hide-subtree))
            (outline-next-visible-heading 1))))))

  (defun my-org-fold-get-fold-info-file-name ()
    (concat (buffer-file-name) ".fold"))

  (defun my-org-fold-save-to-file ()
    "Save list of folding states about current buffer to fold file."
    (let ((foldstates my-org-global-fold-cycle-folding-states))
      (with-temp-file (my-org-fold-get-fold-info-file-name)
        (prin1 foldstates (current-buffer)))))

  (defun my-org-fold-load-from-file ()
    "Return list of folding states about current buffer from fold file."
    (let ((foldfile (my-org-fold-get-fold-info-file-name)))
      (if (file-readable-p foldfile)
          (with-temp-buffer
            (insert-file-contents foldfile)
            (read (current-buffer)))
        (error (format "Can not load fold file: %s" foldfile)))))

  (add-hook 'org-mode-hook 'org-fold-activate)

  (defun org-fold-activate ()
    (defvar-local my-org-global-fold-cycle-state 'user "cycle state of current org buffer")
    (defvar-local my-org-global-fold-cycle-folding-states nil "A list of user folding states of current org buffer")
    (setq-local my-org-global-fold-cycle-folding-states (my-org-fold-load-from-file))

    (my-org-global-fold-cycle-folding-restore)
    (add-hook 'kill-buffer-hook 'org-fold-kill-buffer nil t)
    (add-hook 'kill-emacs-hook  'org-fold-kill-emacs))

  (defun org-fold-kill-buffer ()
    (my-org-fold-save-to-file))

  (defun org-fold-kill-emacs ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'org-mode)
          (my-org-fold-save-to-file)))))

  ;; ----------
  (defun my-org-title-line-p (re)
    (save-excursion (goto-char (line-beginning-position))
                    (re-search-forward re (line-end-position) t)))

  (defun my-org-cycle ()
    (interactive)
    (cond ((my-org-title-line-p "^*+ \\[.\\] ")     ;; todo line?
           (my-org-cycle-todo-forward))
          ((my-org-title-line-p "^*+ ")             ;; title line?
           (my-org-cycle-fold-title)
           (when (eq my-org-global-fold-cycle-state 'user)
             (my-org-global-fold-cycle-folding-store)))
          (t nil)))

  (defun my-org-cycle-fold-title ()
    (if (outline-invisible-p (line-end-position))
        (outline-show-subtree)
      (outline-hide-subtree)))

  (defun my-org-cycle-todo-forward ()
    (interactive)
    (my-org-cycle-todo-1 nil))
  (defun my-org-cycle-todo-backward ()
    (interactive)
    (my-org-cycle-todo-1 t))

  (defun my-org-cycle-todo-1 (reverse)
    (save-excursion
      (goto-char (line-beginning-position))
      (when (re-search-forward "\\(^*+ \\[\\)\\(.\\)\\(\\] \\)" (line-end-position) t)
        (let ((kw (match-string 2)))
          (let ((rpl (if reverse
                         (cond ((string= kw "X") "!")
                               ((string= kw "!") " ")
                               (t nil))
                       (cond ((string= kw " ") "!")
                             ((string= kw "!") "X")
                             (t nil)))))
            (when rpl
              (replace-match (concat (match-string 1) rpl (match-string 3)))
              (cond ((string= rpl "X") (my-org-todo-date-insert))
                    (t                 (my-org-todo-date-remove)))
              (org-update-parent-todo-statistics)))))))

  ;; ----------
  (defun my-org-todo-date-insert ()
    (let ((pt (point)))
      (save-excursion
        (goto-char (line-beginning-position))
        (when (re-search-forward "\\(^*+ \\[X\\]\\) " (line-end-position) t)
          (replace-match (concat "\\1" (format-time-string "  %Y-%m-%d  ")))))
      (goto-char pt)))

  (defun my-org-todo-date-remove ()
    (let ((pt (point)))
      (save-excursion
        (goto-char (line-beginning-position))
        (when (re-search-forward "\\(^*+ \\[.\\]\\)  [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} "
                                 (line-end-position) t)
          (replace-match "\\1")))
      (goto-char pt)))

  ;; ----------
  (evil-define-command my-org-evil-normal-do-demote () "" (org-demote))
  (evil-define-command my-org-evil-normal-do-promote () "" (org-promote))

  (evil-define-operator my-org-evil-visual-do-demote (beg end) "" :type 'line
    (interactive "<r>")
    (org-map-region #'org-demote beg end)
    (org-fix-position-after-promote))
  (evil-define-operator my-org-evil-visual-do-promote (beg end) "" :type 'line
    (interactive "<r>")
    (org-map-region #'org-promote beg end)
    (org-fix-position-after-promote))

  ;; ----------
  (defun my-org-get-links-in-line (&optional beg)
    (interactive)
    (let ((links '())
          (eol (line-end-position)))
      (save-excursion
        (when beg (goto-char beg))
        (while (< (goto-char (next-single-property-change (point) 'htmlize-link nil eol)) eol)
          (let ((lk (get-text-property (point) 'htmlize-link)))
            (when lk
              (setq links (cons (second lk) links))))))
      links))

  (defvar my-org-todo-publish-cemetery-accept-titles '("目安箱" "Emacs" "次のKeyboard" "ウェブ投票システムをつくる"))
  (defvar my-org-todo-publish-cemetery-reason-default-list '(
    "やる気ないので"　"やる気ねえけん" "やる気なかけん" "やる気ねえかぃ" "やっ気なかで" "やる気ないけん" "やる気ないき"
     "やる気あらへんさかいに" "やる気にゃーで" "やる気ねえすけ" "やる気ねぁがら" "やる気ねはんで" "やる気ねーんくとぅ"
     "ダルいので" "ダルさんくとぅ" "ダルぇはんで" "ダルぇがら" "ダリぃけん" "ダリで" "ダルいけぇ" "ダルいき"
     "しんどいさかいに" "ダやいがで" "ダルいで" "ダリぃすけ" "ダルぇがら" "ダルぇはんで" "ダルさんくとぅ"
     "すみません。急いでおりますので。")
    "thx to https://www.8toch.net/translate/")
  (defvar my-org-todo-publish-cemetery-hugo-dir "~/git-clone/cemetery")
  (defvar my-org-todo-publish-cemetery-front-matter-fmt
"#+TITLE: %s
#+DATE: %s
#+DRAFT: false
#+TAGS[]: %s
")

  (defun my-org-todo-publish-cemetery-or-move-to-never-do-get-reason (prompt)
    "Return string as reason from user input.
If the input is empty, the return value is randomly determined."
    (let ((s (read-string prompt))
          (tr-re "[ \t\n\r　]+"))
      (cond ((string-empty-p s)
             (let ((n (length my-org-todo-publish-cemetery-reason-default-list)))
               (nth (random n) my-org-todo-publish-cemetery-reason-default-list)))
            (t (string-trim s tr-re tr-re)))))

  (defun my-org-todo-publish-cemetery-git-push (path)
    "Execute git commands add, commit then push in order to deploy
new post to netlify/hugo. Commands add and push run synchronously,
but command push takes more time so that runs asynchronously."
    (let* ((process-connection-type nil)
           (default-directory (path-join my-org-todo-publish-cemetery-hugo-dir))
           (path (file-relative-name path default-directory)))
      (condition-case err
          (progn
            (unless (= (call-process "git" nil nil nil "add" path) 0)
              (error "Error at 'git add'"))
            (unless (= (call-process "git" nil nil nil "commit" "-m" "add post") 0)
              (error "Error at 'git commit'"))
            (start-process "" nil "git" "push"))
        (error (error-message-string err)))))

  (defun my-org-todo-publish-cemetery-or-move-to-never-do ()
    "Publish the current todo line to cemetery or move to 'never-do' list,
according to `my-org-todo-publish-cemetery-accept-titles'."
    (interactive)
    (cl-flet ((ask-reason 'my-org-todo-publish-cemetery-or-move-to-never-do-get-reason)
              (kill-current-line () (let ((pt (point))) (my-org-kill-whole-line pt) (goto-char pt))))
      (let ((title (my-org-todo-get-title)))
        (cond ((and (my-org-title-line-p "^*+ \\[ \\] ")
                    (member title my-org-todo-publish-cemetery-accept-titles))
               (my-org-todo-publish-cemetery (ask-reason "墓場 <- ") title)
               (kill-current-line)
               (message "Published to TODO墓場"))
              ((my-org-title-line-p "^*+ \\[ \\] ")
               (my-org-move-to-never-do (ask-reason "やらないことリスト <- ") title)
               (kill-current-line)
               (message "Moved to %s" my-org-move-to-never-do-dest-title))
              ((my-org-title-line-p "^*+ \\[.\\] ") (message "This todo item has any status."))
              (t nil)))))

  (defun my-org-todo-publish-cemetery (reason tag)
    (let ((content (string-trim (save-excursion
                                  (goto-char (line-beginning-position))
                                  (buffer-substring-no-properties
                                   (re-search-forward "^*+ \\[ \\] +" (line-end-position) t)
                                   (next-single-property-change (point) 'htmlize-link nil (line-end-position))))))
          (links (my-org-get-links-in-line (line-beginning-position)))
          (path (path-join my-org-todo-publish-cemetery-hugo-dir "content/post"
                           (format-time-string "%Y%m%d-%H%m%S.org"))))
      (with-temp-buffer
        (insert (format my-org-todo-publish-cemetery-front-matter-fmt
                        content
                        (format-time-string "%Y-%m-%dT%H:%m:%S+09:00")
                        tag)
                "* やらなかった理由\n"
                reason "\n")
        (when links
          (insert "* Link\n")
          (mapc #'(lambda (x) (insert (format "- %s\n" x))) links))
        (write-file path))
      (my-org-todo-publish-cemetery-git-push path)))

  ;; ----------

  ;; ----------
  (set-face-attribute 'org-link nil :foreground (face-foreground 'default) :underline t)

  ;; ----------
  (define-key evil-normal-state-map (kbd "t d") #'my-org-capture-add-todo)
  (define-key evil-normal-state-map (kbd "t m") #'my-org-capture-add-memo)
  (define-key evil-normal-state-map (kbd "t t") #'my-org-notes-open)          ; toggle org buffer
  (evil-define-key 'normal org-mode-map (kbd "t t") #'my-org-notes-close)     ; toggle org buffer
  (evil-define-key 'normal org-mode-map (kbd "t d") #'my-org-capture-add-todo)
  (evil-define-key 'normal org-mode-map (kbd "t m") #'my-org-capture-add-memo)
  (evil-define-key 'normal org-mode-map (kbd "<tab>")   #'my-org-evil-normal-do-demote)
  (evil-define-key 'normal org-mode-map (kbd "S-<tab>") #'my-org-evil-normal-do-promote)
  (evil-define-key 'normal org-mode-map (kbd "SPC")   #'my-org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "S-SPC") #'my-org-cycle-todo-backward)
  (evil-define-key 'normal org-mode-map (kbd "M-SPC") #'my-org-global-fold-cycle)
  (evil-define-key 'normal org-mode-map (kbd "C-j") #'org-metadown)
  (evil-define-key 'normal org-mode-map (kbd "C-k") #'org-metaup)
  (evil-define-key 'normal org-mode-map (kbd "O") #'my-org-dup-heading-up)
  (evil-define-key 'normal org-mode-map (kbd "o") #'my-org-dup-heading-down)
  (evil-define-key 'normal org-mode-map (kbd "RET") #'my-org-dup-heading-down)
  (evil-define-key 'normal org-mode-map (kbd "<M-down>") #'my-org-todo-goto-working-forward)
  (evil-define-key 'normal org-mode-map (kbd "<M-up>")   #'my-org-todo-goto-working-backward)
  (evil-define-key 'normal org-mode-map (kbd "<S-left>")  #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<S-right>") #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<S-down>")  #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<S-up>")    #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<C-up>")    #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<C-down>")  #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<C-right>") #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<C-left>")  #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<down>") #'my-org-goto-title-next)
  (evil-define-key 'normal org-mode-map (kbd "<up>")   #'my-org-goto-title-prev)
  (evil-define-key 'normal org-mode-map (kbd "t 0") #'my-org-todo-publish-cemetery-or-move-to-never-do)
  (evil-define-key 'normal org-mode-map (kbd "0")   #'my-org-beginning-of-content)

  (evil-define-key 'insert org-mode-map (kbd "C-a") #'my-org-beginning-of-content)

  (evil-define-key 'visual org-mode-map (kbd "<tab>")   #'my-org-evil-visual-do-demote)
  (evil-define-key 'visual org-mode-map (kbd "S-<tab>") #'my-org-evil-visual-do-promote)
  ;; (evil-define-key 'normal org-mode-map (kbd "M-c") #'my-org-meta-ret)          ; M-RET

  (add-hook 'org-mode-hook #'(lambda ()
          (org-defkey org-mode-map [(meta up)] nil)        ; unmap for tabbar
          (org-defkey org-mode-map [(meta down)] nil)))    ; unmap for tabbar
  )

;; ----------------------------------------------------------------------
(use-package org-fold
  :disabled t
  :if window-system
  :defer t
  ;; :load-path "~/.emacs.d/elisp"
  :config
  ;; re-defined
  (defun org-fold-activate ()
    ;; (org-fold-restore)
    (unless (file-exists-p (buffer-file-name))
      (org-fold-restore))
    (add-hook 'kill-buffer-hook 'org-fold-kill-buffer nil t)
    (add-hook 'kill-emacs-hook  'org-fold-kill-emacs))
  )

;; ----------------------------------------------------------------------
(use-package org-bullets
  :disabled t
  :if window-system
  :after org
  :config
  (setq org-bullets-bullet-list '("❖" "☯" "✪" "✿" "✜" "⬟" "⬢" "⬣"))
  (set-face-attribute 'org-level-1 nil :height 1.2)
  (add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1))))

;; ----------------------------------------------------------------------
(use-package org-tree-slide
  :disabled
  :if window-system
  ;; :defer t
  :bind (:map org-mode-map
         ([f5] . org-tree-slide-on)
         :map org-tree-slide-mode-map
         ([next]  . org-tree-slide-move-next-tree)        ;; page down
         ([prior] . org-tree-slide-move-previous-tree)    ;; page up
         ([f5] . org-tree-slide-off))

  :config
  (defun presen-override-key-bindings ()
    (evil-make-overriding-map org-tree-slide-mode-map 'normal)
    (evil-add-hjkl-bindings org-tree-slide-mode-map 'normal
      [right] 'org-tree-slide-move-next-tree
      [down]  'org-tree-slide-move-next-tree
      [left]  'org-tree-slide-move-previous-tree
      [up]    'org-tree-slide-move-previous-tree)
    (evil-normal-state))    ;; dummy

  (add-hook 'org-tree-slide-play-hook #'presen-override-key-bindings)

  (setq org-tree-slide-indicator '(:next "" :previous "" :content ""))
  (defun org-tree-slide-on  () (interactive) (org-tree-slide-mode 1))
  (defun org-tree-slide-off () (interactive) (org-tree-slide-mode 0))

  (lexical-let ((face-default nil)
                (face-fringe nil)
                (face-minibuf nil)
                (face-link nil)
                (face-level-1 nil)
                (frame-height 36)
                (bg-color "#fefae0")
                (margin (window-margins)))
    (defun presen-enter ()
      (set-frame-height nil frame-height)
      (beacon-mode 0)
      (centaur-tabs-mode -1)
      (scroll-bar-mode 0)
      (set-fringe-mode 0)
      (set-window-margins (selected-window) 4)
      (setq-local evil-normal-state-cursor '(bar . 1))
      (hide-mode-line-mode 1)
      (face-remap-add-relative 'org-tree-slide-header-overlay-face
                                     :foreground "#283618" :background bg-color :height 0.5)
      (setq face-default (face-remap-add-relative 'default :background bg-color
                              :foreground "grey13" :height 2.0 :family "Hiragino Maru Gothic Pro"))
      (setq face-fringe  (face-remap-add-relative 'fringe  :background bg-color))
      (setq face-minibuf (face-remap-add-relative 'minibuffer-prompt :background bg-color))
      (setq face-link  (face-remap-add-relative 'org-link  :foreground "#606c38"))
      (setq face-level-1 (face-remap-add-relative 'outline-1 :foreground "#99581E" :height 1.5 :weight 'bold))
      (setq org-tree-slide-header nil)
      (setq org-tree-slide-slide-in-effect nil)
      (setq org-tree-slide-exit-at-next-last-slide t)
      (org-display-inline-images))

    (defun presen-exit ()
      (face-remap-remove-relative face-default)
      (face-remap-remove-relative face-minibuf)
      (face-remap-remove-relative face-fringe)
      (face-remap-remove-relative face-link)
      (face-remap-remove-relative face-level-1)
      (set-frame-height nil 100)
      (beacon-mode 1)
      (centaur-tabs-mode +1)
      ;; (scroll-bar-mode 1)
      (set-fringe-mode nil)
      (set-window-margins (selected-window) (car margin) (cdr margin))
      (setq-local evil-normal-state-cursor 'box)
      (minibuffer-timer-stop)
      (hide-mode-line-mode 0)
      (my-org-global-fold-set 'hide-all)))

  (lexical-let ((first-paging t))
    (defun presen-timer-reset ()
      (setq first-paging t))

    (defun presen-timer-start ()
      (when first-paging
          (minibuffer-timer-start-force 5)
          (setq first-paging nil)))

    (defun presen-timer-stop ()
      (minibuffer-timer-stop)
      (setq first-paging t)))

    (defun sayonara ()
      (setq buffer-read-only nil)
      (lexical-let ((animate-n-steps 60)
                    (v 8)
                    (h 43))
        (animate-string "おしまい！" v h))
      (sit-for 2)
      (undo))

  (add-hook 'org-tree-slide-before-exit-hook #'sayonara)
  (add-hook 'org-tree-slide-play-hook #'presen-enter)
  (add-hook 'org-tree-slide-stop-hook #'presen-exit)

  (add-hook 'org-tree-slide-play-hook #'presen-timer-reset)
  (add-hook 'org-tree-slide-before-move-next-hook #'presen-timer-start)
  (add-hook 'org-tree-slide-stop-hook #'presen-timer-stop)
  )


;;;
;;;
;;;

(provide 'my-org-ex)
;;; my-org-ex.el ends here
