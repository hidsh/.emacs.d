(defvar theme-loader-theme nil
  "Choose the theme you want to load.")

(defun theme-loader--apply (frame)
  "フレームが作成されたときにテーマを適用する"
  (select-frame frame)
  ;; 使用したいテーマ名に変えてください（例: 'doom-one）
  (load-theme theme-loader-theme t))

;; デーモン起動（emacsclient用）と通常起動の両方に対応させる
(if (daemonp)
    (add-hook 'after-make-frame-functions #'theme-loader--apply)
  (load-theme theme-loader-theme t))

(provide 'theme-loader)
;; theme-loader.el ends here
